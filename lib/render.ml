(** Pure screen renderer: model + terminal size -> a frame of styled rows.

    Layout invariant: the input area is on top (one row, or up to a third of
    the screen in multiline mode), the last row is the status bar,
    everything between is the scrollable output viewport. Every frame has
    exactly [h] rows and every row exactly [w] columns (counting Unicode
    characters as one column each), so the layout can never shift. *)

type ansi_color = Idx of int  (** 0..255 *) | Rgb of int * int * int

(** SGR attributes carried by a piece of command output (with [--ansi]). *)
type ansi_attrs = {
  fg : ansi_color option;
  bg : ansi_color option;
  bold : bool;
  dim : bool;
  italic : bool;
  underline : bool;
  reverse : bool;
}

let ansi_default =
  {
    fg = None;
    bg = None;
    bold = false;
    dim = false;
    italic = false;
    underline = false;
    reverse = false;
  }

type style =
  | Prompt
  | Input
  | Out_text
  | Err_text
  | Info_text
  | Bar
  | Bar_alert
  | Bar_mode
  | Ansi of ansi_attrs
type seg = style * string

type frame = {
  rows : seg list list;
  cursor : (int * int) option;  (** column, row *)
}

(* --- Unicode-character-counted string helpers --- *)

let fold_uchars f acc s =
  let rec go i acc =
    if i >= String.length s then acc
    else
      let d = String.get_utf_8_uchar s i in
      let u =
        if Uchar.utf_decode_is_valid d then Uchar.utf_decode_uchar d
        else Uchar.rep
      in
      go (i + Uchar.utf_decode_length d) (f acc u)
  in
  go 0 acc

let ulength s = fold_uchars (fun n _ -> n + 1) 0 s

(** Substring by character index: [len] characters starting at [start]. *)
let usub s start len =
  let b = Buffer.create (String.length s) in
  let _ =
    fold_uchars
      (fun i u ->
        if i >= start && i < start + len then Buffer.add_utf_8_uchar b u;
        i + 1)
      0 s
  in
  Buffer.contents b

let is_control u =
  let c = Uchar.to_int u in
  c < 0x20 || (c >= 0x7f && c <= 0x9f)

(** Replace control characters one-for-one, preserving the character count
    (used for the input line, where columns must map 1:1 to cursor
    positions). *)
let sanitize_flat s =
  let b = Buffer.create (String.length s) in
  let _ =
    fold_uchars
      (fun () u ->
        if is_control u then Buffer.add_char b '?'
        else Buffer.add_utf_8_uchar b u)
      () s
  in
  Buffer.contents b

(** Sanitize an output line: expand tabs to 8-column stops, replace other
    control characters. *)
let sanitize_line s =
  let b = Buffer.create (String.length s) in
  let _ =
    fold_uchars
      (fun col u ->
        if Uchar.to_int u = 0x09 then begin
          let next = ((col / 8) + 1) * 8 in
          for _ = col to next - 1 do
            Buffer.add_char b ' '
          done;
          next
        end
        else if is_control u then begin
          Buffer.add_char b '?';
          col + 1
        end
        else begin
          Buffer.add_utf_8_uchar b u;
          col + 1
        end)
      0 s
  in
  Buffer.contents b

(** Crop or pad [s] to exactly [w] characters. *)
let fit w s =
  let n = ulength s in
  if n = w then s
  else if n > w then usub s 0 w
  else s ^ String.make (w - n) ' '

(* --- ANSI SGR parsing (--ansi) --- *)

(** Apply the SGR parameter string [body] (the bytes between "ESC[" and the
    final "m") to [attrs]. Unknown parameters are ignored; a malformed
    extended-color introducer swallows the rest of the sequence. *)
let apply_sgr attrs body =
  let params =
    String.split_on_char ';' body
    |> List.map (fun f -> if f = "" then Some 0 else int_of_string_opt f)
  in
  let rec go a = function
    | [] -> a
    | None :: rest -> go a rest
    | Some p :: rest -> (
        match p with
        | 0 -> go ansi_default rest
        | 1 -> go { a with bold = true } rest
        | 2 -> go { a with dim = true } rest
        | 3 -> go { a with italic = true } rest
        | 4 -> go { a with underline = true } rest
        | 7 -> go { a with reverse = true } rest
        | 22 -> go { a with bold = false; dim = false } rest
        | 23 -> go { a with italic = false } rest
        | 24 -> go { a with underline = false } rest
        | 27 -> go { a with reverse = false } rest
        | 39 -> go { a with fg = None } rest
        | 49 -> go { a with bg = None } rest
        | n when n >= 30 && n <= 37 -> go { a with fg = Some (Idx (n - 30)) } rest
        | n when n >= 40 && n <= 47 -> go { a with bg = Some (Idx (n - 40)) } rest
        | n when n >= 90 && n <= 97 ->
            go { a with fg = Some (Idx (n - 90 + 8)) } rest
        | n when n >= 100 && n <= 107 ->
            go { a with bg = Some (Idx (n - 100 + 8)) } rest
        | (38 | 48) as which -> (
            let set a c =
              if which = 38 then { a with fg = Some c } else { a with bg = Some c }
            in
            match rest with
            | Some 5 :: Some n :: rest when n >= 0 && n <= 255 ->
                go (set a (Idx n)) rest
            | Some 2 :: Some r :: Some g :: Some b :: rest
              when r >= 0 && r <= 255 && g >= 0 && g <= 255 && b >= 0 && b <= 255
              ->
                go (set a (Rgb (r, g, b))) rest
            | _ -> a)
        | _ -> go a rest)
  in
  go attrs params

(** Split a raw output line into [(attrs, text)] segments: SGR sequences set
    the attributes (carried across segments, default at the start of each
    line), every other escape sequence is stripped (CSI with any final, OSC
    up to BEL or ESC-backslash, 2-character escapes), tabs expand to
    8-column stops and remaining control characters become '?'. *)
let ansi_segments s =
  let n = String.length s in
  let segs = ref [] in
  let buf = Buffer.create 64 in
  let attrs = ref ansi_default in
  let flush () =
    if Buffer.length buf > 0 then begin
      segs := (!attrs, Buffer.contents buf) :: !segs;
      Buffer.clear buf
    end
  in
  let col = ref 0 in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = '\x1b' then begin
      if !i + 1 >= n then i := n (* truncated escape: drop *)
      else
        match s.[!i + 1] with
        | '[' ->
            (* CSI: parameter/intermediate bytes up to a final 0x40-0x7e *)
            let j = ref (!i + 2) in
            while !j < n && not (s.[!j] >= '\x40' && s.[!j] <= '\x7e') do
              incr j
            done;
            if !j >= n then i := n
            else begin
              if s.[!j] = 'm' then begin
                let body = String.sub s (!i + 2) (!j - !i - 2) in
                let a = apply_sgr !attrs body in
                if a <> !attrs then begin
                  flush ();
                  attrs := a
                end
              end;
              i := !j + 1
            end
        | ']' ->
            (* OSC: skip until BEL or ESC-backslash *)
            let j = ref (!i + 2) in
            let fin = ref (-1) in
            while !fin < 0 && !j < n do
              if s.[!j] = '\x07' then fin := !j + 1
              else if s.[!j] = '\x1b' && !j + 1 < n && s.[!j + 1] = '\\' then
                fin := !j + 2
              else incr j
            done;
            i := (if !fin >= 0 then !fin else n)
        | _ -> i := !i + 2 (* 2-character escape *)
    end
    else if c = '\t' then begin
      let next = ((!col / 8) + 1) * 8 in
      for _ = !col to next - 1 do
        Buffer.add_char buf ' '
      done;
      col := next;
      incr i
    end
    else begin
      let d = String.get_utf_8_uchar s !i in
      let u =
        if Uchar.utf_decode_is_valid d then Uchar.utf_decode_uchar d
        else Uchar.rep
      in
      if is_control u then Buffer.add_char buf '?' else Buffer.add_utf_8_uchar buf u;
      incr col;
      i := !i + Uchar.utf_decode_length d
    end
  done;
  flush ();
  List.rev !segs

(** Crop or pad a segment list to exactly [w] characters; padding gets the
    [pad] style. *)
let fit_segs ~pad w segs =
  let rec crop budget = function
    | [] -> ([], budget)
    | _ when budget = 0 -> ([], 0)
    | (st, s) :: rest ->
        let n = ulength s in
        if n <= budget then
          let tl, rem = crop (budget - n) rest in
          ((st, s) :: tl, rem)
        else ([ (st, usub s 0 budget) ], 0)
  in
  let segs, rem = crop w segs in
  if rem > 0 then segs @ [ (pad, String.make rem ' ') ] else segs

let style_of_kind = function
  | Model.Out -> Out_text
  | Model.Err -> Err_text
  | Model.Info -> Info_text

(* Flatten styled [parts] to per-character styles, take [w] columns starting
   at [hscroll], pad with Input spaces, regroup into segments. *)
let window_parts ~w ~hscroll parts =
  let chars =
    List.concat_map
      (fun (style, s) ->
        List.rev (fold_uchars (fun acc u -> (style, u) :: acc) [] s))
      parts
  in
  let total = List.length chars in
  let visible =
    List.filteri (fun i _ -> i >= hscroll && i < hscroll + w) chars
  in
  let visible =
    visible
    @ List.init (w - min w (max 0 (total - hscroll))) (fun _ ->
          (Input, Uchar.of_char ' '))
  in
  List.fold_left
    (fun acc (style, u) ->
      match acc with
      | (st, b) :: rest when st == style ->
          Buffer.add_utf_8_uchar b u;
          (st, b) :: rest
      | _ ->
          let b = Buffer.create 16 in
          Buffer.add_utf_8_uchar b u;
          (style, b) :: acc)
    [] visible
  |> List.rev_map (fun (st, b) -> (st, Buffer.contents b))

(* The args editor as display lines: one per '\n' in multiline mode, the
   whole (control-sanitized) text as a single line otherwise. *)
let args_lines (m : Model.t) =
  if m.multiline then
    String.split_on_char '\n' (Editor.to_string m.editor)
    |> List.map sanitize_flat
  else [ sanitize_flat (Editor.to_string m.editor) ]

(** Rows taken by the input area: the number of args lines, clamped to a
    third of the screen (and always at least one), so at least one output
    row and the status bar survive. *)
let input_height ~h (m : Model.t) =
  if not m.multiline then 1
  else min (List.length (args_lines m)) (max 1 (h / 3))

(* (line, column) of character position [cur] within [lines] (lines are
   separated by one '\n' character each). *)
let cursor_line_col lines cur =
  let rec go y cur = function
    | [] -> (y, cur)
    | l :: rest ->
        let n = ulength l in
        if cur <= n || rest = [] then (y, min cur n)
        else go (y + 1) (cur - n - 1) rest
  in
  go 0 cur lines

(* The input area is composed of styled regions:
     CMD [SP fixed-args] "> " args-line-0
     "  " args-line-1
     ...
   The fixed-args region is editable too (Tab moves the cursor there) and
   lives on row 0 only. The row holding the cursor scrolls horizontally to
   keep it visible; in multiline mode the area is clamped to [input_height]
   rows and scrolls vertically to keep the cursor row visible. *)
let input_area ~w ~h (m : Model.t) =
  let fixed_text = sanitize_flat (Model.fixed_text m) in
  let prompt_parts, args_off, fixed_cursor =
    match m.cmd with
    | None -> ([ (Prompt, "> ") ], 2, 0)
    | Some c ->
        let c = sanitize_flat c in
        let clen = ulength c in
        (* show the fixed-args slot when it has content or holds the
           cursor *)
        let fixed_shown = fixed_text <> "" || m.focus = Model.F_fixed in
        let fixed_parts =
          if fixed_shown then [ (Prompt, " "); (Input, fixed_text) ] else []
        in
        let args_off =
          clen + (if fixed_shown then 1 + ulength fixed_text else 0) + 2
        in
        ( ((Prompt, c) :: fixed_parts) @ [ (Prompt, "> ") ],
          args_off,
          clen + 1 + Editor.cursor m.fixed_editor )
  in
  let lines = args_lines m in
  let nlines = List.length lines in
  let input_h = input_height ~h m in
  let cy, ccol = cursor_line_col lines (Editor.cursor m.editor) in
  let vscroll =
    if m.focus = Model.F_fixed then 0
    else max 0 (min (nlines - input_h) (cy - input_h + 1))
  in
  (* absolute (row, column) of the cursor before any scrolling *)
  let cursor_row, cursor_abs =
    match m.focus with
    | Model.F_fixed -> (0, fixed_cursor)
    | Model.F_args | Model.F_output ->
        (cy, (if cy = 0 then args_off else 2) + ccol)
  in
  let rows =
    List.init input_h (fun i ->
        let li = vscroll + i in
        let line = List.nth lines li in
        let parts =
          if li = 0 then prompt_parts @ [ (Input, line) ]
          else [ (Prompt, "  "); (Input, line) ]
        in
        let hscroll =
          if li = cursor_row && cursor_abs >= w then cursor_abs - w + 1 else 0
        in
        window_parts ~w ~hscroll parts)
  in
  let hscroll = if cursor_abs < w then 0 else cursor_abs - w + 1 in
  let cx = min (cursor_abs - hscroll) (max 0 (w - 1)) in
  let cyv = max 0 (min (input_h - 1) (cursor_row - vscroll)) in
  (rows, (cx, cyv))

let status_row ~w ~view_h (m : Model.t) =
  let state =
    if m.running then " running… "
    else
      match m.status with
      | None -> " "
      | Some (Model.Exited n) -> Printf.sprintf " exit %d " n
      | Some (Model.Signaled n) -> Printf.sprintf " signal %d " n
  in
  let state_alert =
    (not m.running)
    && match m.status with Some (Model.Exited 0) -> false | None -> false | Some _ -> true
  in
  let parse =
    match m.parse_error with
    | None -> ""
    | Some e -> Printf.sprintf " %s " (Shellwords.error_message e)
  in
  let count = Array.length m.lines in
  let scroll = Model.clamp_scroll ~view_h m m.scroll in
  let range =
    if count = 0 then ""
    else Printf.sprintf "%d-%d/%d  " (scroll + 1) (min count (scroll + view_h)) count
  in
  let vim_mode =
    if not m.vim then ""
    else match m.vmode with Model.V_normal -> " NORMAL " | Model.V_insert -> " INSERT "
  in
  let mode = if m.single then " [1 arg] " else "" in
  let ansi_ind = if m.ansi then " [ansi] " else "" in
  let focus_ind =
    match m.focus with
    | Model.F_fixed -> " [fixed] "
    | Model.F_output -> " [output] "
    | Model.F_args -> ""
  in
  let hints = "enter run · ^D accept · esc quit " in
  let leftw =
    ulength vim_mode + ulength state + ulength parse + ulength mode
    + ulength ansi_ind + ulength focus_ind
  in
  (* Right-hand side: drop the hints, then the range, as space runs out. *)
  let right =
    List.find_opt
      (fun r -> leftw + ulength r <= w)
      [ range ^ hints; range; "" ]
    |> Option.value ~default:""
  in
  let mid = w - leftw - ulength right in
  let segs =
    if mid >= 0 then
      [
        (Bar_mode, vim_mode);
        ((if state_alert then Bar_alert else Bar), state);
        (Bar_alert, parse);
        (Bar, mode);
        (Bar, ansi_ind);
        (Bar, focus_ind);
        (Bar, String.make mid ' ' ^ right);
      ]
    else
      (* Too narrow even for the left part: crop it. *)
      [
        (Bar_mode, fit (min w (ulength vim_mode)) vim_mode);
        ( (if state_alert then Bar_alert else Bar),
          fit (max 0 (w - ulength vim_mode)) state );
      ]
  in
  (* Normalize: drop empty segments, enforce exact width. *)
  let segs = List.filter (fun (_, s) -> s <> "") segs in
  let total = List.fold_left (fun n (_, s) -> n + ulength s) 0 segs in
  if total = w then segs
  else if total < w then segs @ [ (Bar, String.make (w - total) ' ') ]
  else
    (* crop from the right *)
    let rec crop acc budget = function
      | [] -> List.rev acc
      | (st, s) :: rest ->
          let n = ulength s in
          if n <= budget then crop ((st, s) :: acc) (budget - n) rest
          else List.rev ((st, usub s 0 budget) :: acc)
    in
    crop [] w segs

(** One output line as a row of exactly [w] characters. With [--ansi] the
    line is split on its SGR sequences; segments with all-default attributes
    keep the line kind's style (stderr stays red). *)
let output_line ~ansi ~w (line : Model.line) =
  let base = style_of_kind line.kind in
  if not ansi then [ (base, fit w (sanitize_line line.text)) ]
  else
    ansi_segments line.text
    |> List.map (fun (a, s) -> ((if a = ansi_default then base else Ansi a), s))
    |> fit_segs ~pad:base w

let output_rows ~w ~view_h (m : Model.t) =
  let count = Array.length m.lines in
  let scroll = Model.clamp_scroll ~view_h m m.scroll in
  List.init view_h (fun i ->
      let idx = scroll + i in
      if idx < count then output_line ~ansi:m.ansi ~w m.lines.(idx)
      else [ (Out_text, String.make w ' ') ])

let render ~w ~h (m : Model.t) =
  if w <= 0 || h <= 0 then { rows = []; cursor = None }
  else
    let input, cursor = input_area ~w ~h m in
    (* with the output focused nothing can be typed: hide the cursor *)
    let cursor = if m.focus = Model.F_output then None else Some cursor in
    let input_h = List.length input in
    let view_h = h - input_h - 1 in
    if view_h < 0 then { rows = input; cursor }
    else
      let rows =
        input @ output_rows ~w ~view_h m @ [ status_row ~w ~view_h m ]
      in
      { rows; cursor }

(* --- test helpers --- *)

let row_text segs = String.concat "" (List.map snd segs)
let to_strings frame = List.map row_text frame.rows
