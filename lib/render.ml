(** Pure screen renderer: model + terminal size -> a frame of styled rows.

    Layout invariant: the input area is on top (one row per pipeline step,
    up to a third of the screen), the last row is the status bar,
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

(** Split styled segments into rows of exactly [w] characters (at least one
    row); the last row is padded with the [pad] style. *)
let wrap_segs ~pad w segs =
  let rec take_row budget acc segs =
    match segs with
    | [] -> (List.rev acc, [], budget)
    | (st, s) :: rest ->
        let n = ulength s in
        if n = 0 then take_row budget acc rest
        else if n <= budget then take_row (budget - n) ((st, s) :: acc) rest
        else
          let head = usub s 0 budget and tail = usub s budget (n - budget) in
          (List.rev ((st, head) :: acc), (st, tail) :: rest, 0)
  in
  let rec rows segs =
    let row, rest, rem = take_row w [] segs in
    let row = if rem > 0 then row @ [ (pad, String.make rem ' ') ] else row in
    if rest = [] then [ row ] else row :: rows rest
  in
  rows segs

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

(** Display rows a line occupies when wrapped ([--wrap]). *)
let line_height ~ansi ~w (line : Model.line) =
  let len =
    if not ansi then ulength (sanitize_line line.text)
    else
      List.fold_left (fun n (_, s) -> n + ulength s) 0 (ansi_segments line.text)
  in
  max 1 ((len + w - 1) / max 1 w)

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

(* An args editor as display lines: one per '\n' in multiline mode, the
   whole (control-sanitized) text as a single line otherwise. *)
let args_lines_of (m : Model.t) ed =
  if m.multiline then
    String.split_on_char '\n' (Editor.to_string ed) |> List.map sanitize_flat
  else [ sanitize_flat (Editor.to_string ed) ]


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

(* The input area is composed of styled regions, one block per pipeline
   step:
     CMD [SP fixed-args] "> " args-line-0
     "  " args-line-1
     ...
   Each step's fixed-args region is editable too (Shift-Tab moves the
   cursor there) and lives on the step's first row. The row holding the
   cursor scrolls horizontally to keep it visible — or, with
   [--wrap-input], every logical row wraps onto continuation rows. The
   whole area is clamped to [input_height] rows and scrolls vertically to
   keep the cursor row visible. *)

(* All input display rows (each exactly [w] columns) and the cursor's
   (row, column) among them. *)
let input_rows ~w (m : Model.t) =
  let cursor = ref (0, 0) in
  (* logical (row, absolute column) *)
  let rows_rev = ref [] in
  let nrows = ref 0 in
  Array.iteri
    (fun i (st : Model.step) ->
      let fixed_text = sanitize_flat (Editor.to_string st.Model.fixed) in
      (* the whole fixed command line is editable; show its slot when it has
         content or holds the cursor *)
      let fixed_shown =
        fixed_text <> "" || (i = m.Model.cur && m.focus = Model.F_fixed)
      in
      let prompt_parts, args_off, fixed_cursor =
        if not fixed_shown then ([ (Prompt, "> ") ], 2, 0)
        else
          let display = if fixed_text = "" then " " else fixed_text in
          let parts =
            (* cosmetic: the command word keeps the prompt color *)
            match String.index_opt display ' ' with
            | Some sp when fixed_text <> "" ->
                [
                  (Prompt, String.sub display 0 sp);
                  (Input, String.sub display sp (String.length display - sp));
                ]
            | Some _ -> [ (Input, display) ]
            | None -> [ (Prompt, display) ]
          in
          ( parts @ [ (Prompt, "> ") ],
            ulength display + 2,
            Editor.cursor st.Model.fixed )
      in
      let lines = args_lines_of m st.Model.args in
      if i = m.Model.cur then begin
        match m.focus with
        | Model.F_fixed -> cursor := (!nrows, fixed_cursor)
        | Model.F_args | Model.F_output ->
            let cy, ccol =
              cursor_line_col lines (Editor.cursor st.Model.args)
            in
            cursor := (!nrows + cy, (if cy = 0 then args_off else 2) + ccol)
      end;
      List.iteri
        (fun li line ->
          let parts =
            if li = 0 then prompt_parts @ [ (Input, line) ]
            else [ (Prompt, "  "); (Input, line) ]
          in
          rows_rev := parts :: !rows_rev;
          incr nrows)
        lines)
    m.Model.steps;
  let logical = Array.of_list (List.rev !rows_rev) in
  let lrow, abs = !cursor in
  if not m.Model.wrap_input then
    (* the cursor row slides horizontally to keep the cursor on screen *)
    let hscroll = if abs < w then 0 else abs - w + 1 in
    let rows =
      Array.mapi
        (fun i parts ->
          window_parts ~w ~hscroll:(if i = lrow then hscroll else 0) parts)
        logical
    in
    (rows, (lrow, min (abs - hscroll) (max 0 (w - 1))))
  else begin
    (* every logical row wraps onto as many display rows as it needs *)
    let rows = ref [] and n = ref 0 in
    let crow = ref 0 and ccol = ref 0 in
    Array.iteri
      (fun i parts ->
        let chunks = wrap_segs ~pad:Input w parts in
        let chunks =
          (* the cursor can sit just past text ending exactly at the edge:
             give it a blank row to live on *)
          if i = lrow && abs > 0 && abs mod w = 0 && abs / w >= List.length chunks
          then chunks @ [ [ (Input, String.make w ' ') ] ]
          else chunks
        in
        if i = lrow then begin
          crow := !n + (abs / w);
          ccol := abs mod w
        end;
        List.iter
          (fun r ->
            rows := r :: !rows;
            incr n)
          chunks)
      logical;
    (Array.of_list (List.rev !rows), (!crow, !ccol))
  end

(** Rows taken by the input area: the display rows of every step, clamped
    to a third of the screen (and always at least one), so at least one
    output row and the status bar survive. *)
let input_height ~w ~h (m : Model.t) =
  let total = Array.length (fst (input_rows ~w m)) in
  if total <= 1 then 1 else min total (max 1 (h / 3))

let input_area ~w ~h (m : Model.t) =
  let rows, (crow, ccol) = input_rows ~w m in
  let total = Array.length rows in
  let input_h = if total <= 1 then 1 else min total (max 1 (h / 3)) in
  let vscroll = max 0 (min (total - input_h) (crow - input_h + 1)) in
  ( List.init input_h (fun i -> rows.(vscroll + i)),
    (ccol, max 0 (min (input_h - 1) (crow - vscroll))) )

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
    else
      let last =
        if not m.wrap then min count (scroll + view_h)
        else
          (* wrapped lines take several rows: walk until the viewport is
             full, at the width the output actually gets (panes halve it) *)
          let ow =
            if Array.length m.panes = 0 || w < 20 then w else w / 2
          in
          let rec go i rows =
            if i >= count || rows >= view_h then i
            else go (i + 1) (rows + line_height ~ansi:m.ansi ~w:ow m.lines.(i))
          in
          go scroll 0
      in
      Printf.sprintf "%d-%d/%d  " (scroll + 1) last count
  in
  let vim_mode =
    if not m.vim then ""
    else match m.vmode with Model.V_normal -> " NORMAL " | Model.V_insert -> " INSERT "
  in
  let mode = if m.single then " [1 arg] " else "" in
  let ansi_ind = if m.ansi then " [ansi] " else "" in
  let wrap_ind = if m.wrap then " [wrap] " else "" in
  let wrapin_ind = if m.wrap_input then " [wrap-in] " else "" in
  let step_ind =
    let n = Model.nsteps m in
    if n > 1 then Printf.sprintf " [%d/%d] " (m.Model.cur + 1) n else ""
  in
  let focus_ind =
    match m.focus with
    | Model.F_fixed -> " [fixed] "
    | Model.F_output -> " [output] "
    | Model.F_args -> ""
  in
  let hints = "enter run · ^D accept · esc quit " in
  let leftw =
    ulength vim_mode + ulength state + ulength parse + ulength mode
    + ulength ansi_ind + ulength wrap_ind + ulength wrapin_ind
    + ulength step_ind + ulength focus_ind
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
        (Bar, wrap_ind);
        (Bar, wrapin_ind);
        (Bar, step_ind);
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

(** One output line as one or more rows of exactly [w] characters: cropped
    to a single row normally, wrapped onto several with [--wrap]. With
    [--ansi] the line is split on its SGR sequences; segments with
    all-default attributes keep the line kind's style (stderr stays red). *)
let output_line_rows ~ansi ~wrap ~w (line : Model.line) =
  let base = style_of_kind line.kind in
  if (not ansi) && not wrap then
    [ [ (base, fit w (sanitize_line line.text)) ] ]
  else
    let segs =
      if not ansi then [ (base, sanitize_line line.text) ]
      else
        ansi_segments line.text
        |> List.map (fun (a, s) ->
               ((if a = ansi_default then base else Ansi a), s))
    in
    if wrap then wrap_segs ~pad:base w segs else [ fit_segs ~pad:base w segs ]

let output_line ~ansi ~w line =
  List.hd (output_line_rows ~ansi ~wrap:false ~w line)

let output_rows ~w ~view_h (m : Model.t) =
  let count = Array.length m.lines in
  let scroll = Model.clamp_scroll ~view_h m m.scroll in
  if not m.wrap then
    List.init view_h (fun i ->
        let idx = scroll + i in
        if idx < count then output_line ~ansi:m.ansi ~w m.lines.(idx)
        else [ (Out_text, String.make w ' ') ])
  else begin
    let buf = ref [] and n = ref 0 in
    let i = ref scroll in
    while !n < view_h && !i < count do
      List.iter
        (fun row ->
          if !n < view_h then begin
            buf := row :: !buf;
            incr n
          end)
        (output_line_rows ~ansi:m.ansi ~wrap:true ~w m.lines.(!i));
      incr i
    done;
    while !n < view_h do
      buf := [ (Out_text, String.make w ' ') ] :: !buf;
      incr n
    done;
    List.rev !buf
  end

(* The lens/hint pane column: one section per pane, top to bottom, equal
   heights with the last section absorbing the remainder. Each section is a
   one-row header (the command string) over the head of its output. *)
let pane_rows ~w ~view_h (m : Model.t) =
  let n = Array.length m.panes in
  let base = view_h / n in
  List.concat
    (List.init n (fun i ->
         let p = m.panes.(i) in
         let height = if i = n - 1 then view_h - (base * (n - 1)) else base in
         if height <= 0 then []
         else
           [ (Bar, fit w (sanitize_line p.Model.spec)) ]
           :: List.init (height - 1) (fun j ->
                  if j < Array.length p.Model.plines then
                    output_line ~ansi:m.ansi ~w p.Model.plines.(j)
                  else [ (Out_text, String.make w ' ') ])))

(* With panes (and enough width) the output viewport splits vertically:
   the scrollable output on the left, panes on the right, separated by a
   '│' column. The input area and the status bar stay full-width. *)
let viewport_rows ~w ~view_h (m : Model.t) =
  if Array.length m.panes = 0 || w < 20 then output_rows ~w ~view_h m
  else
    let lw = w / 2 in
    let rw = w - lw - 1 in
    List.map2
      (fun l r -> l @ ((Info_text, "│") :: r))
      (output_rows ~w:lw ~view_h m)
      (pane_rows ~w:rw ~view_h m)

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
        input @ viewport_rows ~w ~view_h m @ [ status_row ~w ~view_h m ]
      in
      { rows; cursor }

(* --- test helpers --- *)

let row_text segs = String.concat "" (List.map snd segs)
let to_strings frame = List.map row_text frame.rows
