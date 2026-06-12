(** Pure screen renderer: model + terminal size -> a frame of styled rows.

    Layout invariant: row 0 is the input line, the last row is the status
    bar, everything between is the scrollable output viewport. Every frame
    has exactly [h] rows and every row exactly [w] columns (counting Unicode
    characters as one column each), so the layout can never shift. *)

type style =
  | Prompt
  | Input
  | Out_text
  | Err_text
  | Info_text
  | Bar
  | Bar_alert
  | Bar_mode
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

let style_of_kind = function
  | Model.Out -> Out_text
  | Model.Err -> Err_text
  | Model.Info -> Info_text

let input_row ~w (m : Model.t) =
  let prompt =
    sanitize_flat
      (match m.cmd with
      | Some c -> Shellwords.join_command (c :: m.fixed_args) ^ "> "
      | None -> "> ")
  in
  (* Keep at least one column for the text on absurdly narrow terminals. *)
  let plen = min (ulength prompt) (max 0 (w - 1)) in
  let prompt = usub prompt 0 plen in
  let text = sanitize_flat (Editor.to_string m.editor) in
  let tlen = ulength text in
  let avail = w - plen in
  let cur = Editor.cursor m.editor in
  let hscroll = if cur < avail then 0 else cur - avail + 1 in
  let visible = usub text hscroll (min avail (max 0 (tlen - hscroll))) in
  let row =
    [ (Prompt, prompt); (Input, visible ^ String.make (avail - ulength visible) ' ') ]
  in
  let cx = min (plen + cur - hscroll) (max 0 (w - 1)) in
  (row, (cx, 0))

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
  let hints = "enter run · ^D accept · esc quit " in
  let leftw = ulength vim_mode + ulength state + ulength parse + ulength mode in
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

let output_rows ~w ~view_h (m : Model.t) =
  let count = Array.length m.lines in
  let scroll = Model.clamp_scroll ~view_h m m.scroll in
  List.init view_h (fun i ->
      let idx = scroll + i in
      if idx < count then
        let line = m.lines.(idx) in
        [ (style_of_kind line.kind, fit w (sanitize_line line.text)) ]
      else [ (Out_text, String.make w ' ') ])

let render ~w ~h (m : Model.t) =
  if w <= 0 || h <= 0 then { rows = []; cursor = None }
  else
    let input, cursor = input_row ~w m in
    if h = 1 then { rows = [ input ]; cursor = Some cursor }
    else
      let view_h = h - 2 in
      let rows =
        (input :: output_rows ~w ~view_h m) @ [ status_row ~w ~view_h m ]
      in
      { rows; cursor = Some cursor }

(* --- test helpers --- *)

let row_text segs = String.concat "" (List.map snd segs)
let to_strings frame = List.map row_text frame.rows
