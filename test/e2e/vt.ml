(** A minimal VT/xterm screen emulator: just enough to interpret what Notty
    emits (cursor positioning, erases, SGR, alt screen, cursor visibility)
    and reconstruct the character grid and cursor position. Unknown
    sequences are skipped. *)

type t = {
  mutable w : int;
  mutable h : int;
  mutable grid : string array array;  (* grid.(y).(x) is a 1-character string *)
  mutable cx : int;  (* may sit at [w] right after writing the last column *)
  mutable cy : int;
  mutable cursor_visible : bool;
  pending : Buffer.t;  (* incomplete trailing escape/UTF-8 sequence *)
}

let blank_grid ~w ~h = Array.init h (fun _ -> Array.make w " ")

let create ~w ~h =
  {
    w;
    h;
    grid = blank_grid ~w ~h;
    cx = 0;
    cy = 0;
    cursor_visible = true;
    pending = Buffer.create 256;
  }

let resize t ~w ~h =
  let g = blank_grid ~w ~h in
  for y = 0 to min h t.h - 1 do
    for x = 0 to min w t.w - 1 do
      g.(y).(x) <- t.grid.(y).(x)
    done
  done;
  t.grid <- g;
  t.w <- w;
  t.h <- h;
  t.cx <- min t.cx (w - 1);
  t.cy <- min t.cy (h - 1)

let clear t =
  t.grid <- blank_grid ~w:t.w ~h:t.h

let scroll_up t =
  Array.blit t.grid 1 t.grid 0 (t.h - 1);
  t.grid.(t.h - 1) <- Array.make t.w " "

let line_feed t =
  if t.cy >= t.h - 1 then scroll_up t else t.cy <- t.cy + 1

let put t s =
  if t.cx >= t.w then begin
    t.cx <- 0;
    line_feed t
  end;
  t.grid.(t.cy).(t.cx) <- s;
  t.cx <- t.cx + 1

let erase t ~from_y ~from_x ~to_y ~to_x =
  for y = max 0 from_y to min (t.h - 1) to_y do
    let x0 = if y = from_y then max 0 from_x else 0 in
    let x1 = if y = to_y then min (t.w - 1) to_x else t.w - 1 in
    for x = x0 to x1 do
      t.grid.(y).(x) <- " "
    done
  done

let clamp lo hi v = max lo (min hi v)

let csi t ~private_ ~params ~final =
  let p n default =
    match List.nth_opt params n with Some (Some v) -> v | _ -> default
  in
  let cx = clamp 0 (t.w - 1) t.cx and cy = clamp 0 (t.h - 1) t.cy in
  if private_ then begin
    match (p 0 0, final) with
    | 25, 'h' -> t.cursor_visible <- true
    | 25, 'l' -> t.cursor_visible <- false
    | 1049, ('h' | 'l') ->
        (* We don't keep two screens; entering or leaving the alternate
           screen just clears the view. *)
        clear t;
        t.cx <- 0;
        t.cy <- 0
    | _ -> ()
  end
  else
    match final with
    | 'H' | 'f' ->
        t.cy <- clamp 0 (t.h - 1) (p 0 1 - 1);
        t.cx <- clamp 0 (t.w - 1) (p 1 1 - 1)
    | 'A' -> t.cy <- clamp 0 (t.h - 1) (cy - p 0 1)
    | 'B' -> t.cy <- clamp 0 (t.h - 1) (cy + p 0 1)
    | 'C' -> t.cx <- clamp 0 (t.w - 1) (cx + p 0 1)
    | 'D' -> t.cx <- clamp 0 (t.w - 1) (cx - p 0 1)
    | 'G' -> t.cx <- clamp 0 (t.w - 1) (p 0 1 - 1)
    | 'd' -> t.cy <- clamp 0 (t.h - 1) (p 0 1 - 1)
    | 'J' -> (
        match p 0 0 with
        | 0 -> erase t ~from_y:cy ~from_x:cx ~to_y:(t.h - 1) ~to_x:(t.w - 1)
        | 1 -> erase t ~from_y:0 ~from_x:0 ~to_y:cy ~to_x:cx
        | _ -> clear t)
    | 'K' -> (
        match p 0 0 with
        | 0 -> erase t ~from_y:cy ~from_x:cx ~to_y:cy ~to_x:(t.w - 1)
        | 1 -> erase t ~from_y:cy ~from_x:0 ~to_y:cy ~to_x:cx
        | _ -> erase t ~from_y:cy ~from_x:0 ~to_y:cy ~to_x:(t.w - 1))
    | 'X' -> erase t ~from_y:cy ~from_x:cx ~to_y:cy ~to_x:(cx + p 0 1 - 1)
    | _ -> () (* SGR 'm' and anything else: no effect on the grid *)

let parse_params s =
  (* "1;5" -> [Some 1; Some 5]; empty fields -> None *)
  String.split_on_char ';' s
  |> List.map (fun field -> int_of_string_opt field)

(* Returns the position after the escape sequence starting at [i]
   (s.[i] = ESC), or None if it is incomplete. *)
let parse_escape t s i n =
  if i + 1 >= n then None
  else
    match s.[i + 1] with
    | '[' ->
        let rec scan j =
          if j >= n then None
          else
            let c = s.[j] in
            if c >= '\x40' && c <= '\x7e' then begin
              let body = String.sub s (i + 2) (j - i - 2) in
              let private_ = String.length body > 0 && body.[0] = '?' in
              let body =
                if private_ then String.sub body 1 (String.length body - 1)
                else body
              in
              (* drop intermediate bytes, keep digits and ';' *)
              let body =
                String.to_seq body
                |> Seq.filter (fun c -> (c >= '0' && c <= '9') || c = ';')
                |> String.of_seq
              in
              csi t ~private_ ~params:(parse_params body) ~final:c;
              Some (j + 1)
            end
            else scan (j + 1)
        in
        scan (i + 2)
    | ']' ->
        (* OSC: skip until BEL or ESC backslash *)
        let rec scan j =
          if j >= n then None
          else if s.[j] = '\x07' then Some (j + 1)
          else if s.[j] = '\x1b' then
            if j + 1 >= n then None
            else if s.[j + 1] = '\\' then Some (j + 2)
            else Some (j + 1)
          else scan (j + 1)
        in
        scan (i + 2)
    | '(' | ')' -> if i + 2 >= n then None else Some (i + 3)
    | _ -> Some (i + 2)

let feed t bytes =
  Buffer.add_string t.pending bytes;
  let s = Buffer.contents t.pending in
  Buffer.clear t.pending;
  let n = String.length s in
  let i = ref 0 in
  let incomplete = ref (-1) in
  while !i < n && !incomplete < 0 do
    let c = s.[!i] in
    if c = '\x1b' then
      match parse_escape t s !i n with
      | None -> incomplete := !i
      | Some next -> i := next
    else if c = '\r' then begin
      t.cx <- 0;
      incr i
    end
    else if c = '\n' then begin
      line_feed t;
      incr i
    end
    else if c = '\b' then begin
      t.cx <- max 0 (t.cx - 1);
      incr i
    end
    else if Char.code c < 0x20 then incr i (* BEL, other C0: ignore *)
    else begin
      let d = String.get_utf_8_uchar s !i in
      if Uchar.utf_decode_is_valid d then begin
        let len = Uchar.utf_decode_length d in
        put t (String.sub s !i len);
        i := !i + len
      end
      else if n - !i < 4 then incomplete := !i (* possibly truncated UTF-8 *)
      else begin
        put t "?";
        incr i
      end
    end
  done;
  if !incomplete >= 0 then
    Buffer.add_string t.pending (String.sub s !incomplete (n - !incomplete))

let row t y = String.concat "" (Array.to_list t.grid.(y))

let row_trim t y =
  let s = row t y in
  let n = String.length s in
  let rec last i = if i > 0 && s.[i - 1] = ' ' then last (i - 1) else i in
  String.sub s 0 (last n)

let cursor t = (min t.cx (t.w - 1), t.cy)
let cursor_visible t = t.cursor_visible

let dump t =
  String.concat "\n"
    (List.init t.h (fun y -> Printf.sprintf "%2d|%s|" y (row t y)))
