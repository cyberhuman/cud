(** Pure single-line editor: text as Unicode characters plus a cursor.

    Represented as a zipper: [before] holds the characters left of the cursor
    in reverse order, [after] the characters right of it. All operations are
    pure; operations that cannot apply (e.g. [left] at the start of the line)
    return the value physically unchanged, so callers can detect no-ops with
    [==]. *)

type t = {
  before : Uchar.t list; (* reversed *)
  after : Uchar.t list;
}

let empty = { before = []; after = [] }

let uchars_of_string s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      let u =
        if Uchar.utf_decode_is_valid d then Uchar.utf_decode_uchar d
        else Uchar.rep
      in
      go (i + Uchar.utf_decode_length d) (u :: acc)
  in
  go 0 []

let string_of_uchars us =
  let b = Buffer.create 64 in
  List.iter (Buffer.add_utf_8_uchar b) us;
  Buffer.contents b

let of_string s = { before = List.rev (uchars_of_string s); after = [] }
let to_string t = string_of_uchars (List.rev_append t.before t.after)
let to_uchars t = List.rev_append t.before t.after

(** Cursor position, in characters from the start of the line. *)
let cursor t = List.length t.before

let length t = List.length t.before + List.length t.after
let insert u t = { t with before = u :: t.before }

let insert_string s t =
  List.fold_left (fun t u -> insert u t) t (uchars_of_string s)

let backspace t =
  match t.before with [] -> t | _ :: tl -> { t with before = tl }

let delete t = match t.after with [] -> t | _ :: tl -> { t with after = tl }

let left t =
  match t.before with
  | [] -> t
  | u :: tl -> { before = tl; after = u :: t.after }

let right t =
  match t.after with
  | [] -> t
  | u :: tl -> { before = u :: t.before; after = tl }

let home t =
  match t.before with
  | [] -> t
  | _ -> { before = []; after = List.rev_append t.before t.after }

let end_ t =
  match t.after with
  | [] -> t
  | _ -> { before = List.rev_append t.after t.before; after = [] }

let kill_to_end t = match t.after with [] -> t | _ -> { t with after = [] }

let kill_to_start t =
  match t.before with [] -> t | _ -> { t with before = [] }

let is_blank u =
  match Uchar.to_int u with 0x20 | 0x09 -> true | _ -> false

(* [before] is scanned from the cursor towards the start of the line: first
   skip blanks, then the word itself. *)
let rec drop_blanks = function
  | u :: tl when is_blank u -> drop_blanks tl
  | l -> l

let rec drop_word = function
  | u :: tl when not (is_blank u) -> drop_word tl
  | l -> l

let kill_prev_word t =
  match t.before with
  | [] -> t
  | _ -> { t with before = drop_word (drop_blanks t.before) }

let word_left t =
  match t.before with
  | [] -> t
  | _ ->
      let rec skip_blanks before after =
        match before with
        | u :: tl when is_blank u -> skip_blanks tl (u :: after)
        | _ -> skip_word before after
      and skip_word before after =
        match before with
        | u :: tl when not (is_blank u) -> skip_word tl (u :: after)
        | _ -> { before; after }
      in
      skip_blanks t.before t.after

let word_right t =
  match t.after with
  | [] -> t
  | _ ->
      let rec skip_blanks before after =
        match after with
        | u :: tl when is_blank u -> skip_blanks (u :: before) tl
        | _ -> skip_word before after
      and skip_word before after =
        match after with
        | u :: tl when not (is_blank u) -> skip_word (u :: before) tl
        | _ -> { before; after }
      in
      skip_blanks t.before t.after
