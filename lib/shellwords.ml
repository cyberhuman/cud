(** Shell-like word splitting for the argument line.

    Words are separated by blanks. Single quotes preserve everything
    literally; double quotes preserve everything except backslash-escaped
    double quotes and backslashes; a backslash outside quotes escapes the
    next character. *)

type error =
  | Unterminated_single_quote
  | Unterminated_double_quote
  | Trailing_backslash

let error_message = function
  | Unterminated_single_quote -> "unterminated '"
  | Unterminated_double_quote -> "unterminated \""
  | Trailing_backslash -> "trailing backslash"

let split s =
  let n = String.length s in
  let words = ref [] in
  let buf = Buffer.create 16 in
  let in_word = ref false in
  let flush_word () =
    if !in_word then begin
      words := Buffer.contents buf :: !words;
      Buffer.clear buf;
      in_word := false
    end
  in
  let rec plain i =
    if i >= n then begin
      flush_word ();
      Ok (List.rev !words)
    end
    else
      match s.[i] with
      | ' ' | '\t' | '\n' ->
          flush_word ();
          plain (i + 1)
      | '\'' ->
          in_word := true;
          single (i + 1)
      | '"' ->
          in_word := true;
          double (i + 1)
      | '\\' ->
          if i + 1 >= n then Error Trailing_backslash
          else begin
            in_word := true;
            Buffer.add_char buf s.[i + 1];
            plain (i + 2)
          end
      | c ->
          in_word := true;
          Buffer.add_char buf c;
          plain (i + 1)
  and single i =
    if i >= n then Error Unterminated_single_quote
    else if s.[i] = '\'' then plain (i + 1)
    else begin
      Buffer.add_char buf s.[i];
      single (i + 1)
    end
  and double i =
    if i >= n then Error Unterminated_double_quote
    else
      match s.[i] with
      | '"' -> plain (i + 1)
      | '\\' when i + 1 < n && (s.[i + 1] = '"' || s.[i + 1] = '\\') ->
          Buffer.add_char buf s.[i + 1];
          double (i + 2)
      | c ->
          Buffer.add_char buf c;
          double (i + 1)
  in
  plain 0

(** Quote a word for display as a shell command, only when needed. *)
let quote_word s =
  let safe c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | '@' | '%' | '+' | '=' | ':' | ',' | '.' | '/' | '_' | '-' -> true
    | _ -> false
  in
  if s <> "" && String.for_all safe s then s else Filename.quote s

let join_command words = String.concat " " (List.map quote_word words)
