(** Prototype: ine's [Editor] interface implemented on top of the [zed]
    edition engine (Zed_edit / Zed_rope / Zed_cursor).

    Zed's engine is mutable and React-signal driven, while ine's editor is a
    pure value. To keep the pure interface, every operation materialises a
    fresh engine, loads the text, positions the cursor, applies one Zed
    action, and reads the result back. This is obviously not how zed is meant
    to be used (it is designed as a long-lived stateful buffer shared by the
    rendering layer via signals); the wrapper exists only to compare
    semantics and to gauge the impedance mismatch.

    Known, deliberate differences from [Ine_lib.Editor]:
    - cursor/length count Zed_char.t "characters" (grapheme-like core +
      combining marks), not Unicode scalar values;
    - word operations use UAX#29 word segmentation (uuseg), not
      whitespace-delimited words;
    - operations cannot signal "no-op" via physical equality ([==]), because
      the result is always freshly rebuilt. *)

type t = { text : string; cursor : int }

let empty = { text = ""; cursor = 0 }
let of_string s = { text = s; cursor = Zed_string.length (Zed_string.of_utf8 s) }
let to_string t = t.text
let cursor t = t.cursor
let length t = Zed_string.length (Zed_string.of_utf8 t.text)

(* Build an engine around [t], run [f] on an edit context, read back. *)
let with_zed f t =
  let engine : unit Zed_edit.t = Zed_edit.create () in
  let zcursor = Zed_edit.new_cursor engine in
  let ctx = Zed_edit.context engine zcursor in
  Zed_edit.insert ctx (Zed_rope.of_string (Zed_string.of_utf8 t.text));
  Zed_edit.goto ctx t.cursor;
  f ctx;
  {
    text = Zed_string.to_utf8 (Zed_rope.to_string (Zed_edit.text engine));
    cursor = Zed_edit.position ctx;
  }

let insert u = with_zed (fun ctx -> Zed_edit.insert_char ctx u)

let insert_string s t =
  with_zed
    (fun ctx -> Zed_edit.insert ctx (Zed_rope.of_string (Zed_string.of_utf8 s)))
    t

let backspace = with_zed Zed_edit.delete_prev_char
let delete = with_zed Zed_edit.delete_next_char
let left = with_zed Zed_edit.prev_char
let right = with_zed Zed_edit.next_char

(* Single-line buffer: beginning/end of line == beginning/end of text. *)
let home = with_zed Zed_edit.goto_bol
let end_ = with_zed Zed_edit.goto_eol
let kill_to_end = with_zed Zed_edit.delete_next_line
let kill_to_start = with_zed Zed_edit.delete_prev_line
let kill_prev_word = with_zed Zed_edit.delete_prev_word
let word_left = with_zed Zed_edit.prev_word
let word_right = with_zed Zed_edit.next_word
