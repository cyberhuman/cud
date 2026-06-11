(** Side-by-side comparison of ine's hand-rolled [Editor] and the zed-backed
    prototype [Zed_editor], over inputs representative of editing shell
    arguments. Prints SAME/DIFF per case; always exits 0 (it is a probe, not
    a regression test). *)

module E = Ine_lib.Editor
module Z = Zed_editor

let diffs = ref 0

let show text cursor = Printf.sprintf "%S cur=%d" text cursor

let case name ~ine ~zed =
  let i = show (fst ine) (snd ine) and z = show (fst zed) (snd zed) in
  if i = z then Printf.printf "SAME  %-38s %s\n" name i
  else begin
    incr diffs;
    Printf.printf "DIFF  %-38s ine: %s\n      %-38s zed: %s\n" name i "" z
  end

let e_state e = (E.to_string e, E.cursor e)
let z_state z = (Z.to_string z, Z.cursor z)

(* Apply the same abstract operation to both implementations starting from
   the same text with the cursor at the end. *)
let run name text op_e op_z =
  case
    (Printf.sprintf "%s %S" name text)
    ~ine:(e_state (op_e (E.of_string text)))
    ~zed:(z_state (op_z (Z.of_string text)))

let () =
  (* Plain motions and edits: expected to agree. *)
  run "left" "ab" E.left Z.left;
  run "home" "hello world" E.home Z.home;
  run "backspace" "abλ" E.backspace Z.backspace;
  run "delete@home" "ab" (fun e -> E.delete (E.home e)) (fun z -> Z.delete (Z.home z));
  run "kill_to_start" "hello world" E.kill_to_start Z.kill_to_start;
  run "kill_to_end@home" "hello world"
    (fun e -> E.kill_to_end (E.home e))
    (fun z -> Z.kill_to_end (Z.home z));
  run "insert-x@left" "ab"
    (fun e -> E.insert (Uchar.of_char 'x') (E.left e))
    (fun z -> Z.insert (Uchar.of_char 'x') (Z.left z));

  (* Word operations on plain words: expected to agree. *)
  run "word_left" "hello world" E.word_left Z.word_left;
  run "kill_prev_word" "hello world" E.kill_prev_word Z.kill_prev_word;
  run "word_right@home" "hello world"
    (fun e -> E.word_right (E.home e))
    (fun z -> Z.word_right (Z.home z));

  (* Word operations on shell-ish tokens: ine treats a whitespace-delimited
     token as one word; zed segments at punctuation (UAX#29). *)
  run "kill_prev_word" "--filter=name" E.kill_prev_word Z.kill_prev_word;
  run "kill_prev_word" "path/to/file.txt" E.kill_prev_word Z.kill_prev_word;
  run "kill_prev_word" "a-b" E.kill_prev_word Z.kill_prev_word;
  run "word_left" "git log --one-line" E.word_left Z.word_left;

  (* Trailing spaces: ine's Ctrl-W first skips blanks then kills the word;
     zed's delete_prev_word behaviour may differ. *)
  run "kill_prev_word" "hello world  " E.kill_prev_word Z.kill_prev_word;

  (* Combining characters: ine counts scalar values, zed counts
     Zed_char.t (base + combining marks). e\u{0301} = e + COMBINING ACUTE. *)
  run "cursor-count" "e\xcc\x81x" Fun.id Fun.id;
  run "backspace" "e\xcc\x81" E.backspace Z.backspace;

  Printf.printf "\n%d difference(s)\n" !diffs
