(** End-to-end TUI tests: run the real [cud] binary on a pseudo-terminal,
    send key sequences, and assert on the rendered screen and cursor
    reconstructed by {!Vt}. *)

let cud = Sys.getenv "CUD"

let failures = ref 0

(* --- key sequences --- *)
let ctrl c = String.make 1 (Char.chr (Char.code c land 0x1f))
let up = "\x1b[A"
let down = "\x1b[B"
let right = "\x1b[C"
let left = "\x1b[D"
let home = "\x1b[H"
let end_ = "\x1b[F"
let pgup = "\x1b[5~"
let pgdn = "\x1b[6~"
let ctrl_right = "\x1b[1;5C"
let meta_b = "\x1bb"
let backtab = "\x1b[Z" (* Shift+Tab *)
let enter = "\r"

type sess = {
  master : Unix.file_descr;
  pid : int;
  vt : Vt.t;
  transcript : Buffer.t;
  name : string;
}

let spawn ?(w = 80) ?(h = 24) name shell_cmd =
  let master, slave = Pty.openpty ~w ~h in
  match Unix.fork () with
  | 0 -> (
      Unix.close master;
      (try Pty.login_tty slave with _ -> exit 125);
      let env =
        [|
          "TERM=xterm";
          "PATH=" ^ Sys.getenv "PATH";
          "HOME=" ^ (try Sys.getenv "HOME" with Not_found -> "/");
          "LC_ALL=C.UTF-8";
        |]
      in
      try Unix.execve "/bin/sh" [| "sh"; "-c"; shell_cmd |] env
      with _ -> exit 126)
  | pid ->
      Unix.close slave;
      { master; pid; vt = Vt.create ~w ~h; transcript = Buffer.create 8192; name }

(* Read whatever output is available within [wait] seconds. *)
let pump ?(wait = 0.05) sess =
  let buf = Bytes.create 65536 in
  let deadline = Unix.gettimeofday () +. wait in
  let rec go () =
    let timeout = max 0. (deadline -. Unix.gettimeofday ()) in
    match Unix.select [ sess.master ] [] [] timeout with
    | [], _, _ -> ()
    | _ -> (
        match Unix.read sess.master buf 0 (Bytes.length buf) with
        | 0 -> ()
        | n ->
            let s = Bytes.sub_string buf 0 n in
            Buffer.add_string sess.transcript s;
            Vt.feed sess.vt s;
            go ()
        | exception Unix.Unix_error (Unix.EIO, _, _) -> () (* pty closed *))
  in
  go ()

let fail sess what =
  incr failures;
  let x, y = Vt.cursor sess.vt in
  Printf.printf "FAIL %s: %s\n--- screen (cursor %d,%d%s):\n%s\n" sess.name
    what x y
    (if Vt.cursor_visible sess.vt then "" else ", hidden")
    (Vt.dump sess.vt)

let wait_for ?(timeout = 10.) sess what pred =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec go () =
    pump sess;
    if pred sess.vt then ()
    else if Unix.gettimeofday () > deadline then fail sess ("timeout: " ^ what)
    else go ()
  in
  go ()

let send sess s =
  ignore (Unix.write_substring sess.master s 0 (String.length s))

let contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let expect_row sess y expected =
  wait_for sess
    (Printf.sprintf "row %d = %S" y expected)
    (fun vt -> Vt.row_trim vt y = expected)

let expect_row_contains sess y needle =
  wait_for sess
    (Printf.sprintf "row %d contains %S" y needle)
    (fun vt -> contains (Vt.row_trim vt y) needle)

let expect_some_row_contains sess needle =
  wait_for sess
    (Printf.sprintf "some row contains %S" needle)
    (fun vt ->
      List.exists
        (fun y -> contains (Vt.row_trim vt y) needle)
        (List.init vt.Vt.h Fun.id))

let expect_status sess needle =
  wait_for sess
    (Printf.sprintf "status contains %S" needle)
    (fun vt -> contains (Vt.row_trim vt (vt.Vt.h - 1)) needle)

let expect_no_status sess needle =
  pump sess ~wait:0.2;
  let vt = sess.vt in
  if contains (Vt.row_trim vt (vt.Vt.h - 1)) needle then
    fail sess (Printf.sprintf "status unexpectedly contains %S" needle)

let expect_cursor_hidden sess =
  wait_for sess "cursor hidden"
    (fun vt -> not (Vt.cursor_visible vt))
(* The two halves of a row when the output is split by --lens/--hint: the
   main output occupies columns [0, w/2), the separator sits at w/2, the
   lens/hint panes start after it. *)
let left_half vt y =
  let lw = vt.Vt.w / 2 in
  String.concat "" (Array.to_list (Array.sub vt.Vt.grid.(y) 0 lw))

let right_half vt y =
  let lw = vt.Vt.w / 2 in
  String.concat ""
    (Array.to_list (Array.sub vt.Vt.grid.(y) (lw + 1) (vt.Vt.w - lw - 1)))

let expect_left sess y expected =
  wait_for sess
    (Printf.sprintf "left half of row %d = %S" y expected)
    (fun vt -> String.trim (left_half vt y) = expected)

let expect_left_contains sess y needle =
  wait_for sess
    (Printf.sprintf "left half of row %d contains %S" y needle)
    (fun vt -> contains (left_half vt y) needle)

let expect_right_contains sess y needle =
  wait_for sess
    (Printf.sprintf "right half of row %d contains %S" y needle)
    (fun vt -> contains (right_half vt y) needle)

let expect_right sess y expected =
  wait_for sess
    (Printf.sprintf "right half of row %d = %S" y expected)
    (fun vt -> String.trim (right_half vt y) = expected)

let expect_cursor sess (x, y) =
  wait_for sess
    (Printf.sprintf "cursor at %d,%d (visible)" x y)
    (fun vt -> Vt.cursor vt = (x, y) && Vt.cursor_visible vt)

let wait_exit ?(timeout = 5.) sess =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec go () =
    pump sess;
    match Unix.waitpid [ Unix.WNOHANG ] sess.pid with
    | 0, _ ->
        if Unix.gettimeofday () > deadline then begin
          fail sess "timeout waiting for exit";
          Unix.kill sess.pid Sys.sigkill;
          ignore (Unix.waitpid [] sess.pid);
          None
        end
        else go ()
    | _, status -> Some status
  in
  let r = go () in
  Unix.close sess.master;
  r

let expect_exit sess code =
  match wait_exit sess with
  | Some (Unix.WEXITED n) when n = code -> ()
  | Some st ->
      incr failures;
      let show = function
        | Unix.WEXITED n -> Printf.sprintf "exit %d" n
        | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
        | Unix.WSTOPPED n -> Printf.sprintf "stop %d" n
      in
      Printf.printf "FAIL %s: expected exit %d, got %s\n" sess.name code
        (show st)
  | None -> ()

(* Plain text written after the TUI left the alternate screen — i.e. the
   final printed arguments. *)
let printed_after_release sess =
  let s = Buffer.contents sess.transcript in
  let marker = "\x1b[?1049l" in
  let rec last_index from acc =
    match String.index_from_opt s from '\x1b' with
    | None -> acc
    | Some i ->
        let here =
          if
            i + String.length marker <= String.length s
            && String.sub s i (String.length marker) = marker
          then Some (i + String.length marker)
          else None
        in
        last_index (i + 1) (match here with Some _ -> here | None -> acc)
  in
  match last_index 0 None with
  | None -> ""
  | Some start ->
      (* strip remaining escape sequences with a scratch emulator-less scan *)
      let b = Buffer.create 64 in
      let n = String.length s in
      let i = ref start in
      while !i < n do
        if s.[!i] = '\x1b' then begin
          (* skip CSI/2-char escapes *)
          if !i + 1 < n && s.[!i + 1] = '[' then begin
            i := !i + 2;
            while !i < n && not (s.[!i] >= '\x40' && s.[!i] <= '\x7e') do
              incr i
            done;
            if !i < n then incr i
          end
          else i := !i + 2
        end
        else begin
          Buffer.add_char b s.[!i];
          incr i
        end
      done;
      Buffer.contents b

let quote = Filename.quote
let json = {|{"a":1,"b":[1,2,3]}|}

(* --- tests --- *)

let test_jq_flow () =
  let sess =
    spawn "jq-flow"
      (Printf.sprintf "printf %%s %s | %s --debounce 0.05 -i . jq" (quote json)
         (quote cud))
  in
  (* startup layout: input on top, pretty-printed output, status bar *)
  expect_row sess 0 "jq> .";
  expect_row sess 1 "{";
  expect_row sess 2 {|  "a": 1,|};
  expect_row sess 3 {|  "b": [|};
  expect_row sess 8 "}";
  expect_status sess "exit 0";
  expect_status sess "1-8/8";
  expect_cursor sess (5, 0);

  (* typing re-runs after the debounce *)
  send sess "b";
  expect_row sess 0 "jq> .b";
  expect_row sess 1 "[";
  expect_row sess 2 "  1,";
  expect_row sess 5 "]";
  expect_status sess "1-5/5";
  expect_cursor sess (6, 0);

  (* line editing: cursor moves, layout never shifts *)
  send sess (ctrl 'a');
  expect_cursor sess (4, 0);
  send sess right;
  expect_cursor sess (5, 0);
  send sess (ctrl 'e');
  expect_cursor sess (6, 0);
  send sess (ctrl 'b');
  expect_cursor sess (5, 0);
  send sess home;
  expect_cursor sess (4, 0);
  send sess end_;
  expect_cursor sess (6, 0);
  send sess left;
  expect_cursor sess (5, 0);
  send sess right;
  expect_cursor sess (6, 0);
  expect_row sess 0 "jq> .b" (* the text itself did not change *);

  (* multiple words: " -c" makes the output compact *)
  send sess " -c";
  expect_row sess 0 "jq> .b -c";
  expect_cursor sess (9, 0);
  expect_row sess 1 "[1,2,3]";
  expect_status sess "1-1/1";

  (* C-w kills the last word *)
  send sess (ctrl 'w');
  expect_row sess 0 "jq> .b";
  expect_cursor sess (7, 0);
  expect_status sess "1-5/5";

  (* word motion *)
  send sess meta_b;
  expect_cursor sess (4, 0);
  send sess ctrl_right;
  expect_cursor sess (6, 0);

  (* C-k kills to end of line *)
  send sess (ctrl 'k');
  expect_row sess 0 "jq> .b";
  expect_cursor sess (6, 0);

  (* C-u clears the line; bare jq defaults the filter to '.' *)
  send sess (ctrl 'u');
  expect_row sess 0 "jq>";
  expect_cursor sess (4, 0);
  expect_status sess "exit 0";
  expect_status sess "1-8/8";

  (* a broken filter: stderr is displayed, exit code shown *)
  send sess "bogus";
  expect_status sess "exit 3";
  expect_some_row_contains sess "jq: error";
  send sess (ctrl 'u');
  expect_status sess "1-8/8";

  (* unterminated quote is reported, fixed quote recovers *)
  send sess "'";
  expect_status sess "unterminated '";
  send sess ".'";
  expect_row sess 0 "jq> '.'";
  expect_status sess "exit 0";
  expect_no_status sess "unterminated";

  (* single-arg mode toggle: a filter with spaces *)
  send sess (ctrl 'u');
  send sess ". | keys";
  expect_row sess 0 "jq> . | keys";
  expect_status sess "exit 2" (* split mode: "|" and "keys" treated as files *);
  send sess (ctrl 't');
  expect_status sess "[1 arg]";
  expect_status sess "exit 0";
  expect_row sess 1 "[";
  expect_row_contains sess 2 {|"a"|};

  (* accept: prints the args (quoted, since they contain spaces) *)
  send sess (ctrl 'd');
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "'. | keys'") then begin
    incr failures;
    Printf.printf "FAIL jq-flow: printed args %S\n" printed
  end

let test_scroll_resize_cancel () =
  let sess =
    spawn "scroll" (Printf.sprintf "seq 1 50 | %s --debounce 0.05 cat" (quote cud))
  in
  expect_row sess 0 "cat>";
  expect_row sess 1 "1";
  expect_row sess 22 "22";
  expect_status sess "exit 0";
  expect_status sess "1-22/50";
  expect_cursor sess (5, 0);

  send sess down;
  expect_row sess 1 "2";
  expect_status sess "2-23/50";
  send sess pgdn;
  expect_row sess 1 "24";
  expect_status sess "24-45/50";
  send sess pgdn (* clamps at the bottom *);
  expect_row sess 1 "29";
  expect_row sess 22 "50";
  expect_status sess "29-50/50";
  send sess up;
  expect_row sess 1 "28";
  send sess pgup;
  expect_row sess 1 "6";
  send sess pgup;
  expect_row sess 1 "1";
  expect_status sess "1-22/50";

  (* Home edits the input line, it does not scroll the output *)
  send sess home;
  expect_cursor sess (5, 0);
  pump sess ~wait:0.2;
  expect_row sess 1 "1";

  (* resize: layout follows the new geometry (TIOCSWINSZ delivers SIGWINCH) *)
  Pty.set_winsize sess.master ~w:100 ~h:30;
  Vt.resize sess.vt ~w:100 ~h:30;
  expect_row sess 0 "cat>";
  expect_row sess 28 "28";
  expect_status sess "1-28/50";
  expect_cursor sess (5, 0);

  send sess (ctrl 'c');
  expect_exit sess 130

let test_manual_mode () =
  let sess =
    spawn "manual"
      (Printf.sprintf "printf %%s %s | %s -m jq" (quote json) (quote cud))
  in
  (* initial run: bare jq pretty-prints the whole input *)
  expect_row sess 1 "{";
  expect_status sess "1-8/8";
  send sess ".a";
  expect_row sess 0 "jq> .a";
  (* no auto re-run in manual mode: output stays as it was *)
  Unix.sleepf 0.7;
  pump sess;
  expect_row sess 1 "{";
  expect_status sess "1-8/8";
  (* Enter runs *)
  send sess enter;
  expect_row sess 1 "1";
  expect_status sess "1-1/1";
  send sess (ctrl 'd');
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed ".a") then begin
    incr failures;
    Printf.printf "FAIL manual: printed args %S\n" printed
  end

let test_fixed_args_and_lines_output () =
  let sess =
    spawn "echo"
      (Printf.sprintf "printf '' | %s --debounce 0.05 -l -- echo hello"
         (quote cud))
  in
  expect_row sess 0 "echo hello>";
  expect_row sess 1 "hello";
  send sess "world \"two words\"";
  expect_row sess 1 "hello world two words";
  send sess (ctrl 'd');
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "world\r\ntwo words") then begin
    incr failures;
    Printf.printf "FAIL echo: printed args %S\n" printed
  end

let test_null_output () =
  let sess =
    spawn "null"
      (Printf.sprintf "printf '' | %s --debounce 0.05 -0 -- echo x" (quote cud))
  in
  expect_row sess 1 "x";
  send sess "a b";
  expect_row sess 1 "x a b";
  send sess (ctrl 'd');
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "a\x00b\x00") then begin
    incr failures;
    Printf.printf "FAIL null: printed args %S\n" printed
  end

let test_command_output () =
  let sess =
    spawn "command"
      (Printf.sprintf "printf '' | %s --debounce 0.05 -c -- echo hello"
         (quote cud))
  in
  expect_row sess 1 "hello";
  send sess "two words";
  expect_row sess 1 "hello two words";
  send sess (ctrl 'd');
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "echo hello two words") then begin
    incr failures;
    Printf.printf "FAIL command: printed %S\n" printed
  end

let test_vim_mode () =
  let sess =
    spawn "vim"
      (Printf.sprintf "printf %%s %s | %s --debounce 0.05 --vim -i .b jq"
         (quote json) (quote cud))
  in
  expect_status sess "INSERT";
  expect_row sess 0 "jq> .b";
  expect_status sess "1-5/5";
  expect_cursor sess (6, 0);
  (* Esc enters normal mode, cursor steps back onto the last character *)
  send sess "\x1b";
  expect_status sess "NORMAL";
  expect_cursor sess (5, 0);
  send sess "0";
  expect_cursor sess (4, 0);
  send sess "$";
  expect_cursor sess (5, 0);
  (* x deletes under the cursor and the command re-runs *)
  send sess "x";
  expect_row sess 0 "jq> .";
  expect_cursor sess (4, 0);
  expect_status sess "1-8/8";
  (* undo *)
  send sess "u";
  expect_row sess 0 "jq> .b";
  expect_status sess "1-5/5";
  (* A appends at end of line, back in insert mode *)
  send sess "A";
  expect_status sess "INSERT";
  expect_cursor sess (6, 0);
  send sess "\x04" (* C-d accepts even from insert mode *);
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed ".b") then begin
    incr failures;
    Printf.printf "FAIL vim: printed args %S\n" printed
  end

let test_vim_scroll () =
  let sess =
    spawn "vim-scroll"
      (Printf.sprintf "seq 1 50 | %s --debounce 0.05 --vim cat" (quote cud))
  in
  expect_status sess "1-22/50";
  send sess "\x1b";
  expect_status sess "NORMAL";
  send sess "j";
  expect_status sess "2-23/50";
  send sess "k";
  expect_status sess "1-22/50";
  send sess "G";
  expect_status sess "29-50/50";
  send sess "gg";
  expect_status sess "1-22/50";
  send sess "\x03" (* C-c cancels *);
  expect_exit sess 130

let test_no_command () =
  let sess =
    spawn "no-command"
      (Printf.sprintf "printf %%s %s | %s --debounce 0.05" (quote json)
         (quote cud))
  in
  (* no fixed command: bare prompt, a note instead of output *)
  expect_row sess 0 ">";
  expect_row sess 1 "(type a command to run)";
  expect_cursor sess (2, 0);
  send sess "jq .a";
  expect_row sess 0 "> jq .a";
  expect_row sess 1 "1";
  expect_status sess "exit 0";
  send sess (ctrl 'd');
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "jq .a") then begin
    incr failures;
    Printf.printf "FAIL no-command: printed args %S\n" printed
  end

let test_no_stdin () =
  (* stdin is the terminal itself (nothing piped): the child must get
     immediate EOF instead of hanging on our tty *)
  let sess =
    spawn "no-stdin" (Printf.sprintf "%s --debounce 0.05 cat" (quote cud))
  in
  expect_row sess 1 "(no output)";
  expect_status sess "exit 0";
  send sess (ctrl 'c');
  expect_exit sess 130

let test_accept_exit_code () =
  (* accepting while the last run failed propagates its exit code *)
  let sess =
    spawn "accept-exit-code"
      (Printf.sprintf "printf '' | %s -e --debounce 0.05 -- sh -c 'exit 7'"
         (quote cud))
  in
  expect_status sess "exit 7";
  expect_row sess 1 "(no output)";
  send sess enter (* -e: accept *);
  expect_exit sess 7;
  (* C-d goes through the same path: a succeeding command still exits 0 *)
  let sess =
    spawn "accept-exit-zero"
      (Printf.sprintf "printf '' | %s --debounce 0.05 -- true" (quote cud))
  in
  expect_status sess "exit 0";
  send sess (ctrl 'd');
  expect_exit sess 0

let test_edit_fixed () =
  let sess =
    spawn "edit-fixed"
      (Printf.sprintf "printf '' | %s --debounce 0.05 -- echo AA BB"
         (quote cud))
  in
  expect_row sess 0 "echo AA BB>";
  expect_row sess 1 "AA BB";
  expect_cursor sess (12, 0);
  (* Shift-Tab moves the cursor into the fixed-args region (end of "AA BB") *)
  send sess backtab;
  expect_cursor sess (10, 0);
  expect_status sess "[fixed]";
  send sess " CC";
  expect_row sess 0 "echo AA BB CC>";
  expect_row sess 1 "AA BB CC";
  (* editing keys work there: C-w kills the last fixed word *)
  send sess (ctrl 'w');
  expect_row sess 1 "AA BB";
  (* Tab returns to the args field *)
  send sess "\t";
  send sess "x";
  expect_row sess 1 "AA BB x";
  (* edits work anywhere inside the fixed args: Shift-Tab in, then move
     with plain motions (Left/Right never switch fields) *)
  send sess backtab;
  expect_cursor sess (11, 0) (* C-w left "AA BB " with a trailing space *);
  send sess left;
  send sess left;
  send sess "\x7f" (* backspace inside the fixed args: deletes a 'B' *);
  expect_row sess 0 "echo AA B > x";
  expect_row sess 1 "AA B x";
  send sess end_;
  expect_cursor sess (10, 0) (* still in the fixed args *);
  send sess (ctrl 'c');
  expect_exit sess 130

let test_output_on_exit () =
  let sess =
    spawn "output-on-exit"
      (Printf.sprintf "printf %%s %s | %s --debounce 0.05 -o -i .b jq"
         (quote json) (quote cud))
  in
  expect_row sess 1 "[";
  expect_status sess "1-5/5";
  send sess (ctrl 'd');
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "  2," && contains printed "]") then begin
    incr failures;
    Printf.printf "FAIL output-on-exit: printed %S\n" printed
  end

let test_enter_accept () =
  let sess =
    spawn "enter-accept"
      (Printf.sprintf "printf '' | %s --debounce 0.05 -e -- echo hi"
         (quote cud))
  in
  expect_row sess 1 "hi";
  send sess "there";
  expect_row sess 1 "hi there";
  send sess enter;
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "there") then begin
    incr failures;
    Printf.printf "FAIL enter-accept: printed %S\n" printed
  end

let test_cancel_silent () =
  let sess =
    spawn "cancel-silent"
      (Printf.sprintf "printf '' | %s --debounce 0.05 -i hello -- echo"
         (quote cud))
  in
  expect_row sess 0 "echo> hello";
  expect_row sess 1 "hello";
  send sess (ctrl 'c');
  expect_exit sess 130;
  let printed = printed_after_release sess in
  if contains printed "hello" then begin
    incr failures;
    Printf.printf "FAIL cancel-silent: printed %S on cancel\n" printed
  end

let test_placeholder () =
  let sess =
    spawn "placeholder"
      (Printf.sprintf "printf '' | %s --debounce 0.05 -I{} -- echo A {} B"
         (quote cud))
  in
  (* empty line: the placeholder argument disappears *)
  expect_row sess 1 "A B";
  expect_status sess "exit 0";
  (* the editable args are spliced where {} sits, not appended *)
  send sess "x y";
  expect_row sess 1 "A x y B";
  send sess (ctrl 'd');
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "x y") then begin
    incr failures;
    Printf.printf "FAIL placeholder: printed args %S\n" printed
  end

let test_ansi () =
  (* --ansi: SGR sequences pass through to the terminal; the VT emulator
     ignores SGR, so the row reads as the bare text *)
  let sess =
    spawn "ansi"
      (Printf.sprintf
         "printf '\\033[31mRED\\033[0m plain' | %s --ansi --debounce 0.05 cat"
         (quote cud))
  in
  expect_row sess 0 "cat>";
  expect_row sess 1 "RED plain";
  expect_status sess "exit 0";
  (* Alt-A toggles ANSI rendering off (escapes sanitized) and back on *)
  send sess "\x1ba";
  expect_row sess 1 "?[31mRED?[0m plain";
  expect_status sess "exit 0" (* no re-run, just re-render *);
  send sess "\x1ba";
  expect_row sess 1 "RED plain";
  send sess (ctrl 'c');
  expect_exit sess 130

let test_no_ansi () =
  (* without --ansi the escape bytes are sanitized to '?' *)
  let sess =
    spawn "no-ansi"
      (Printf.sprintf
         "printf '\\033[31mRED\\033[0m plain' | %s --debounce 0.05 cat"
         (quote cud))
  in
  expect_row sess 1 "?[31mRED?[0m plain";
  send sess (ctrl 'c');
  expect_exit sess 130

let test_multiline () =
  (* a 30-element array so the output is taller than the viewport *)
  let big_json =
    Printf.sprintf {|{"a":{"b":[%s]}}|}
      (String.concat "," (List.init 30 (fun i -> string_of_int (i + 1))))
  in
  let sess =
    spawn "multiline"
      (Printf.sprintf "printf %%s %s | %s -M -1 --debounce 0.05 jq"
         (quote big_json) (quote cud))
  in
  expect_row sess 0 "jq>";
  send sess ".a";
  expect_row sess 0 "jq> .a";
  (* plain Enter inserts a line break; the input grows to two rows and the
     output shifts down by one *)
  send sess enter;
  send sess "|.b";
  expect_row sess 0 "jq> .a";
  expect_row sess 1 "  |.b";
  expect_cursor sess (5, 1) (* two columns of indent + 3 chars *);
  expect_row sess 2 "[";
  expect_status sess "exit 0";
  expect_status sess "1-21/32" (* 24 rows - 2 input rows - status bar *);

  (* Up moves the cursor onto the first input line (column preserved as far
     as it fits: col 3 clamps to ".a"'s length) *)
  send sess up;
  expect_cursor sess (6, 0);
  send sess down;
  expect_cursor sess (4, 1) (* the clamped column sticks *);
  send sess end_;
  expect_cursor sess (5, 1);

  (* Shift-Tab reaches the fixed command; Tab heads back toward the output *)
  send sess backtab;
  expect_cursor sess (2, 0) (* end of the editable command "jq" *);
  send sess "\t" (* fixed -> args *);
  expect_cursor sess (5, 1);
  send sess "\t" (* args -> output *);
  (* output focused: the cursor is hidden, the status bar says so, Up/Down
     scroll, the input stays put *)
  expect_cursor_hidden sess;
  expect_status sess "[output]";
  send sess down;
  expect_status sess "2-22/32";
  expect_row sess 1 "  |.b";
  send sess pgdn;
  expect_status sess "12-32/32";
  send sess up;
  expect_status sess "11-31/32";

  (* Shift-Tab returns to the args; typing edits again *)
  send sess backtab;
  expect_cursor sess (5, 1);
  send sess "%";
  expect_row sess 1 "  |.b%";
  expect_status sess "exit 3" (* broken filter *);
  send sess "\x7f" (* backspace *);
  expect_row sess 1 "  |.b";
  expect_status sess "exit 0";

  (* Ctrl+Down / Shift+Down scroll the output without leaving the input *)
  send sess "\x1b[1;5B" (* Ctrl+Down *);
  expect_status sess "2-22/32";
  send sess "\x1b[1;2B" (* Shift+Down *);
  expect_status sess "3-23/32";
  send sess "\x1b[1;5A" (* Ctrl+Up *);
  expect_status sess "2-22/32";
  expect_cursor sess (5, 1) (* the input cursor did not move *);
  send sess "\x1b[1;2A";
  expect_status sess "1-21/32";

  (* Enter again: a third line *)
  send sess enter;
  expect_cursor sess (2, 2);
  expect_status sess "1-20/32" (* three input rows now *);

  send sess (ctrl 'c');
  expect_exit sess 130

let test_multiline_enter_accept () =
  (* priority: in multiline mode Enter is a line break even with -e; the
     accept keys are Alt-Enter (or C-o) *)
  let sess =
    spawn "ml-enter-accept"
      (Printf.sprintf "printf '' | %s -M -e --debounce 0.05 -- echo hi"
         (quote cud))
  in
  expect_row sess 1 "hi";
  send sess "x";
  expect_row sess 0 "echo hi> x";
  send sess enter (* newline, NOT accept *);
  expect_cursor sess (2, 1);
  send sess "y";
  expect_row sess 1 "  y";
  send sess "\x1b\r" (* Alt-Enter: accept *);
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "x y" || contains printed "x") then begin
    incr failures;
    Printf.printf "FAIL ml-enter-accept: printed %S\n" printed
  end
let test_ctrl_o_submit () =
  (* C-o (0x0F) submits — accepts with -e — from the args field... *)
  let sess =
    spawn "ctrl-o-args"
      (Printf.sprintf "printf '' | %s -M -e --debounce 0.05 -- echo hi"
         (quote cud))
  in
  expect_row sess 1 "hi";
  send sess "x";
  expect_row sess 0 "echo hi> x";
  send sess "\x0f";
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "x") then begin
    incr failures;
    Printf.printf "FAIL ctrl-o-args: printed %S\n" printed
  end;
  (* ...and with the output focused *)
  let sess =
    spawn "ctrl-o-output"
      (Printf.sprintf "printf '' | %s -M -e --debounce 0.05 -- echo hi"
         (quote cud))
  in
  expect_row sess 1 "hi";
  send sess "x";
  expect_row sess 0 "echo hi> x";
  send sess "\t" (* args -> output *);
  send sess "\x0f";
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "x") then begin
    incr failures;
    Printf.printf "FAIL ctrl-o-output: printed %S\n" printed
  end

let test_tab_output_single_line () =
  (* the output is focusable outside multiline mode too *)
  let sess =
    spawn "tab-output"
      (Printf.sprintf "seq 1 50 | %s --debounce 0.05 cat" (quote cud))
  in
  expect_status sess "1-22/50";
  send sess "\t";
  expect_status sess "[output]";
  expect_cursor_hidden sess;
  send sess down;
  expect_status sess "2-23/50";
  (* typing is ignored with the output focused *)
  send sess "x";
  pump sess ~wait:0.2;
  expect_row sess 0 "cat>";
  (* Shift-Tab returns to the args, twice more reaches the command *)
  send sess backtab;
  expect_cursor sess (5, 0);
  send sess "y";
  expect_row sess 0 "cat> y";
  send sess backtab;
  expect_status sess "[fixed]";
  expect_cursor sess (3, 0);
  send sess (ctrl 'c');
  expect_exit sess 130

let test_pipe () =
  let sess =
    spawn "pipe"
      (Printf.sprintf
         "printf 'foo\\nbar\\nboo\\n' | %s --debounce 0.05 -p -c -i o -i 'a-z \
          A-Z' -- grep tr"
         (quote cud))
  in
  (* one prompt row per step, then the pipeline's output *)
  expect_row sess 0 "grep> o";
  expect_row sess 1 "tr> a-z A-Z";
  expect_row sess 2 "FOO";
  expect_row sess 3 "BOO";
  expect_status sess "exit 0";
  expect_status sess "[1/2]";
  expect_cursor sess (7, 0);
  (* C-n focuses the next step *)
  send sess (ctrl 'n');
  expect_status sess "[2/2]";
  expect_cursor sess (11, 1);
  (* edits go to that step; the whole pipeline re-runs *)
  send sess (ctrl 'u');
  send sess "o 0";
  expect_row sess 2 "f00";
  expect_row sess 3 "b00";
  (* C-p returns to the first step *)
  send sess (ctrl 'p');
  expect_status sess "[1/2]";
  expect_cursor sess (7, 0);
  (* -c prints the whole pipeline on accept *)
  send sess (ctrl 'd');
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "grep o | tr o 0") then begin
    incr failures;
    Printf.printf "FAIL pipe: printed %S\n" printed
  end

let test_pipe_args_output () =
  (* the default quoted output keeps the step structure: one line per step *)
  let sess =
    spawn "pipe-args"
      (Printf.sprintf
         "printf 'x\\n' | %s --debounce 0.05 -p -i 'a b' -i 'a A' -- echo tr"
         (quote cud))
  in
  expect_row sess 0 "echo> a b";
  expect_row sess 1 "tr> a A";
  expect_row sess 2 "A b";
  send sess (ctrl 'd');
  expect_exit sess 0;
  let printed = printed_after_release sess in
  if not (contains printed "a b\r\na A") then begin
    incr failures;
    Printf.printf "FAIL pipe-args: printed %S\n" printed
  end

let test_pipefail () =
  (* -P: a failing first step fails the pipeline, and accepting propagates
     its exit code *)
  let sess =
    spawn "pipefail"
      (Printf.sprintf "printf 'x\\n' | %s --debounce 0.05 -p -P -- false cat"
         (quote cud))
  in
  expect_row sess 0 "false>";
  expect_row sess 1 "cat>";
  expect_status sess "exit 1";
  send sess (ctrl 'd');
  expect_exit sess 1;
  (* without -P the shell reports the last step's exit code *)
  let sess =
    spawn "pipefail-off"
      (Printf.sprintf "printf 'x\\n' | %s --debounce 0.05 -p -- false cat"
         (quote cud))
  in
  expect_status sess "exit 0";
  send sess (ctrl 'd');
  expect_exit sess 0

let test_lens () =
  let sess =
    spawn "lens"
      (Printf.sprintf
         "printf %%s %s | %s --debounce 0.05 --lens 'head -1' -i . jq"
         (quote json) (quote cud))
  in
  expect_row sess 0 "jq> .";
  (* left pane: the lens header and its output; right pane: the usual
     output, shifted past the separator column *)
  expect_right_contains sess 1 "head -1";
  expect_right sess 2 "{";
  expect_left sess 1 "{";
  expect_left_contains sess 2 {|"a": 1,|};
  expect_status sess "exit 0";
  expect_status sess "1-8/8" (* scrolling geometry is unchanged *);
  send sess (ctrl 'c');
  expect_exit sess 130

let test_hint () =
  let sess =
    spawn "hint"
      (Printf.sprintf
         "printf %%s %s | %s --debounce 0.05 --lens 'head -1' --hint \
          'printf %%s \"$CUD_BEFORE\"' jq"
         (quote json) (quote cud))
  in
  (* two sections of 11 rows each: lens header on row 1, hint header on
     row 12, hint output from row 13 *)
  expect_right_contains sess 1 "head -1";
  expect_right_contains sess 12 "CUD_BEFORE";
  (* typing re-runs the hint with the text before the cursor *)
  send sess ".b";
  expect_row sess 0 "jq> .b";
  expect_right sess 13 ".b";
  expect_left_contains sess 1 "[" (* the main output also re-ran *);
  expect_right sess 2 "[" (* ... and so did the lens *);
  (* pure cursor motion re-runs the hint too: CUD_BEFORE shrinks *)
  send sess left;
  expect_right sess 13 ".";
  send sess (ctrl 'c');
  expect_exit sess 130

let () =
  let tests =
    [
      ("jq-flow", test_jq_flow);
      ("scroll-resize-cancel", test_scroll_resize_cancel);
      ("manual-mode", test_manual_mode);
      ("fixed-args-lines", test_fixed_args_and_lines_output);
      ("null-output", test_null_output);
      ("command-output", test_command_output);
      ("vim-mode", test_vim_mode);
      ("vim-scroll", test_vim_scroll);
      ("no-command", test_no_command);
      ("no-stdin", test_no_stdin);
      ("placeholder", test_placeholder);
      ("cancel-silent", test_cancel_silent);
      ("accept-exit-code", test_accept_exit_code);
      ("enter-accept", test_enter_accept);
      ("output-on-exit", test_output_on_exit);
      ("edit-fixed", test_edit_fixed);
      ("tab-output", test_tab_output_single_line);
      ("ansi", test_ansi);
      ("no-ansi", test_no_ansi);
      ("multiline", test_multiline);
      ("ml-enter-accept", test_multiline_enter_accept);
      ("ctrl-o-submit", test_ctrl_o_submit);
      ("pipe", test_pipe);
      ("pipe-args", test_pipe_args_output);
      ("pipefail", test_pipefail);
      ("lens", test_lens);
      ("hint", test_hint);
    ]
  in
  List.iter
    (fun (name, f) ->
      let before = !failures in
      (try f ()
       with exn ->
         incr failures;
         Printf.printf "FAIL %s: exception %s\n" name (Printexc.to_string exn));
      if !failures = before then Printf.printf "PASS %s\n" name)
    tests;
  if !failures > 0 then begin
    Printf.printf "%d e2e failure(s)\n" !failures;
    exit 1
  end
  else print_endline "all e2e tests passed"
