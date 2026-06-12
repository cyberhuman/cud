open Cud_lib

let failures = ref 0

let fail name fmt =
  incr failures;
  Printf.printf "FAIL %s: " name;
  Printf.kfprintf (fun _ -> print_newline ()) stdout fmt

let check name cond = if not cond then fail name "condition is false"

let check_str name expected actual =
  if expected <> actual then
    fail name "expected %S, got %S" expected actual

let check_int name expected actual =
  if expected <> actual then fail name "expected %d, got %d" expected actual

let check_rows name expected actual =
  if expected <> actual then begin
    let show rows =
      String.concat "\n" (List.map (Printf.sprintf "|%s|") rows)
    in
    fail name "frame mismatch\n--- expected:\n%s\n--- actual:\n%s" (show expected)
      (show actual)
  end

(* --- Editor --- *)

let editor_tests () =
  let ed = Editor.of_string "hello world" in
  check_int "editor.cursor-at-end" 11 (Editor.cursor ed);
  check_str "editor.roundtrip" "hello world" (Editor.to_string ed);

  let ed0 = Editor.home ed in
  check_int "editor.home" 0 (Editor.cursor ed0);
  check_int "editor.word_right" 5 (Editor.cursor (Editor.word_right ed0));
  check_int "editor.word_right2" 11
    (Editor.cursor (Editor.word_right (Editor.word_right ed0)));
  check_int "editor.word_left" 6 (Editor.cursor (Editor.word_left ed));
  check_int "editor.end" 11 (Editor.cursor (Editor.end_ ed0));

  check_str "editor.kill_prev_word" "hello "
    (Editor.to_string (Editor.kill_prev_word ed));
  check_str "editor.kill_to_start" "" (Editor.to_string (Editor.kill_to_start ed));
  check_str "editor.kill_to_end-noop" "hello world"
    (Editor.to_string (Editor.kill_to_end ed));
  let mid = Editor.left (Editor.left ed) in
  check_str "editor.kill_to_end" "hello wor"
    (Editor.to_string (Editor.kill_to_end mid));

  let ed = Editor.left (Editor.of_string "ab") in
  check_int "editor.left" 1 (Editor.cursor ed);
  check_str "editor.backspace-mid" "b"
    (Editor.to_string (Editor.backspace ed));
  check_str "editor.delete-mid" "a" (Editor.to_string (Editor.delete ed));
  check_str "editor.insert-mid" "axb"
    (Editor.to_string (Editor.insert (Uchar.of_char 'x') ed));

  (* no-ops return the value physically unchanged *)
  let e = Editor.of_string "x" in
  check "editor.right-noop" (Editor.right e == e);
  check "editor.backspace-empty-noop"
    (Editor.backspace Editor.empty == Editor.empty);
  let h = Editor.home e in
  check "editor.left-at-home-noop" (Editor.left h == h);

  (* unicode: 3 characters, cursor counts characters not bytes *)
  let u = Editor.of_string "λλλ" in
  check_int "editor.unicode-cursor" 3 (Editor.cursor u);
  check_str "editor.unicode-backspace" "λλ"
    (Editor.to_string (Editor.backspace u))

(* --- Shellwords --- *)

let shellwords_tests () =
  let ok name expected input =
    match Shellwords.split input with
    | Ok words ->
        if words <> expected then
          fail name "split %S = [%s]" input (String.concat "; " words)
    | Error e -> fail name "split %S = error %s" input (Shellwords.error_message e)
  in
  let err name expected input =
    match Shellwords.split input with
    | Ok words -> fail name "split %S = Ok [%s]" input (String.concat "; " words)
    | Error e -> if e <> expected then fail name "wrong error for %S" input
  in
  ok "sw.empty" [] "";
  ok "sw.blank" [] "   ";
  ok "sw.simple" [ ".a" ] ".a";
  ok "sw.multi" [ "a"; "b"; "c" ] "a  b\tc";
  ok "sw.single-quote" [ "-r"; ".[] | .name" ] "-r '.[] | .name'";
  ok "sw.double-quote" [ "x \" y" ] "\"x \\\" y\"";
  ok "sw.double-backslash" [ "a\\b" ] "\"a\\\\b\"";
  ok "sw.backslash-space" [ "a b" ] "a\\ b";
  ok "sw.empty-word" [ "" ] "''";
  ok "sw.adjacent" [ "ab" ] "'a'\"b\"";
  err "sw.unterminated-single" Shellwords.Unterminated_single_quote "'";
  err "sw.unterminated-double" Shellwords.Unterminated_double_quote "\"ab";
  err "sw.trailing-backslash" Shellwords.Trailing_backslash "x\\";

  check_str "sw.quote-plain" "abc" (Shellwords.quote_word "abc");
  check_str "sw.quote-space" "'a b'" (Shellwords.quote_word "a b");
  check_str "sw.join" "jq -r '.[] | .name'"
    (Shellwords.join_command [ "jq"; "-r"; ".[] | .name" ])

(* --- Model --- *)

let model () = Model.create ~cmd:"jq" ~fixed_args:[] ~initial:"" ()

let with_lines n m =
  {
    m with
    Model.lines =
      Array.init n (fun i -> { Model.kind = Out; text = Printf.sprintf "line%d" i });
  }

let model_tests () =
  let m = model () in
  check "model.initial-no-parse-error" (m.Model.parse_error = None);

  (match Model.handle_key ~view_h:5 m (Model.Insert (Uchar.of_char 'x')) with
  | Model.Continue (m', effects) ->
      check_int "model.insert-bumps-edit_seq" 1 m'.Model.edit_seq;
      check "model.insert-schedules-rerun" (effects = [ Model.Schedule_rerun ]);
      check_str "model.insert-text" "x" (Editor.to_string m'.Model.editor)
  | _ -> fail "model.insert" "unexpected reaction");

  (* motions and impossible edits don't schedule re-runs *)
  (match Model.handle_key ~view_h:5 m Model.Left with
  | Model.Continue (m', []) -> check_int "model.motion-seq" 0 m'.Model.edit_seq
  | _ -> fail "model.motion" "motion produced effects");
  (match Model.handle_key ~view_h:5 m Model.Backspace with
  | Model.Continue (m', []) ->
      check_int "model.noop-edit-seq" 0 m'.Model.edit_seq
  | _ -> fail "model.noop-edit" "no-op edit produced effects");

  (match Model.handle_key ~view_h:5 m Model.Enter with
  | Model.Continue (_, [ Model.Start_run ]) -> ()
  | _ -> fail "model.enter" "Enter should request Start_run");
  let ea =
    Model.create ~enter_accept:true ~cmd:"jq" ~fixed_args:[] ~initial:"" ()
  in
  check "model.enter-accept"
    (Model.handle_key ~view_h:5 ea Model.Enter = Model.Accept_exit);
  check "model.accept" (Model.handle_key ~view_h:5 m Model.Accept = Model.Accept_exit);
  check "model.quit" (Model.handle_key ~view_h:5 m Model.Quit = Model.Quit_exit);

  (* scrolling clamps to the line count *)
  let m30 = with_lines 30 m in
  let scroll_of = function
    | Model.Continue (m, []) -> m.Model.scroll
    | _ -> -1
  in
  check_int "model.scroll-up-clamp" 0 (scroll_of (Model.handle_key ~view_h:5 m30 Model.Scroll_up));
  check_int "model.scroll-down" 1 (scroll_of (Model.handle_key ~view_h:5 m30 Model.Scroll_down));
  check_int "model.page-down" 5 (scroll_of (Model.handle_key ~view_h:5 m30 Model.Page_down));
  let bottom = { m30 with Model.scroll = 100 } in
  check_int "model.page-down-clamp" 25
    (scroll_of (Model.handle_key ~view_h:5 bottom Model.Page_down));

  (* parse errors are tracked while typing *)
  (match Model.handle_key ~view_h:5 m (Model.Insert (Uchar.of_char '\'')) with
  | Model.Continue (m', _) ->
      check "model.parse-error"
        (m'.Model.parse_error = Some Shellwords.Unterminated_single_quote);
      check "model.args-error" (Result.is_error (Model.command m'))
  | _ -> fail "model.parse-error" "unexpected reaction");

  (* run lifecycle: stale generations are ignored, empty output gets a note *)
  let running = Model.start_run m in
  check "model.running" running.Model.running;
  check_int "model.gen" 1 running.Model.gen;
  let stale =
    Model.finish_run running ~gen:0
      ~lines:[| { Model.kind = Out; text = "old" } |]
      ~status:(Model.Exited 1)
  in
  check "model.stale-run-ignored" (stale = running);
  let finished =
    Model.finish_run running ~gen:1 ~lines:[||] ~status:(Model.Exited 0)
  in
  check "model.finished" (not finished.Model.running);
  check "model.finished-status" (finished.Model.status = Some (Model.Exited 0));
  check_str "model.no-output-note" "(no output)" finished.Model.lines.(0).Model.text;

  (* single-argument mode: line passed verbatim, no parse errors *)
  let s =
    Model.create ~single:true ~cmd:"jq" ~fixed_args:[ "-r" ]
      ~initial:".x | .y" ()
  in
  check "model.single-args"
    (Model.command s = Ok (Some ("jq", [ "-r"; ".x | .y" ])));
  check "model.single-user-args" (Model.user_args s = [ ".x | .y" ]);
  check_str "model.single-command_string" "jq -r '.x | .y'"
    (Model.command_string s);
  check "model.single-no-parse-error"
    ({ s with Model.editor = Editor.of_string "'" }
     |> fun s ->
     Model.parse_error_of ~single:true (Editor.to_string s.Model.editor)
     = None);
  let empty_single =
    Model.create ~single:true ~cmd:"jq" ~fixed_args:[] ~initial:"" ()
  in
  check "model.single-empty-no-arg"
    (Model.command empty_single = Ok (Some ("jq", [])));
  (match Model.handle_key ~view_h:5 s Model.Toggle_single with
  | Model.Continue (s', [ Model.Start_run ]) ->
      check "model.toggle-off" (not s'.Model.single);
      check "model.toggle-args-split"
        (Model.command s' = Ok (Some ("jq", [ "-r"; ".x"; "|"; ".y" ])))
  | _ -> fail "model.toggle" "Toggle_single should request Start_run");

  (* -I placeholder: splice in place, inline substitution, append fallback *)
  let ph =
    Model.create ~cmd:"echo" ~placeholder:"{}"
      ~fixed_args:[ "A"; "{}"; "B" ] ~initial:"x y" ()
  in
  check "model.ph-splice"
    (Model.command ph = Ok (Some ("echo", [ "A"; "x"; "y"; "B" ])));
  let ph_empty = { ph with Model.editor = Editor.of_string "" } in
  check "model.ph-empty-line"
    (Model.command ph_empty = Ok (Some ("echo", [ "A"; "B" ])));
  let inline =
    Model.create ~cmd:"rg" ~placeholder:"@" ~fixed_args:[ "--glob"; "*.@" ]
      ~initial:"ml" ()
  in
  check "model.ph-inline"
    (Model.command inline = Ok (Some ("rg", [ "--glob"; "*.ml" ])));
  let absent =
    Model.create ~cmd:"jq" ~placeholder:"{}" ~fixed_args:[ "-r" ]
      ~initial:".a" ()
  in
  check "model.ph-absent-appends"
    (Model.command absent = Ok (Some ("jq", [ "-r"; ".a" ])));
  let ph_single =
    Model.create ~cmd:"jq" ~placeholder:"{}" ~single:true
      ~fixed_args:[ "{}"; "-c" ] ~initial:".x | .y" ()
  in
  check "model.ph-single"
    (Model.command ph_single = Ok (Some ("jq", [ ".x | .y"; "-c" ])));

  (* editable fixed args: Tab switches focus, edits re-run with new fixed *)
  let mf = Model.create ~cmd:"echo" ~fixed_args:[ "AA"; "BB" ] ~initial:"x" () in
  check "model.focus-starts-args" (mf.Model.focus = Model.F_args);
  (match Model.handle_key ~view_h:5 mf Model.Toggle_focus with
  | Model.Continue (mf, []) -> (
      check "model.focus-fixed" (mf.Model.focus = Model.F_fixed);
      match
        Model.handle_key ~view_h:5 mf (Model.Insert (Uchar.of_char 'C'))
      with
      | Model.Continue (mf, [ Model.Schedule_rerun ]) -> (
          check_str "model.fixed-edited" "AA BBC" (Model.fixed_text mf);
          check "model.fixed-command"
            (Model.command mf = Ok (Some ("echo", [ "AA"; "BBC"; "x" ])));
          match
            Model.handle_key ~view_h:5 mf (Model.Insert (Uchar.of_char '\''))
          with
          | Model.Continue (mf, _) ->
              check "model.fixed-parse-error" (mf.Model.parse_error <> None)
          | _ -> fail "model.fixed-parse" "unexpected reaction")
      | _ -> fail "model.fixed-edit" "unexpected reaction")
  | _ -> fail "model.focus" "Tab should continue");
  (* without a fixed command Tab is a no-op *)
  let nc2 = Model.create ~fixed_args:[] ~initial:"" () in
  (match Model.handle_key ~view_h:5 nc2 Model.Toggle_focus with
  | Model.Continue (m, []) ->
      check "model.focus-nocmd" (m.Model.focus = Model.F_args)
  | _ -> fail "model.focus-nocmd" "unexpected reaction");

  (* the status bar shows when the fixed args are being edited *)
  let contains hay needle =
    let nl = String.length needle and hl = String.length hay in
    let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
    nl = 0 || go 0
  in
  let mfoc = Model.create ~cmd:"echo" ~fixed_args:[ "hi" ] ~initial:"" () in
  let bar m = List.nth (Render.to_strings (Render.render ~w:60 ~h:4 m)) 3 in
  check "model.no-fixed-indicator" (not (contains (bar mfoc) "[fixed]"));
  (match Model.handle_key ~view_h:5 mfoc Model.Toggle_focus with
  | Model.Continue (m, _) ->
      check "model.fixed-indicator" (contains (bar m) "[fixed]")
  | _ -> fail "model.fixed-indicator" "unexpected reaction");

  (* Left/Right never switch fields: motions stop at the field edges *)
  let bx = Model.create ~cmd:"echo" ~fixed_args:[ "AA" ] ~initial:"x" () in
  let bx =
    match Model.handle_key ~view_h:5 bx Model.Home with
    | Model.Continue (m, _) -> m
    | _ -> bx
  in
  (match Model.handle_key ~view_h:5 bx Model.Left with
  | Model.Continue (m, []) ->
      check "model.left-stays-in-args" (m.Model.focus = Model.F_args);
      check_int "model.left-at-edge" 0 (Editor.cursor m.Model.editor)
  | _ -> fail "model.left-edge" "unexpected reaction");

  (* no fixed command: the line itself is the command *)
  let nc = Model.create ~fixed_args:[] ~initial:"" () in
  check "model.nocmd-empty" (Model.command nc = Ok None);
  let nc = { nc with Model.editor = Editor.of_string "jq .a" } in
  check "model.nocmd-split" (Model.command nc = Ok (Some ("jq", [ ".a" ])));
  let ncs = Model.create ~single:true ~fixed_args:[] ~initial:"jq .a | cat" () in
  check "model.nocmd-single-sh"
    (Model.command ncs = Ok (Some ("sh", [ "-c"; "jq .a | cat" ])));

  (* user_args in split mode: parsed words; unparseable line passed as-is *)
  let m = Model.create ~cmd:"jq" ~fixed_args:[] ~initial:"-r '.a'" () in
  check "model.user-args-split" (Model.user_args m = [ "-r"; ".a" ]);
  let bad = { m with Model.editor = Editor.of_string "'oops" } in
  check "model.user-args-bad" (Model.user_args bad = [ "'oops" ]);

  (* command_string *)
  let m =
    Model.create ~cmd:"jq" ~fixed_args:[ "-r" ] ~initial:".x | .y" ()
  in
  check_str "model.command_string" "jq -r .x '|' .y" (Model.command_string m);
  let m = Model.create ~cmd:"jq" ~fixed_args:[] ~initial:"'.x | .y'" () in
  check_str "model.command_string-quoted" "jq '.x | .y'" (Model.command_string m)

(* --- Vim mode --- *)

let vim_tests () =
  let esc = Model.I_special Model.S_escape in
  let chars s =
    List.init (String.length s) (fun i -> Model.I_char (Uchar.of_char s.[i]))
  in
  let apply m inputs =
    List.fold_left
      (fun m i ->
        match Model.handle_input ~view_h:5 m i with
        | Model.Continue (m', _) -> m'
        | _ -> m)
      m inputs
  in
  let mk initial =
    Model.create ~vim:true ~cmd:"jq" ~fixed_args:[] ~initial ()
  in
  let text m = Editor.to_string m.Model.editor in
  let cur m = Editor.cursor m.Model.editor in

  (* starts in insert mode; Esc enters normal and steps back one column *)
  let m = mk "hello world" in
  check "vim.starts-insert" (m.Model.vmode = Model.V_insert);
  let n = apply m [ esc ] in
  check "vim.esc-normal" (n.Model.vmode = Model.V_normal);
  check_int "vim.esc-cursor" 10 (cur n);

  (* motions *)
  check_int "vim.0" 0 (cur (apply n (chars "0")));
  check_int "vim.dollar" 10 (cur (apply n (chars "0$")));
  check_int "vim.h-clamp" 0 (cur (apply n (chars "0h")));
  check_int "vim.l" 1 (cur (apply n (chars "0l")));
  check_int "vim.w" 6 (cur (apply n (chars "0w")));
  check_int "vim.e" 4 (cur (apply n (chars "0e")));
  check_int "vim.b" 0 (cur (apply n (chars "0wb")));
  check_int "vim.f" 4 (cur (apply n (chars "0fo;"))) (* ';' ignored *);
  check_int "vim.F" 7 (cur (apply n (chars "$Fo")));

  (* x deletes under cursor into the register *)
  let d = apply n (chars "0x") in
  check_str "vim.x" "ello world" (text d);
  check_str "vim.x-register" "h" d.Model.register;

  (* undo / redo *)
  let u = apply d (chars "u") in
  check_str "vim.undo" "hello world" (text u);
  let r = apply u [ Model.I_ctrl 'r' ] in
  check_str "vim.redo" "ello world" (text r);

  (* operator + motion *)
  let dw = apply n (chars "0dw") in
  check_str "vim.dw" "world" (text dw);
  check_str "vim.dw-register" "hello " dw.Model.register;
  let p = apply dw (chars "P") in
  check_str "vim.P" "hello world" (text p);
  check_int "vim.P-cursor" 5 (cur p);
  let cw = apply n (chars "0cw") in
  check_str "vim.cw" " world" (text cw);
  check_str "vim.cw-register" "hello" cw.Model.register;
  check "vim.cw-insert" (cw.Model.vmode = Model.V_insert);
  let dfo = apply n (chars "0dfo") in
  check_str "vim.dfo" " world" (text dfo);
  let dd = apply n (chars "dd") in
  check_str "vim.dd" "" (text dd);
  check_str "vim.dd-register" "hello world" dd.Model.register;
  let pp = apply dd (chars "p") in
  check_str "vim.p-empty" "hello world" (text pp);

  (* r, D, C, s, X *)
  check_str "vim.r" "Xello world" (text (apply n (chars "0rX")));
  let big_d = apply n (chars "0wD") in
  check_str "vim.D" "hello " (text big_d);
  check_int "vim.D-cursor" 5 (cur big_d);
  let c = apply n (chars "0wC") in
  check "vim.C-insert" (c.Model.vmode = Model.V_insert);
  check_str "vim.C" "hello " (text c);
  check_str "vim.X" "ello world" (text (apply n (chars "0lX")));
  let s = apply n (chars "0s") in
  check_str "vim.s" "ello world" (text s);
  check "vim.s-insert" (s.Model.vmode = Model.V_insert);

  (* insert transitions *)
  let a = apply n (chars "0a") in
  check "vim.a-insert" (a.Model.vmode = Model.V_insert);
  check_int "vim.a-cursor" 1 (cur a);
  check_int "vim.A-cursor" 11 (cur (apply n (chars "A")));
  check_int "vim.I-cursor" 0 (cur (apply n (chars "$I")));
  let edited = apply n (chars "A!") in
  check_str "vim.A-type" "hello world!" (text edited);

  (* scrolling in normal mode *)
  let n30 = with_lines 30 (apply (mk "") [ esc ]) in
  check_int "vim.j" 1 (apply n30 (chars "j")).Model.scroll;
  check_int "vim.G" 25 (apply n30 (chars "G")).Model.scroll;
  check_int "vim.gg" 0 (apply n30 (chars "Ggg")).Model.scroll;
  check_int "vim.k" 0 (apply n30 (chars "jk")).Model.scroll;

  (* exits: Esc never quits in vim mode; C-c and C-d still do *)
  check "vim.esc-no-quit"
    (match Model.handle_input ~view_h:5 n esc with
    | Model.Continue _ -> true
    | _ -> false);
  check "vim.ctrl-c-quit"
    (Model.handle_input ~view_h:5 n (Model.I_ctrl 'c') = Model.Quit_exit);
  check "vim.ctrl-d-accept"
    (Model.handle_input ~view_h:5 n (Model.I_ctrl 'd') = Model.Accept_exit);
  check "vim.normal-enter-runs"
    (match Model.handle_input ~view_h:5 n (Model.I_special Model.S_enter) with
    | Model.Continue (_, [ Model.Start_run ]) -> true
    | _ -> false);

  (* edits schedule re-runs *)
  check "vim.x-schedules-rerun"
    (match Model.handle_input ~view_h:5 n (Model.I_char (Uchar.of_char 'x')) with
    | Model.Continue (_, [ Model.Schedule_rerun ]) -> true
    | _ -> false);

  (* without --vim, Escape still quits *)
  let plain = Model.create ~cmd:"jq" ~fixed_args:[] ~initial:"" () in
  check "vim.disabled-esc-quits"
    (Model.handle_input ~view_h:5 plain esc = Model.Quit_exit)

(* --- Render --- *)

let render_tests () =
  (* canonical layout: input on top, output viewport, status bar at the
     bottom *)
  let m =
    {
      (Model.create ~cmd:"jq" ~fixed_args:[] ~initial:".a" ()) with
      Model.lines =
        [|
          { Model.kind = Out; text = "{" };
          { Model.kind = Out; text = "  \"a\": 1" };
          { Model.kind = Out; text = "}" };
        |];
      status = Some (Model.Exited 0);
    }
  in
  let frame = Render.render ~w:30 ~h:6 m in
  check_rows "render.basic"
    [
      "jq> .a                        ";
      "{                             ";
      "  \"a\": 1                      ";
      "}                             ";
      "                              ";
      " exit 0                1-3/3  ";
    ]
    (Render.to_strings frame);
  check "render.cursor" (frame.Render.cursor = Some (6, 0));

  (* scrolled view *)
  let m30 = { (with_lines 30 m) with Model.scroll = 7 } in
  let frame = Render.render ~w:30 ~h:6 m30 in
  let rows = Render.to_strings frame in
  check_str "render.scrolled-first-line" "line7"
    (String.trim (List.nth rows 1));
  check_str "render.scrolled-range" "8-11/30"
    (String.trim (List.nth rows 5) |> fun s ->
     String.sub s (String.length s - 7) 7);

  (* the prompt shows the fixed args, shell-quoted *)
  let mf =
    Model.create ~cmd:"jq" ~fixed_args:[ "-r"; ".x | .y" ] ~initial:"" ()
  in
  let frame = Render.render ~w:30 ~h:3 mf in
  check_str "render.fixed-args-prompt" "jq -r '.x | .y'>"
    (String.trim (List.nth (Render.to_strings frame) 0));
  check "render.fixed-args-cursor" (frame.Render.cursor = Some (17, 0));

  (* horizontal scrolling keeps the cursor on screen *)
  let long =
    Model.create ~cmd:"jq" ~fixed_args:[] ~initial:"abcdefghij" ()
  in
  let frame = Render.render ~w:10 ~h:3 long in
  check_str "render.hscroll" "bcdefghij "
    (List.nth (Render.to_strings frame) 0);
  check "render.hscroll-cursor" (frame.Render.cursor = Some (9, 0));
  (* cursor in the fixed-args region *)
  let mf2 =
    Model.create ~cmd:"echo" ~fixed_args:[ "AA" ] ~initial:"zz" ()
  in
  let mf2 =
    match Model.handle_key ~view_h:5 mf2 Model.Toggle_focus with
    | Model.Continue (m, _) -> m
    | _ -> mf2
  in
  let frame = Render.render ~w:20 ~h:3 mf2 in
  check_str "render.fixed-focus-row" "echo AA> zz"
    (String.trim (List.nth (Render.to_strings frame) 0));
  check "render.fixed-focus-cursor" (frame.Render.cursor = Some (7, 0));

  (* running indicator *)
  let contains hay needle =
    let nl = String.length needle and hl = String.length hay in
    let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
    go 0
  in
  let frame = Render.render ~w:30 ~h:3 (Model.start_run m) in
  let bar = List.nth (Render.to_strings frame) 2 in
  check "render.running-text" (contains bar "running");

  (* single-argument mode indicator *)
  let s = Model.create ~single:true ~cmd:"jq" ~fixed_args:[] ~initial:"" () in
  let bar = List.nth (Render.to_strings (Render.render ~w:40 ~h:3 s)) 2 in
  check "render.single-indicator" (contains bar "[1 arg]");
  let bar = List.nth (Render.to_strings (Render.render ~w:40 ~h:3 m)) 2 in
  check "render.no-single-indicator" (not (contains bar "[1 arg]"))

(* --- ANSI SGR parsing (--ansi) --- *)

let ansi_toggle_tests () =
  let has hay needle =
    let nl = String.length needle and hl = String.length hay in
    let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
    nl = 0 || go 0
  in
  let m = Model.create ~cmd:"jq" ~fixed_args:[] ~initial:"" () in
  let bar m = List.nth (Render.to_strings (Render.render ~w:60 ~h:4 m)) 3 in
  check "ansi.toggle-off-default" (not m.Model.ansi);
  (match Model.handle_input ~view_h:5 m (Model.I_meta 'a') with
  | Model.Continue (m', []) ->
      check "ansi.toggle-on" m'.Model.ansi;
      check "ansi.toggle-indicator" (has (bar m') "[ansi]");
      (match Model.handle_input ~view_h:5 m' (Model.I_meta 'a') with
      | Model.Continue (m'', []) ->
          check "ansi.toggle-back-off" (not m''.Model.ansi);
          check "ansi.toggle-indicator-gone" (not (has (bar m'') "[ansi]"))
      | _ -> fail "ansi.toggle-back" "unexpected reaction")
  | _ -> fail "ansi.toggle" "unexpected reaction")

let ansi_tests () =
  let dflt = Render.ansi_default in
  let red = { dflt with Render.fg = Some (Render.Idx 1) } in
  let check_segs name expected input =
    let actual = Render.ansi_segments input in
    if actual <> expected then begin
      let show segs =
        String.concat "; "
          (List.map (fun (_, s) -> Printf.sprintf "%S" s) segs)
      in
      fail name "segments mismatch on %S: got [%s] (%d segs, expected %d)"
        input (show actual) (List.length actual) (List.length expected)
    end
  in
  (* plain text: one default segment *)
  check_segs "ansi.plain" [ (dflt, "hello") ] "hello";
  (* basic color and reset mid-line *)
  check_segs "ansi.basic-color"
    [ (red, "RED"); (dflt, " plain") ]
    "\x1b[31mRED\x1b[0m plain";
  (* state carries across segments within the line *)
  check_segs "ansi.carry"
    [ (red, "a"); ({ red with Render.bold = true }, "b") ]
    "\x1b[31ma\x1b[1mb";
  (* 256-color and truecolor *)
  check_segs "ansi.256"
    [ ({ dflt with Render.fg = Some (Render.Idx 196) }, "x") ]
    "\x1b[38;5;196mx";
  check_segs "ansi.256-bg"
    [ ({ dflt with Render.bg = Some (Render.Idx 22) }, "x") ]
    "\x1b[48;5;22mx";
  check_segs "ansi.truecolor"
    [ ({ dflt with Render.fg = Some (Render.Rgb (12, 34, 56)) }, "x") ]
    "\x1b[38;2;12;34;56mx";
  (* bold + reverse, then attribute-specific resets *)
  check_segs "ansi.bold-reverse"
    [
      ({ dflt with Render.bold = true; reverse = true }, "a");
      ({ dflt with Render.reverse = true }, "b");
      (dflt, "c");
    ]
    "\x1b[1;7ma\x1b[22mb\x1b[27mc";
  (* bright colors and 39/49 defaults *)
  check_segs "ansi.bright"
    [
      ({ dflt with Render.fg = Some (Render.Idx 12) }, "a");
      (dflt, "b");
    ]
    "\x1b[94ma\x1b[39mb";
  (* unknown CSI sequences are stripped without touching attributes *)
  check_segs "ansi.unknown-csi" [ (dflt, "ab") ] "a\x1b[2Kb";
  check_segs "ansi.cursor-csi" [ (dflt, "ab") ] "a\x1b[10;20Hb";
  (* OSC stripped, both terminators; 2-char escapes stripped *)
  check_segs "ansi.osc-bel" [ (dflt, "ab") ] "a\x1b]0;title\x07b";
  check_segs "ansi.osc-st" [ (dflt, "ab") ] "a\x1b]8;;http://x\x1b\\b";
  check_segs "ansi.two-char-esc" [ (dflt, "ab") ] "a\x1b=b";
  (* truncated escape at end of line is dropped *)
  check_segs "ansi.truncated" [ (dflt, "a") ] "a\x1b[31";
  (* tabs expand to 8-column stops, other control chars become '?' *)
  check_segs "ansi.tab" [ (dflt, "ab      cd") ] "ab\tcd";
  check_segs "ansi.tab-after-color"
    [ (red, "ab      c") ]
    "\x1b[31mab\tc";
  check_segs "ansi.control" [ (dflt, "a?b") ] "a\x01b";
  (* empty sequence = reset; lone "m" *)
  check_segs "ansi.empty-reset" [ (red, "a"); (dflt, "b") ] "\x1b[31ma\x1b[mb";

  (* rendering: --ansi off keeps the sanitize path, on splits segments *)
  let lines = [| { Model.kind = Out; text = "\x1b[31mRED\x1b[0m plain" } |] in
  let plain =
    {
      (Model.create ~cmd:"cat" ~fixed_args:[] ~initial:"" ()) with
      Model.lines;
    }
  in
  let colored =
    {
      (Model.create ~ansi:true ~cmd:"cat" ~fixed_args:[] ~initial:"" ()) with
      Model.lines;
    }
  in
  let row1 m = List.nth (Render.render ~w:20 ~h:3 m).Render.rows 1 in
  check_str "ansi.render-off" "?[31mRED?[0m plain  "
    (Render.row_text (row1 plain));
  check_str "ansi.render-on" "RED plain           "
    (Render.row_text (row1 colored));
  (match row1 colored with
  | (Render.Ansi a, "RED") :: (Render.Out_text, _) :: _ ->
      check "ansi.render-red" (a.Render.fg = Some (Render.Idx 1))
  | _ -> fail "ansi.render-segments" "unexpected segment structure");
  (* stderr without SGR stays Err_text in ansi mode *)
  let err =
    {
      colored with
      Model.lines = [| { Model.kind = Err; text = "oops" } |];
    }
  in
  check "ansi.render-err"
    (List.for_all (fun (st, _) -> st = Render.Err_text) (row1 err));
  (* truncated colored line still fits exactly *)
  let long =
    {
      colored with
      Model.lines =
        [| { Model.kind = Out; text = "\x1b[32m" ^ String.make 50 'g' } |];
    }
  in
  check_int "ansi.render-crop" 20 (Render.ulength (Render.row_text (row1 long)))

let render_invariants () =
  let base = Model.create ~cmd:"jq" ~fixed_args:[] ~initial:"" () in
  let states =
    [
      base;
      Model.create ~cmd:"some-long-command-name" ~fixed_args:[]
        ~initial:"with a fairly long argument line that overflows" ();
      { (with_lines 50 base) with Model.scroll = 1000 };
      Model.start_run (with_lines 3 base);
      (match
         Model.handle_key ~view_h:5 base (Model.Insert (Uchar.of_char '\''))
       with
      | Model.Continue (m, _) -> m
      | _ -> base);
      {
        base with
        Model.lines =
          [|
            { Model.kind = Err; text = "tab\there" };
            { Model.kind = Info; text = "ctrl\007char" };
            { Model.kind = Out; text = String.make 200 'x' };
          |];
        status = Some (Model.Exited 3);
      };
      {
        (Model.create ~ansi:true ~cmd:"cat" ~fixed_args:[] ~initial:"" ()) with
        Model.lines =
          [|
            { Model.kind = Out; text = "\x1b[31mRED\x1b[0m plain" };
            { Model.kind = Out; text = "\x1b[1;38;5;196m" ^ String.make 120 'c' };
            { Model.kind = Out; text = "\x1b[38;2;1;2;3mtrue\x1b[48;2;9;9;9mcolor" };
            { Model.kind = Err; text = "a\x1b]0;t\x07b\tc\x1b[31" };
            { Model.kind = Out; text = "\x1b[7m\x1b[4m" };
          |];
        status = Some (Model.Exited 0);
      };
    ]
  in
  List.iteri
    (fun si m ->
      for w = 1 to 45 do
        for h = 1 to 8 do
          let frame = Render.render ~w ~h m in
          let rows = Render.to_strings frame in
          if List.length rows <> h then
            fail "render.inv-rows" "state %d %dx%d: %d rows" si w h
              (List.length rows);
          List.iteri
            (fun ri row ->
              if Render.ulength row <> w then
                fail "render.inv-width" "state %d %dx%d row %d: width %d" si w
                  h ri (Render.ulength row))
            rows;
          match frame.Render.cursor with
          | Some (x, y) ->
              if not (x >= 0 && x < w && y = 0) then
                fail "render.inv-cursor" "state %d %dx%d: cursor %d,%d" si w h
                  x y
          | None -> fail "render.inv-cursor-none" "state %d %dx%d" si w h
        done
      done)
    states

(* --- Runner (real processes) --- *)

let runner_tests () =
  let run cmd args input =
    Lwt_main.run (Runner.start ~cmd ~args ~input).Runner.outcome
  in
  let outcome =
    run "sh" [ "-c"; "cat; echo oops >&2" ] (Some "hello\nworld\n")
  in
  let texts =
    Array.to_list outcome.Runner.lines |> List.map (fun l -> l.Model.text)
  in
  check "runner.stdout-replayed" (List.mem "hello" texts && List.mem "world" texts);
  check "runner.stderr-captured"
    (Array.exists
       (fun l -> l.Model.kind = Err && l.Model.text = "oops")
       outcome.Runner.lines);
  check "runner.exit0" (outcome.Runner.status = Model.Exited 0);

  let outcome = run "sh" [ "-c"; "exit 3" ] (Some "ignored input") in
  check "runner.exit3" (outcome.Runner.status = Model.Exited 3);

  (* no piped stdin: the child sees immediate EOF, it can never hang *)
  let outcome = run "cat" [] None in
  check_str "runner.no-stdin-eof" "(0 lines)"
    (Printf.sprintf "(%d lines)" (Array.length outcome.Runner.lines));
  check "runner.no-stdin-exit0" (outcome.Runner.status = Model.Exited 0);

  (* a command that never reads its input must not deadlock, and large
     input must not deadlock the writer while output is pending *)
  let big = String.concat "" (List.init 20000 (fun i -> Printf.sprintf "%d\n" i)) in
  let outcome = run "cat" [] (Some big) in
  check_int "runner.big-roundtrip" 20000 (Array.length outcome.Runner.lines);

  let handle = Runner.start ~cmd:"sleep" ~args:[ "100" ] ~input:None in
  handle.Runner.terminate ();
  let outcome = Lwt_main.run handle.Runner.outcome in
  check "runner.terminated"
    (match outcome.Runner.status with Model.Signaled _ -> true | _ -> false)

let () =
  editor_tests ();
  shellwords_tests ();
  model_tests ();
  vim_tests ();
  render_tests ();
  ansi_tests ();
  ansi_toggle_tests ();
  render_invariants ();
  runner_tests ();
  if !failures > 0 then begin
    Printf.printf "%d failure(s)\n" !failures;
    exit 1
  end
  else print_endline "all unit tests passed"
