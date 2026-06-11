open Ine_lib

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
      check "model.args-error" (Result.is_error (Model.args m'))
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
  check "model.single-args" (Model.args s = Ok [ "-r"; ".x | .y" ]);
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
  check "model.single-empty-no-arg" (Model.args empty_single = Ok []);
  (match Model.handle_key ~view_h:5 s Model.Toggle_single with
  | Model.Continue (s', [ Model.Start_run ]) ->
      check "model.toggle-off" (not s'.Model.single);
      check "model.toggle-args-split"
        (Model.args s' = Ok [ "-r"; ".x"; "|"; ".y" ])
  | _ -> fail "model.toggle" "Toggle_single should request Start_run");

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

  (* horizontal scrolling keeps the cursor on screen *)
  let long =
    Model.create ~cmd:"jq" ~fixed_args:[] ~initial:"abcdefghij" ()
  in
  let frame = Render.render ~w:10 ~h:3 long in
  check_str "render.hscroll" "jq> fghij "
    (List.nth (Render.to_strings frame) 0);
  check "render.hscroll-cursor" (frame.Render.cursor = Some (9, 0));

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
  let outcome = run "sh" [ "-c"; "cat; echo oops >&2" ] "hello\nworld\n" in
  let texts =
    Array.to_list outcome.Runner.lines |> List.map (fun l -> l.Model.text)
  in
  check "runner.stdout-replayed" (List.mem "hello" texts && List.mem "world" texts);
  check "runner.stderr-captured"
    (Array.exists
       (fun l -> l.Model.kind = Err && l.Model.text = "oops")
       outcome.Runner.lines);
  check "runner.exit0" (outcome.Runner.status = Model.Exited 0);

  let outcome = run "sh" [ "-c"; "exit 3" ] "ignored input" in
  check "runner.exit3" (outcome.Runner.status = Model.Exited 3);

  (* a command that never reads its input must not deadlock, and large
     input must not deadlock the writer while output is pending *)
  let big = String.concat "" (List.init 20000 (fun i -> Printf.sprintf "%d\n" i)) in
  let outcome = run "cat" [] big in
  check_int "runner.big-roundtrip" 20000 (Array.length outcome.Runner.lines);

  let handle = Runner.start ~cmd:"sleep" ~args:[ "100" ] ~input:"" in
  handle.Runner.terminate ();
  let outcome = Lwt_main.run handle.Runner.outcome in
  check "runner.terminated"
    (match outcome.Runner.status with Model.Signaled _ -> true | _ -> false)

let () =
  editor_tests ();
  shellwords_tests ();
  model_tests ();
  render_tests ();
  render_invariants ();
  runner_tests ();
  if !failures > 0 then begin
    Printf.printf "%d failure(s)\n" !failures;
    exit 1
  end
  else print_endline "all unit tests passed"
