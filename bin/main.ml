(* [args]: one list per pipeline step (a single one without --pipe). The
   quoted default keeps the step structure, one line per step; the plain
   formats flatten it. *)
let print_args mode args command out_lines =
  match mode with
  | `Quiet -> ()
  | `Quoted ->
      List.iter
        (fun step ->
          print_endline
            (String.concat " " (List.map Cud_lib.Shellwords.quote_word step)))
        args
  | `Lines -> List.iter print_endline (List.concat args)
  | `Null ->
      List.iter
        (fun arg ->
          print_string arg;
          print_char '\000')
        (List.concat args)
  | `Command -> print_endline command
  | `Output -> List.iter print_endline out_lines

let run ~initials ~manual ~debounce ~single ~vim ~enter_accept ~ansi ~wrap
    ~multiline ~lenses ~hints ~placeholder ~pipe ~pipefail ~output cmdline =
  if (not pipe) && List.length initials > 1 then begin
    prerr_endline "cud: --initial repeated: only meaningful with --pipe";
    124
  end
  else if pipefail && not pipe then begin
    prerr_endline "cud: --pipefail: only meaningful with --pipe";
    124
  end
  else begin
  let cmd, fixed_args =
    match cmdline with [] -> (None, []) | cmd :: rest -> (Some cmd, rest)
  in
  (* The command may exit without reading the input we replay to it. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let opts =
    {
      Cud_lib.Tui.cmd;
      fixed_args;
      placeholder;
      initials;
      pipe;
      pipefail;
      auto = not manual;
      debounce;
      single;
      vim;
      enter_accept;
      ansi;
      wrap;
      multiline;
      lenses;
      hints;
    }
  in
  match Lwt_main.run (Cud_lib.Tui.run opts) with
  | { Cud_lib.Tui.accepted; status; args; command; output = out_lines } ->
      if accepted then print_args output args command out_lines;
      if not accepted then 130
      else (
        (* a failing command propagates its exit code through the accept *)
        match status with
        | Some (Cud_lib.Model.Exited n) -> n
        | Some (Cud_lib.Model.Signaled s) -> 128 + s
        | None -> 0)
  | exception Unix.Unix_error (err, fn, _) ->
      Printf.eprintf "cud: %s: %s\n" fn (Unix.error_message err);
      1
  end

open Cmdliner

let initials =
  Arg.(
    value & opt_all string []
    & info [ "i"; "initial" ] ~docv:"TEXT"
        ~doc:
          "Initial contents of the argument line. With $(b,--pipe), repeat \
           to give each step its initial arguments, in order.")

let pipe =
  Arg.(
    value & flag
    & info [ "p"; "pipe" ]
        ~doc:
          "Build a shell pipeline: every positional argument is one step's \
           command line (quote steps that have fixed arguments), each with \
           its own prompt and editable arguments, stacked at the top. The \
           last step starts focused; switch between steps with \
           Ctrl-P/Ctrl-N (vim normal mode: J/K); Up/Down (vim: gj/gk) also \
           cross into the neighbouring step, keeping the on-screen cursor \
           column. A step left empty drops out of the pipeline.")

let pipefail =
  Arg.(
    value & flag
    & info [ "P"; "pipefail" ]
        ~doc:
          "With $(b,--pipe), the pipeline fails if any step fails ($(b,set \
           -o pipefail)) instead of reporting only the last step's exit \
           code.")

let manual =
  Arg.(
    value & flag
    & info [ "m"; "manual" ]
        ~doc:"Do not re-run automatically after edits; re-run only on Enter.")

let debounce =
  Arg.(
    value & opt float 0.3
    & info [ "debounce" ] ~docv:"SECONDS"
        ~doc:"Delay after the last edit before an automatic re-run.")

let single =
  Arg.(
    value & flag
    & info [ "1"; "single" ]
        ~doc:
          "Pass the whole argument line as a single argument instead of \
           splitting it into words. Toggle at runtime with Ctrl-T.")

let output =
  Arg.(
    value
    & vflag `Quoted
        [
          ( `Quiet,
            info [ "q"; "quiet" ]
              ~doc:"Do not print the final arguments on exit." );
          ( `Lines,
            info [ "l"; "lines" ]
              ~doc:"Print the final arguments one per line, unquoted." );
          ( `Null,
            info [ "0"; "null" ]
              ~doc:
                "Print the final arguments separated by NUL bytes, for \
                 $(b,xargs -0)." );
          ( `Command,
            info [ "c"; "command" ]
              ~doc:
                "Print the whole command, including the fixed arguments, \
                 shell-quoted and ready to execute." );
          ( `Output,
            info [ "o"; "output" ]
              ~doc:
                "Print the command's output instead of the arguments." );
        ])

let vim =
  Arg.(
    value & flag
    & info [ "vim" ]
        ~doc:
          "Vim keybindings: Escape switches between insert and normal mode \
           (motions, d/c operators, x, r, p, u, j/k scrolling, ...).")

let enter_accept =
  Arg.(
    value & flag
    & info [ "e"; "enter-accept" ]
        ~doc:"Enter accepts the arguments and exits (like Ctrl-D) instead of re-running.")

let lenses =
  Arg.(
    value & opt_all string []
    & info [ "lens" ] ~docv:"CMD"
        ~doc:
          "Run $(docv) with $(b,sh -c) over the main command's output and \
           show the result in a pane left of the output, re-run after every \
           main run. Repeatable. Example: $(b,--lens 'jq keys').")

let hints =
  Arg.(
    value & opt_all string []
    & info [ "hint" ] ~docv:"CMD"
        ~doc:
          "Like $(b,--lens), but also re-run (debounced) whenever the input \
           line or the cursor moves, with the context in the environment: \
           $(b,CUD_BEFORE)/$(b,CUD_AFTER) (the args text before/after the \
           cursor), $(b,CUD_FIXED) (the fixed-args line) and $(b,CUD_CMD) \
           (the command name). Repeatable; hints are shown below the \
           lenses.")

let multiline =
  Arg.(
    value & flag
    & info [ "M"; "multiline" ]
        ~doc:
          "Multi-line argument editor: Enter inserts a line break (the \
           accept/re-run keys become Alt-Enter or Ctrl-O), and Up/Down move \
           the cursor across lines while Ctrl/Shift+Up/Down scroll the \
           output.")

let ansi =
  Arg.(
    value & flag
    & info [ "A"; "ansi" ]
        ~doc:
          "Respect ANSI SGR sequences (colors, bold, ...) in the command's \
           output instead of stripping them. Other escape sequences are \
           still removed. Toggle at runtime with Alt-A.")

let wrap =
  Arg.(
    value & flag
    & info [ "w"; "wrap" ]
        ~doc:
          "Wrap long output lines onto continuation rows instead of \
           cropping them at the screen edge. Toggle at runtime with Alt-W.")

let placeholder =
  Arg.(
    value & opt (some string) None
    & info [ "I"; "placeholder" ] ~docv:"STR"
        ~doc:
          "Replace $(docv) in the fixed arguments with the editable \
           arguments, like $(b,xargs -I): a fixed argument that is exactly \
           $(docv) is replaced by the arguments themselves, one that merely \
           contains $(docv) gets them substituted as text. If $(docv) does \
           not occur, the arguments are appended as usual.")

let cmdline =
  Arg.(
    value & pos_all string []
    & info [] ~docv:"CMD"
        ~doc:
          "Command to run, with optional fixed arguments. Put $(b,--) before \
           it if it starts with a dash. With no command at all, the input \
           line itself is the command line (run with $(b,sh -c) in \
           single-argument mode). With $(b,--pipe), each $(docv) is one \
           pipeline step.")

let cmd =
  let doc = "interactively edit a command's arguments and re-run it" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) captures its standard input, then runs $(i,CMD) in a \
         full-screen UI: the top line edits extra arguments (appended after \
         the fixed ones), the rest of the screen shows the command's output. \
         Every run is fed the captured input again.";
      `P "Example: $(b,swaymsg -t get_outputs | cud jq)";
      `P
        "With $(b,--pipe) the positional arguments form a shell pipeline, \
         one editable step per line: $(b,ps aux | cud -p -- 'grep ssh' 'wc \
         -l').";
      `P
        "Tab moves the focus toward the output, Shift-Tab toward the \
         (editable) fixed command line; the output scrolls with Up/Down \
         while focused.";
      `P
        "On exit the arguments from the editor are printed to stdout \
         (shell-quoted by default; see $(b,--quiet), $(b,--lines), \
         $(b,--null)).";
      `S Manpage.s_exit_status;
      `P
        "On accept (Ctrl-D), the last run's exit code: 0 on success, the \
         command's code when nonzero, 128+N when it died of signal N. 130 \
         when cancelled with Escape or Ctrl-C.";
    ]
  in
  let term =
    let open Term.Syntax in
    let+ initials
    and+ manual
    and+ debounce
    and+ single
    and+ vim
    and+ enter_accept
    and+ ansi
    and+ wrap
    and+ multiline
    and+ lenses
    and+ hints
    and+ placeholder
    and+ pipe
    and+ pipefail
    and+ output
    and+ cmdline in
    run ~initials ~manual ~debounce ~single ~vim ~enter_accept ~ansi ~wrap
      ~multiline ~lenses ~hints ~placeholder ~pipe ~pipefail ~output cmdline
  in
  Cmd.v (Cmd.info "cud" ~doc ~man) term

let () = exit (Cmd.eval' cmd)
