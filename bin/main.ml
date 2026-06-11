let print_args mode args command =
  match mode with
  | `Quiet -> ()
  | `Quoted ->
      print_endline
        (String.concat " " (List.map Ine_lib.Shellwords.quote_word args))
  | `Lines -> List.iter print_endline args
  | `Null ->
      List.iter
        (fun arg ->
          print_string arg;
          print_char '\000')
        args
  | `Command -> print_endline command

let run ~initial ~manual ~debounce ~single ~output cmdline =
  match cmdline with
  | [] -> 2 (* unreachable: the positional argument list is non-empty *)
  | cmd :: fixed_args -> (
      (* The command may exit without reading the input we replay to it. *)
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      let opts =
        {
          Ine_lib.Tui.cmd;
          fixed_args;
          initial;
          auto = not manual;
          debounce;
          single;
        }
      in
      match Lwt_main.run (Ine_lib.Tui.run opts) with
      | { Ine_lib.Tui.accepted; args; command } ->
          print_args output args command;
          if accepted then 0 else 130
      | exception Unix.Unix_error (err, fn, _) ->
          Printf.eprintf "ine: %s: %s\n" fn (Unix.error_message err);
          1)

open Cmdliner

let initial =
  Arg.(
    value & opt string ""
    & info [ "i"; "initial" ] ~docv:"TEXT"
        ~doc:"Initial contents of the argument line.")

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
        ])

let cmdline =
  Arg.(
    non_empty & pos_all string []
    & info [] ~docv:"CMD"
        ~doc:
          "Command to run, with optional fixed arguments. Put $(b,--) before \
           it if it starts with a dash.")

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
      `P "Example: $(b,swaymsg -t get_outputs | ine jq)";
      `P
        "On exit the arguments from the editor are printed to stdout \
         (shell-quoted by default; see $(b,--quiet), $(b,--lines), \
         $(b,--null)).";
      `S Manpage.s_exit_status;
      `P "0 when accepted with Ctrl-D, 130 when cancelled with Escape or \
          Ctrl-C.";
    ]
  in
  let term =
    let open Term.Syntax in
    let+ initial
    and+ manual
    and+ debounce
    and+ single
    and+ output
    and+ cmdline in
    run ~initial ~manual ~debounce ~single ~output cmdline
  in
  Cmd.v (Cmd.info "ine" ~doc ~man) term

let () = exit (Cmd.eval' cmd)
