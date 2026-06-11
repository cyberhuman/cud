(** Terminal shell: Notty rendering and the Lwt event loop around the pure
    {!Model}. Keyboard input and drawing go through [/dev/tty], so stdin
    stays free for the data being piped in. *)

module Term = Notty_lwt.Term

type opts = {
  cmd : string option;  (** [None]: the input line is the whole command *)
  fixed_args : string list;
  placeholder : string option;
      (** xargs -I style substitution point in [fixed_args] *)
  initial : string;
  auto : bool;  (** re-run automatically after edits *)
  debounce : float;  (** seconds to wait after the last edit *)
  single : bool;  (** start in single-argument mode *)
  vim : bool;  (** vim keybindings *)
}

type result = {
  accepted : bool;  (** Ctrl-D (true) vs Escape/Ctrl-C (false) *)
  args : string list;  (** the arguments in the editor when the UI closed *)
  command : string;  (** the whole command, shell-quoted *)
}

type msg =
  | Event of [ Notty.Unescape.event | `Resize of int * int ]
  | Run_done of int * Runner.outcome
  | Debounce of int
  | Events_closed

let ( let* ) = Lwt.bind

let read_all_stdin () =
  if Unix.isatty Unix.stdin then None
  else Some (In_channel.input_all In_channel.stdin)

let attr_of_style =
  let open Notty.A in
  function
  | Render.Prompt -> fg cyan ++ st bold
  | Render.Input -> empty
  | Render.Out_text -> empty
  | Render.Err_text -> fg red
  | Render.Info_text -> fg (gray 10)
  | Render.Bar -> st reverse
  | Render.Bar_alert -> fg red ++ st reverse ++ st bold
  | Render.Bar_mode -> fg yellow ++ st reverse ++ st bold

let image_of_frame (frame : Render.frame) =
  let open Notty in
  frame.rows
  |> List.map (fun row ->
         row
         |> List.map (fun (style, text) -> I.string (attr_of_style style) text)
         |> I.hcat)
  |> I.vcat

let input_of_event : Notty.Unescape.event -> Model.input option = function
  | `Key (`ASCII c, mods) when List.mem `Ctrl mods ->
      Some (Model.I_ctrl (Char.lowercase_ascii c))
  | `Key (`ASCII c, mods) when List.mem `Meta mods ->
      Some (Model.I_meta (Char.lowercase_ascii c))
  | `Key (`ASCII c, _) -> Some (Model.I_char (Uchar.of_char c))
  | `Key (`Uchar u, mods) when not (List.mem `Ctrl mods) ->
      Some (Model.I_char u)
  | `Key (`Enter, _) -> Some (Model.I_special Model.S_enter)
  | `Key (`Backspace, _) -> Some (Model.I_special Model.S_backspace)
  | `Key (`Delete, _) -> Some (Model.I_special Model.S_delete)
  | `Key (`Arrow `Left, mods) when List.mem `Ctrl mods || List.mem `Meta mods
    ->
      Some (Model.I_special Model.S_ctrl_left)
  | `Key (`Arrow `Right, mods) when List.mem `Ctrl mods || List.mem `Meta mods
    ->
      Some (Model.I_special Model.S_ctrl_right)
  | `Key (`Arrow `Left, _) -> Some (Model.I_special Model.S_left)
  | `Key (`Arrow `Right, _) -> Some (Model.I_special Model.S_right)
  | `Key (`Arrow `Up, _) -> Some (Model.I_special Model.S_up)
  | `Key (`Arrow `Down, _) -> Some (Model.I_special Model.S_down)
  | `Key (`Home, _) -> Some (Model.I_special Model.S_home)
  | `Key (`End, _) -> Some (Model.I_special Model.S_end)
  | `Key (`Page `Up, _) -> Some (Model.I_special Model.S_pgup)
  | `Key (`Page `Down, _) -> Some (Model.I_special Model.S_pgdn)
  | `Key (`Escape, _) -> Some (Model.I_special Model.S_escape)
  | _ -> None

let run (opts : opts) : result Lwt.t =
  let input_data = read_all_stdin () in
  let tty_unix = Unix.openfile "/dev/tty" [ Unix.O_RDWR ] 0 in
  let tty = Lwt_unix.of_unix_file_descr tty_unix in
  let term = Term.create ~input:tty ~output:tty () in
  let msgs, push_opt = Lwt_stream.create () in
  let push m = push_opt (Some m) in
  Lwt.async (fun () ->
      let* () = Lwt_stream.iter (fun e -> push (Event e)) (Term.events term) in
      push Events_closed;
      Lwt.return_unit);

  let draw model =
    let w, h = Term.size term in
    let frame = Render.render ~w ~h model in
    let* () = Term.image term (image_of_frame frame) in
    Term.cursor term frame.cursor
  in

  let schedule_debounce (model : Model.t) =
    if opts.auto then
      let seq = model.edit_seq in
      Lwt.async (fun () ->
          let* () = Lwt_unix.sleep opts.debounce in
          push (Debounce seq);
          Lwt.return_unit)
  in

  let start_run (model : Model.t) proc =
    match Model.command model with
    | Error _ -> (model, proc) (* shown in the status bar; keep the old run *)
    | Ok None ->
        (* nothing to run: kill any in-flight run, show a note *)
        Option.iter (fun (p : Runner.handle) -> p.terminate ()) proc;
        (Model.set_idle model ~note:"(type a command to run)", None)
    | Ok (Some (prog, args)) ->
        Option.iter (fun (p : Runner.handle) -> p.terminate ()) proc;
        let model = Model.start_run model in
        let gen = model.gen in
        let handle = Runner.start ~cmd:prog ~args ~input:input_data in
        Lwt.async (fun () ->
            let* outcome = handle.outcome in
            push (Run_done (gen, outcome));
            Lwt.return_unit);
        (model, Some handle)
  in

  let apply_effects model proc effects =
    List.fold_left
      (fun (model, proc) effect_ ->
        match effect_ with
        | Model.Schedule_rerun ->
            schedule_debounce model;
            (model, proc)
        | Model.Start_run -> start_run model proc)
      (model, proc) effects
  in

  let finish proc ~accepted model =
    Option.iter (fun (p : Runner.handle) -> p.terminate ()) proc;
    let* () = Term.release term in
    Lwt.return
      {
        accepted;
        args = Model.user_args model;
        command = Model.command_string model;
      }
  in

  let rec loop (model : Model.t) proc =
    let* () = draw model in
    let* msg = Lwt_stream.next msgs in
    match msg with
    | Events_closed -> finish proc ~accepted:false model
    | Event (`Resize _) -> loop model proc
    | Event (`Mouse _ | `Paste _) -> loop model proc
    | Event (`Key _ as event) -> (
        match input_of_event event with
        | None -> loop model proc
        | Some input -> (
            let view_h = max 1 (snd (Term.size term) - 2) in
            match Model.handle_input ~view_h model input with
            | Model.Quit_exit -> finish proc ~accepted:false model
            | Model.Accept_exit -> finish proc ~accepted:true model
            | Model.Continue (model, effects) ->
                let model, proc = apply_effects model proc effects in
                loop model proc))
    | Run_done (gen, { lines; status }) ->
        loop (Model.finish_run model ~gen ~lines ~status) proc
    | Debounce seq ->
        (* Only the debounce of the latest edit triggers; an in-flight run is
           superseded (terminated) by [start_run]. *)
        if opts.auto && seq = model.edit_seq then
          let model, proc = start_run model proc in
          loop model proc
        else loop model proc
  in

  let model =
    Model.create ?cmd:opts.cmd ?placeholder:opts.placeholder
      ~single:opts.single ~vim:opts.vim
      ~fixed_args:opts.fixed_args ~initial:opts.initial ()
  in
  (* Run once at startup so the output area is populated immediately. *)
  let model, proc = start_run model None in
  Lwt.catch
    (fun () -> loop model proc)
    (fun exn ->
      let* _ = finish proc ~accepted:false model in
      Lwt.fail exn)
