(** Terminal shell: Notty rendering and the Eio event loop around the pure
    {!Model}. Keyboard input and drawing go through [/dev/tty], so stdin
    stays free for the data being piped in.

    Notty has no Eio backend, so we use [Notty_unix.Term] (plain blocking IO)
    and bridge it ourselves: a dedicated system thread blocks in
    [Term.event]/[select] and pushes events into an {!Eio.Stream} that the
    main fiber consumes, alongside run-completion and debounce messages.
    [SIGWINCH] is forwarded through a self-pipe so the event thread wakes up
    no matter which thread the signal handler ran on. *)

module Term = Notty_unix.Term

type opts = {
  cmd : string;
  fixed_args : string list;
  initial : string;
  auto : bool;  (** re-run automatically after edits *)
  debounce : float;  (** seconds to wait after the last edit *)
  single : bool;  (** start in single-argument mode *)
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

let read_all_stdin () =
  if Unix.isatty Unix.stdin then "" else In_channel.input_all In_channel.stdin

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

let image_of_frame (frame : Render.frame) =
  let open Notty in
  frame.rows
  |> List.map (fun row ->
         row
         |> List.map (fun (style, text) -> I.string (attr_of_style style) text)
         |> I.hcat)
  |> I.vcat

let key_of_event : Notty.Unescape.event -> Model.key option = function
  | `Key (`ASCII c, mods) when List.mem `Ctrl mods -> (
      match Char.lowercase_ascii c with
      | 'a' -> Some Model.Home
      | 'e' -> Some Model.End
      | 'b' -> Some Model.Left
      | 'f' -> Some Model.Right
      | 'k' -> Some Model.Kill_to_end
      | 'u' -> Some Model.Kill_to_start
      | 'w' -> Some Model.Kill_prev_word
      | 'p' -> Some Model.Scroll_up
      | 'n' -> Some Model.Scroll_down
      | 'l' -> Some Model.Redraw
      | 't' -> Some Model.Toggle_single
      | 'd' -> Some Model.Accept
      | 'c' -> Some Model.Quit
      | _ -> None)
  | `Key (`ASCII c, mods) when List.mem `Meta mods -> (
      match Char.lowercase_ascii c with
      | 'b' -> Some Model.Word_left
      | 'f' -> Some Model.Word_right
      | _ -> None)
  | `Key (`ASCII c, _) -> Some (Model.Insert (Uchar.of_char c))
  | `Key (`Uchar u, mods) when not (List.mem `Ctrl mods) ->
      Some (Model.Insert u)
  | `Key (`Enter, _) -> Some Model.Enter
  | `Key (`Backspace, _) -> Some Model.Backspace
  | `Key (`Delete, _) -> Some Model.Delete
  | `Key (`Arrow `Left, mods) when List.mem `Ctrl mods || List.mem `Meta mods
    ->
      Some Model.Word_left
  | `Key (`Arrow `Right, mods) when List.mem `Ctrl mods || List.mem `Meta mods
    ->
      Some Model.Word_right
  | `Key (`Arrow `Left, _) -> Some Model.Left
  | `Key (`Arrow `Right, _) -> Some Model.Right
  | `Key (`Arrow `Up, _) -> Some Model.Scroll_up
  | `Key (`Arrow `Down, _) -> Some Model.Scroll_down
  | `Key (`Home, _) -> Some Model.Home
  | `Key (`End, _) -> Some Model.End
  | `Key (`Page `Up, _) -> Some Model.Page_up
  | `Key (`Page `Down, _) -> Some Model.Page_down
  | `Key (`Escape, _) -> Some Model.Quit
  | _ -> None

(* Blocking event reader, run on a dedicated system thread. Waits on the tty
   and on the winch self-pipe; [push]es every event into the Eio stream. The
   thread is not joined: it blocks in [read]/[select] until the process
   exits. *)
let event_thread term tty winch_r push =
  let drain_buf = Bytes.create 64 in
  let rec drain () =
    match Unix.read winch_r drain_buf 0 (Bytes.length drain_buf) with
    | _ -> drain ()
    | exception
        Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
      ->
        ()
  in
  let rec go () =
    let ev =
      if Term.pending term then Some (Term.event term)
      else
        match Unix.select [ tty; winch_r ] [] [] (-1.0) with
        | exception Unix.Unix_error (Unix.EINTR, _, _) ->
            (* If the signal was handled on this thread, [Term.pending] picks
               the resize up on the next iteration. *)
            None
        | rs, _, _ ->
            if List.mem winch_r rs then begin
              drain ();
              (* Notty's own winch callback (same signal dispatch) updates the
                 terminal size; a possibly-stale size here is harmless because
                 the [`Resize] payload is ignored and [Term.pending] delivers a
                 second resize once the flag is set. *)
              Some (`Resize (Term.size term))
            end
            else if rs <> [] then Some (Term.event term)
            else None
    in
    match ev with
    | None -> go ()
    | Some `End -> push Events_closed
    | Some (#Notty.Unescape.event as e) ->
        push (Event e);
        go ()
    | Some (`Resize _ as e) ->
        push (Event e);
        go ()
  in
  try go () with _ -> ( try push Events_closed with _ -> ())

let run ~env (opts : opts) : result =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  let input_data = read_all_stdin () in
  let tty = Unix.openfile "/dev/tty" [ Unix.O_RDWR ] 0 in
  let term = Term.create ~input:tty ~output:tty () in
  Eio.Switch.run @@ fun sw ->
  (* [max_int] capacity: adds never block, so the event thread (which runs
     outside any Eio scheduler) can push safely. *)
  let msgs : msg Eio.Stream.t = Eio.Stream.create max_int in
  let push m = Eio.Stream.add msgs m in

  (* Self-pipe for SIGWINCH: the handler may run on any thread; the write
     wakes the event thread's [select]. *)
  let winch_r, winch_w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock winch_r;
  Unix.set_nonblock winch_w;
  let winch_byte = Bytes.make 1 '!' in
  let (`Revert _) =
    Term.Winch.add tty (fun _dim ->
        try ignore (Unix.write winch_w winch_byte 0 1) with _ -> ())
  in
  let _events : Thread.t =
    Thread.create (fun () -> event_thread term tty winch_r push) ()
  in

  let draw model =
    let w, h = Term.size term in
    let frame = Render.render ~w ~h model in
    Term.image term (image_of_frame frame);
    Term.cursor term frame.cursor
  in

  let schedule_debounce (model : Model.t) =
    if opts.auto then begin
      let seq = model.edit_seq in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Time.sleep clock opts.debounce;
          push (Debounce seq);
          `Stop_daemon)
    end
  in

  let start_run (model : Model.t) proc =
    match Model.args model with
    | Error _ -> (model, proc) (* shown in the status bar; keep the old run *)
    | Ok args ->
        Option.iter (fun (p : Runner.handle) -> p.terminate ()) proc;
        let model = Model.start_run model in
        let gen = model.gen in
        let handle =
          Runner.start ~sw ~proc_mgr ~cmd:model.cmd ~args ~input:input_data
        in
        Eio.Fiber.fork_daemon ~sw (fun () ->
            push (Run_done (gen, Eio.Promise.await handle.outcome));
            `Stop_daemon);
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
    Term.release term;
    {
      accepted;
      args = Model.user_args model;
      command = Model.command_string model;
    }
  in

  let rec loop (model : Model.t) proc =
    draw model;
    match Eio.Stream.take msgs with
    | Events_closed -> finish proc ~accepted:false model
    | Event (`Resize _) -> loop model proc
    | Event (`Mouse _ | `Paste _) -> loop model proc
    | Event (`Key _ as event) -> (
        match key_of_event event with
        | None -> loop model proc
        | Some key -> (
            let view_h = max 1 (snd (Term.size term) - 2) in
            match Model.handle_key ~view_h model key with
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
    Model.create ~single:opts.single ~cmd:opts.cmd
      ~fixed_args:opts.fixed_args ~initial:opts.initial ()
  in
  (* Run once at startup so the output area is populated immediately. *)
  let model, proc = start_run model None in
  match loop model proc with
  | result -> result
  | exception exn ->
      let _ = finish proc ~accepted:false model in
      raise exn
