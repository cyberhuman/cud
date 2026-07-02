(** Terminal shell: Notty rendering and the Lwt event loop around the pure
    {!Model}. Keyboard input and drawing go through [/dev/tty], so stdin
    stays free for the data being piped in. *)

module Term = Notty_lwt.Term

type opts = {
  cmd : string option;  (** [None]: the input line is the whole command *)
  fixed_args : string list;
  placeholder : string option;
      (** xargs -I style substitution point in [fixed_args] *)
  initials : string list;
      (** initial argument lines; with [--pipe] one per step, in order *)
  pipe : bool;
      (** each positional argument is one step of a shell pipeline *)
  pipefail : bool;
      (** the pipeline's exit code reflects any failing step, not just the
          last one *)
  auto : bool;  (** re-run automatically after edits *)
  debounce : float;  (** seconds to wait after the last edit *)
  single : bool;  (** start in single-argument mode *)
  vim : bool;  (** vim keybindings *)
  enter_accept : bool;  (** Enter accepts and exits *)
  ansi : bool;  (** respect SGR color sequences in the output *)
  multiline : bool;  (** multi-line args editor *)
  lenses : string list;  (** [--lens] commands for the second pane *)
  hints : string list;  (** [--hint] commands for the second pane *)
}

type result = {
  accepted : bool;  (** Ctrl-D (true) vs Escape/Ctrl-C (false) *)
  status : Model.status option;  (** of the last finished run *)
  args : string list list;
      (** the arguments in the editors when the UI closed, one list per
          pipeline step (a single one without [--pipe]) *)
  command : string;  (** the whole command (pipeline), shell-quoted *)
  output : string list;  (** the last finished run's output lines *)
}

type msg =
  | Event of [ Notty.Unescape.event | `Resize of int * int ]
  | Run_done of int * Runner.outcome
  | Pane_done of int * int * Runner.outcome  (** pane index, pane generation *)
  | Debounce of int
  | Hint_debounce of int
  | Events_closed

let ( let* ) = Lwt.bind

let read_all_stdin () =
  if Unix.isatty Unix.stdin then None
  else Some (In_channel.input_all In_channel.stdin)

(* 256-color palette: 16 named colors, the 6x6x6 cube, the grayscale ramp.
   Truecolor is downscaled to the cube — notty's [A.rgb_888] would emit
   38;2;R;G;B SGR even on terminals that don't understand it. *)
let color_of_ansi =
  let open Notty.A in
  let base = [| black; red; green; yellow; blue; magenta; cyan; white |] in
  let light =
    [|
      lightblack;
      lightred;
      lightgreen;
      lightyellow;
      lightblue;
      lightmagenta;
      lightcyan;
      lightwhite;
    |]
  in
  function
  | Render.Idx n ->
      let n = max 0 (min 255 n) in
      if n < 8 then base.(n)
      else if n < 16 then light.(n - 8)
      else if n < 232 then
        let n = n - 16 in
        rgb ~r:(n / 36) ~g:(n / 6 mod 6) ~b:(n mod 6)
      else gray (n - 232)
  | Render.Rgb (r, g, b) ->
      let q c = ((max 0 (min 255 c) * 5) + 127) / 255 in
      rgb ~r:(q r) ~g:(q g) ~b:(q b)

(* notty-community has no dim style; [a.dim] is dropped. *)
let attr_of_ansi (a : Render.ansi_attrs) =
  let open Notty.A in
  let opt color acc = function Some c -> acc ++ color (color_of_ansi c) | None -> acc in
  let flag cond s acc = if cond then acc ++ st s else acc in
  opt fg empty a.fg |> fun acc ->
  opt bg acc a.bg |> flag a.bold bold |> flag a.italic italic
  |> flag a.underline underline |> flag a.reverse reverse

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
  | Render.Ansi a -> attr_of_ansi a

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
  | `Key (`Enter, mods) when List.mem `Meta mods ->
      (* ESC CR, i.e. Alt-Enter (the tty's ICRNL turns CR into NL) *)
      Some (Model.I_special Model.S_meta_enter)
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
  | `Key (`Arrow `Up, mods) when List.mem `Ctrl mods || List.mem `Shift mods
    ->
      Some (Model.I_special Model.S_mod_up)
  | `Key (`Arrow `Down, mods)
    when List.mem `Ctrl mods || List.mem `Shift mods ->
      Some (Model.I_special Model.S_mod_down)
  | `Key (`Arrow `Up, _) -> Some (Model.I_special Model.S_up)
  | `Key (`Arrow `Down, _) -> Some (Model.I_special Model.S_down)
  | `Key (`Home, _) -> Some (Model.I_special Model.S_home)
  | `Key (`End, _) -> Some (Model.I_special Model.S_end)
  | `Key (`Page `Up, _) -> Some (Model.I_special Model.S_pgup)
  | `Key (`Page `Down, _) -> Some (Model.I_special Model.S_pgdn)
  | `Key (`Tab, _) -> Some (Model.I_special Model.S_tab)
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

  (* --- lens/hint panes --- *)
  let npanes = List.length opts.lenses + List.length opts.hints in
  (* per-pane run generation and in-flight process, for superseding *)
  let pane_gens = Array.make (max 1 npanes) 0 in
  let pane_procs : Runner.handle option array = Array.make (max 1 npanes) None in

  (* The panes' stdin: the main command's latest finished output. *)
  let pane_input (model : Model.t) =
    let texts =
      Array.to_list model.lines
      |> List.filter_map (fun (l : Model.line) ->
             if l.kind = Model.Info then None else Some l.text)
    in
    match texts with
    | [] -> Some ""
    | _ -> Some (String.concat "\n" texts ^ "\n")
  in
  let hint_env (model : Model.t) =
    let ed = Model.editor model in
    let extras =
      [|
        "CUD_BEFORE=" ^ Editor.to_string (Editor.kill_to_end ed);
        "CUD_AFTER=" ^ Editor.to_string (Editor.kill_to_start ed);
        "CUD_FIXED=" ^ Model.fixed_text model;
        ("CUD_CMD="
        ^
        match Model.current_command model with
        | Ok (Some (p, _)) -> p
        | _ -> "");
      |]
    in
    Array.append (Unix.environment ()) extras
  in
  let start_pane (model : Model.t) i =
    let p = model.panes.(i) in
    Option.iter (fun (h : Runner.handle) -> h.terminate ()) pane_procs.(i);
    pane_gens.(i) <- pane_gens.(i) + 1;
    let gen = pane_gens.(i) in
    let env = if p.Model.hint then Some (hint_env model) else None in
    let handle =
      Runner.start ?env ~cmd:"sh" ~args:[ "-c"; p.Model.spec ]
        ~input:(pane_input model) ()
    in
    pane_procs.(i) <- Some handle;
    Lwt.async (fun () ->
        let* outcome = handle.outcome in
        push (Pane_done (i, gen, outcome));
        Lwt.return_unit)
  in
  let start_panes ?(hints_only = false) (model : Model.t) =
    Array.iteri
      (fun i (p : Model.pane) ->
        if p.hint || not hints_only then start_pane model i)
      model.panes
  in

  (* Hints also re-run when the input text or cursor moves: a debounced
     sequence counter, bumped on any change of the focused editor. *)
  let hint_seq = ref 0 in
  let schedule_hints () =
    incr hint_seq;
    let seq = !hint_seq in
    Lwt.async (fun () ->
        let* () = Lwt_unix.sleep 0.15 in
        push (Hint_debounce seq);
        Lwt.return_unit)
  in
  let has_hints = opts.hints <> [] in
  let edit_sig (model : Model.t) =
    let ed = Model.focused model in
    (model.focus, model.cur, Editor.to_string ed, Editor.cursor ed)
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
        let handle = Runner.start ~cmd:prog ~args ~input:input_data () in
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
    Array.iter
      (Option.iter (fun (h : Runner.handle) -> h.terminate ()))
      pane_procs;
    let* () = Term.release term in
    Lwt.return
      {
        accepted;
        status = model.Model.status;
        args = Model.all_args model;
        command = Model.command_string model;
        output =
          Array.to_list model.lines
          |> List.filter_map (fun (l : Model.line) ->
                 if l.kind = Model.Info then None else Some l.text);
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
            let th = snd (Term.size term) in
            let view_h = max 1 (th - Render.input_height ~h:th model - 1) in
            match Model.handle_input ~view_h model input with
            | Model.Quit_exit -> finish proc ~accepted:false model
            | Model.Accept_exit -> finish proc ~accepted:true model
            | Model.Continue (model', effects) ->
                (* any change of the input text or cursor position
                   (re)schedules the hints *)
                if has_hints && edit_sig model' <> edit_sig model then
                  schedule_hints ();
                let model', proc = apply_effects model' proc effects in
                loop model' proc))
    | Run_done (gen, { lines; status }) ->
        let model' = Model.finish_run model ~gen ~lines ~status in
        (* a fresh main output: re-run every lens and hint over it *)
        if gen = model.gen then start_panes model';
        loop model' proc
    | Pane_done (i, gen, { lines; status = _ }) ->
        if gen = pane_gens.(i) then begin
          pane_procs.(i) <- None;
          loop (Model.set_pane model i lines) proc
        end
        else loop model proc
    | Debounce seq ->
        (* Only the debounce of the latest edit triggers; an in-flight run is
           superseded (terminated) by [start_run]. *)
        if opts.auto && seq = model.edit_seq then
          let model, proc = start_run model proc in
          loop model proc
        else loop model proc
    | Hint_debounce seq ->
        if seq = !hint_seq then start_panes ~hints_only:true model;
        loop model proc
  in

  let model =
    (* --pipe: every positional argument is one step's fixed command line,
       verbatim; -i values pair up with the steps in order (either list may
       be the longer one) *)
    let steps =
      if not opts.pipe then []
      else
        let fixed =
          match opts.cmd with Some c -> c :: opts.fixed_args | None -> []
        in
        let n = max 1 (max (List.length fixed) (List.length opts.initials)) in
        List.init n (fun i ->
            ( Option.value (List.nth_opt fixed i) ~default:"",
              Option.value (List.nth_opt opts.initials i) ~default:"" ))
    in
    Model.create ?cmd:opts.cmd ?placeholder:opts.placeholder ~steps
      ~single:opts.single ~pipefail:opts.pipefail ~vim:opts.vim
      ~enter_accept:opts.enter_accept
      ~ansi:opts.ansi ~multiline:opts.multiline ~lenses:opts.lenses
      ~hints:opts.hints ~fixed_args:opts.fixed_args
      ~initial:(Option.value (List.nth_opt opts.initials 0) ~default:"")
      ()
  in
  (* Run once at startup so the output area is populated immediately. *)
  let model, proc = start_run model None in
  Lwt.catch
    (fun () -> loop model proc)
    (fun exn ->
      let* _ = finish proc ~accepted:false model in
      Lwt.fail exn)
