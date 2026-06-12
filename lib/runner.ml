(** Run one command: feed it the captured stdin, collect stdout/stderr lines
    in arrival order, report the exit status. *)

type outcome = { lines : Model.line array; status : Model.status }

type handle = {
  outcome : outcome Lwt.t;
  terminate : unit -> unit;  (** kill the process (new run supersedes it) *)
}

let ( let* ) = Lwt.bind

(** [input = None] means there was no piped stdin: the command's stdin is
    closed right away (immediate EOF), so it can never try to read the
    user's terminal. [env] overrides the inherited environment (used to
    pass the CUD_* variables to hint commands). *)
let start ?env ~cmd ~args ~input () : handle =
  let proc =
    Lwt_process.open_process_full ?env (cmd, Array.of_list (cmd :: args))
  in
  (* Arrival order across the two channels approximates the interleaving the
     command produced; Lwt is cooperative, so the shared accumulator is
     safe. *)
  let acc = ref [] in
  let reader kind channel =
    let rec go () =
      let* line = Lwt_io.read_line_opt channel in
      match line with
      | Some text ->
          acc := { Model.kind; text } :: !acc;
          go ()
      | None -> Lwt.return_unit
    in
    Lwt.catch go (fun _ -> Lwt.return_unit)
  in
  let feed_stdin () =
    match input with
    | None ->
        Lwt.catch
          (fun () -> Lwt_io.close proc#stdin)
          (fun _ -> Lwt.return_unit)
    | Some data ->
        (* The command may exit without reading its input (EPIPE) — that's
           fine. *)
        Lwt.catch
          (fun () ->
            let* () = Lwt_io.write proc#stdin data in
            Lwt_io.close proc#stdin)
          (fun _ ->
            Lwt.catch
              (fun () -> Lwt_io.close proc#stdin)
              (fun _ -> Lwt.return_unit))
  in
  let outcome =
    let* () =
      Lwt.join
        [ feed_stdin (); reader Model.Out proc#stdout; reader Model.Err proc#stderr ]
    in
    let* process_status = proc#close in
    let status =
      match process_status with
      | Unix.WEXITED n -> Model.Exited n
      | Unix.WSIGNALED n | Unix.WSTOPPED n -> Model.Signaled n
    in
    Lwt.return { lines = Array.of_list (List.rev !acc); status }
  in
  let outcome =
    Lwt.catch
      (fun () -> outcome)
      (fun exn ->
        Lwt.return
          {
            lines = [| { Model.kind = Err; text = "cud: " ^ Printexc.to_string exn } |];
            status = Model.Exited 127;
          })
  in
  { outcome; terminate = (fun () -> proc#terminate) }
