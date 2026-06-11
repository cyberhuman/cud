(** Run one command: feed it the captured stdin, collect stdout/stderr lines
    in arrival order, report the exit status. *)

type outcome = { lines : Model.line array; status : Model.status }

type handle = {
  outcome : outcome Eio.Promise.t;
  terminate : unit -> unit;  (** kill the process (new run supersedes it) *)
}

(* Lwt's fork/exec surfaced a missing executable as the child exiting 127;
   [Eio.Process.spawn] raises in the parent instead. Map spawn-time failures
   to the same synthetic outcome. *)
let fail_outcome exn =
  {
    lines = [| { Model.kind = Err; text = "cud: " ^ Printexc.to_string exn } |];
    status = Model.Exited 127;
  }

let close flow = try Eio.Resource.close flow with _ -> ()

(** [input = None] means there was no piped stdin: the command's stdin is
    closed right away (immediate EOF), so it can never try to read the
    user's terminal. *)
let start ~sw ~proc_mgr ~cmd ~args ~input : handle =
  let promise, resolve = Eio.Promise.create () in
  let stdin_r, stdin_w = Eio.Process.pipe ~sw proc_mgr in
  let stdout_r, stdout_w = Eio.Process.pipe ~sw proc_mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw proc_mgr in
  match
    Eio.Process.spawn ~sw proc_mgr ~stdin:stdin_r ~stdout:stdout_w
      ~stderr:stderr_w (cmd :: args)
  with
  | exception exn ->
      List.iter close [ stdin_r; stdout_r; stderr_r ];
      List.iter close [ stdin_w; stdout_w; stderr_w ];
      Eio.Promise.resolve resolve (fail_outcome exn);
      { outcome = promise; terminate = (fun () -> ()) }
  | proc ->
      (* The child holds duplicates of its ends; drop ours so the readers see
         EOF when the child exits. *)
      close stdin_r;
      close stdout_w;
      close stderr_w;
      (* Arrival order across the two pipes approximates the interleaving the
         command produced; fibers of one domain run cooperatively, so the
         shared accumulator is safe. *)
      let acc = ref [] in
      let reader kind flow () =
        (try
           Eio.Buf_read.of_flow flow ~max_size:max_int
           |> Eio.Buf_read.lines
           |> Seq.iter (fun text -> acc := { Model.kind; text } :: !acc)
         with _ -> ());
        close flow
      in
      let feed_stdin () =
        (* The command may exit without reading its input (EPIPE) — that's
           fine. No piped stdin: close without writing, for instant EOF. *)
        (match input with
        | None -> ()
        | Some data -> ( try Eio.Flow.copy_string data stdin_w with _ -> ()));
        close stdin_w
      in
      Eio.Fiber.fork ~sw (fun () ->
          let outcome =
            match
              Eio.Fiber.all
                [ feed_stdin; reader Model.Out stdout_r; reader Model.Err stderr_r ];
              Eio.Process.await proc
            with
            | `Exited n ->
                { lines = Array.of_list (List.rev !acc); status = Model.Exited n }
            | `Signaled n ->
                { lines = Array.of_list (List.rev !acc); status = Model.Signaled n }
            | exception exn -> fail_outcome exn
          in
          ignore (Eio.Promise.try_resolve resolve outcome));
      {
        outcome = promise;
        terminate =
          (fun () -> try Eio.Process.signal proc Sys.sigkill with _ -> ());
      }
