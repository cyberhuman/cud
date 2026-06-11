# Experiment: porting ine's concurrency layer from Lwt to Eio

Branch `exp-eio`. Scope: `lib/runner.ml`, `lib/tui.ml`, `bin/main.ml`, dune
files, and the runner section of `test/unit_tests.ml`. The pure core
(`editor.ml`, `shellwords.ml`, `model.ml`, `render.ml`) and the e2e suite are
untouched. Environment: OCaml 5.3.0, eio 1.3 / eio_main 1.3, notty 0.2.3.

## Status

- `dune test` fully green: unit tests (including the real-process runner
  tests, plus a new spawn-failure case) and all six real-PTY e2e tests.
- Verified stable over five consecutive e2e runs, and under both eio
  backends (`eio_linux`/io_uring, the default here, and
  `EIO_BACKEND=posix`).

## Notty/Eio integration: approach taken

Notty has no Eio backend on opam, so `Notty_lwt` was replaced by
`Notty_unix.Term` (the plain blocking backend) plus a hand-rolled bridge:

- **Rendering** (`Term.image`/`Term.cursor`/`Term.release`) is called
  directly from the main fiber. These are short synchronous writes to
  `/dev/tty`; blocking the domain for them is fine and keeps the code
  trivial.
- **Input**: a dedicated system thread (`Thread.create`) blocks in
  `Unix.select`/`Term.event` and pushes events into an `Eio.Stream`, which
  the main fiber consumes. The stream is created with capacity `max_int` so
  `Stream.add` never suspends — important because the event thread runs
  outside any Eio scheduler and must not hit an effect handler.
  `Eio.Stream` is documented thread-safe, and waking a suspended `take` uses
  the same cross-domain wakeup path eio uses internally; this works from a
  plain systhread (empirically confirmed by the e2e suite).
- **SIGWINCH**: the subtle bit. `Notty_unix.Term.event` notices a resize via
  a flag set by notty's signal handler, but the OCaml handler can run on
  *any* thread; if it runs on the main (scheduler) thread, the event
  thread's blocking `read` is never interrupted and the resize would only be
  delivered on the next keypress. Fix: a classic self-pipe. A second
  listener registered with `Notty_unix.Term.Winch.add` (the sanctioned way
  to add winch listeners without breaking notty's own) writes a byte to a
  non-blocking pipe; the event thread `select`s on both the tty and the pipe
  and synthesizes a `` `Resize `` message. Duplicate resize events are
  harmless (the loop ignores the payload and just redraws from
  `Term.size`). The e2e resize test (TIOCSWINSZ on the pty) passes
  consistently.

Why not the other options:

- `Eio_unix.run_in_systhread (fun () -> Term.event term)` in a fiber looks
  cleaner, but a systhread job cannot be cancelled once started
  (`thread_pool.ml` only checks for cancellation before submission), so a
  daemon fiber blocked there would stall `Switch.run` at exit until one more
  key or resize arrived. A raw thread that is simply never joined sidesteps
  this: the process exits while the thread is still blocked in `read`.
- Driving Notty core ourselves (`Notty.Unescape` + `Notty.Render` over Eio
  flows, own termios/alt-screen handling) would remove the thread but means
  reimplementing most of `Notty_unix.Term` (tcattr setup/restore, terminal
  init/reset sequences, winch plumbing, buffered escape parsing) — far more
  code for no behavioral gain in this app.

## Process running (`runner.ml`)

`Lwt_process.open_process_full` → `Eio.Process.spawn` with three
`Eio.Process.pipe`s. Semantics preserved:

- stdin replay tolerant of EPIPE (catch-all around `Flow.copy_string`,
  always close the sink);
- stdout/stderr collected line-by-line into one shared accumulator in
  arrival order (`Buf_read.lines`; fibers in one domain are cooperative, so
  the unsynchronized accumulator stays safe, same argument as under Lwt);
- `terminate` sends SIGKILL (what Lwt's `proc#terminate` does);
  `Eio.Process.signal` is a no-op after exit, so superseding is race-free;
- exit status mapped to `Model.Exited n` / `Model.Signaled n`.

One real semantic difference: Lwt's fork/exec makes a missing executable
surface as the child exiting 127, while `Eio.Process.spawn` raises
`Executable_not_found` *in the parent, at spawn time*. The port catches
spawn exceptions and resolves the outcome promise with the same synthetic
"ine: …" / exit-127 outcome the Lwt code produced for in-flight failures
(covered by a new unit test). The parent must also explicitly close its
copies of the child's pipe ends or readers never see EOF — under Lwt,
`open_process_full` did that bookkeeping; pipes are additionally attached to
the switch, so nothing leaks even on failure paths.

Debounce: `Lwt.async (sleep; push)` → `Fiber.fork_daemon (Eio.Time.sleep;
push)`. The superseding logic is untouched — it lives in the model
(`edit_seq` comparison), and stale debounce messages are dropped exactly as
before. Daemon fibers are cancelled automatically when the switch ends, so
a pending debounce can't delay exit (with `Lwt.async` it just leaked).

## Code comparison vs the Lwt version

- **LoC**: runner 64 → 72; tui 202 → 268; main unchanged (one line). The
  tui growth is entirely the Notty bridge (~55 lines of event thread +
  winch self-pipe) that `Notty_lwt` used to provide; the event loop proper
  shrank slightly.
- **Direct style vs monadic**: the win is real but modest here because the
  Lwt code was already tidy. `let* () = draw model in let* msg = …` becomes
  `draw model; match Stream.take msgs with …`; `finish` returns a plain
  record; `Tui.run` has type `… -> result` instead of `… -> result Lwt.t`.
  No more `Lwt.return_unit` noise, no `Lwt.catch` — ordinary
  `try`/`match … with exception`.
- **Error handling**: ordinary exceptions compose better (`match spawn …
  with exception exn -> …`), and `Switch`/`Promise` make lifetimes explicit.
  The flip side: one must think about *structured* concurrency — fibers
  belong to a switch, `Switch.run` won't return while a non-daemon fiber is
  blocked, and a systhread job can't be cancelled. Two of the three design
  problems in this port (event-thread shutdown, pipe-end ownership) were
  exactly lifetime questions that Lwt's garbage-collected promises let you
  ignore.
- **Capability passing**: `Runner.start` and `Tui.run` now take `~sw`,
  `~proc_mgr`, `~env` arguments where the Lwt versions used ambient global
  state. Slightly more plumbing, considerably more testable/explicit.
- **Trickiness budget**: Lwt version's hardest part was nothing — Notty_lwt
  did the terminal. Here the hardest part is the input bridge; it's
  self-contained (~70 lines including comments) but it required
  understanding OCaml signal delivery across threads. That cost is paid
  once and would disappear entirely if a notty-eio backend existed.

## Does OCaml 5.4 matter ("better effects syntax")?

No. Confirmed in practice: the entire port compiles and runs on 5.3.0.

- Eio's *public* API is direct-style functions; application code never
  performs or handles effects, so effect syntax is irrelevant to users of
  eio regardless of compiler version. Effects are an implementation detail
  inside eio's schedulers, written against the `Effect` *library* API
  (available since 5.0).
- The dedicated `effect` pattern syntax (`match … with effect E, k -> …`)
  already shipped in OCaml 5.3; 5.4's changes there are incremental and,
  again, only visible to code that implements its own handlers — which this
  project does not.
- Nothing in this codebase would change a single line under 5.4.

## Blockers / rough edges

- **No notty-eio backend** (the known gap). The bridge works, but it is the
  kind of code a library should own: thread + self-pipe + signal-delivery
  reasoning.
- **`Eio_unix.run_in_systhread` is not cancellable once started**, which
  rules out the "obvious" clean wrapper for blocking `Term.event` (would
  hang `Switch.run` on exit).
- **`Eio.Stream` suspension constraint**: pushing from a non-eio thread is
  only safe if `add` cannot suspend, hence the unbounded stream. Not
  documented prominently; easy trap.
- **Spawn-failure semantics differ from Lwt** (parent-side exception vs
  child exit 127) — easy to paper over, but a porter must notice it.
- No missing packages; eio 1.3 + eio_main 1.3 from the shared switch were
  sufficient. No new opam switch needed.

## Recommendation

The port is viable and fully green against the implementation-agnostic e2e
suite, with equivalent unit coverage (plus one extra spawn-failure test).
The runner, debounce and event-loop code genuinely read better in direct
style, and structured concurrency fixed two latent untidinesses (leaked
debounce timers; in-flight runs not awaited on exit).

Whether to merge is a judgement call on the Notty bridge: ~70 lines of
thread/signal plumbing replacing a maintained `Notty_lwt` backend. If the
project wants to standardize on eio (e.g. to drop the Lwt dependency tree or
to grow multicore features), this branch is a sound base — ideally with the
bridge extracted into a small `notty_eio` module/library. If Lwt is
otherwise fine, the main branch loses nothing by staying on `Notty_lwt`: the
behavior, performance and test results are indistinguishable for this
workload. OCaml 5.4 is not a factor either way.
