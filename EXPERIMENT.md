# Experiment: porting cud's concurrency layer from Lwt to Eio (round 2)

Branch `exp-eio-cud`, based on current master (vim mode, optional command,
no-piped-stdin handling, kill register/undo/redo, single-arg mode). This
repeats the earlier `ine` experiment (branch `exp-eio` in
`/home/raman/projects/ine-exp-eio`) against the substantially evolved
codebase. Scope is unchanged: `lib/runner.ml`, `lib/tui.ml`, `bin/main.ml`,
dune files, and the runner section of `test/unit_tests.ml`. The pure core
(`editor.ml`, `shellwords.ml`, `model.ml`, `render.ml`) and the e2e suite are
untouched. Environment: OCaml 5.3.0, eio 1.3 / eio_main 1.3, notty 0.2.3.

## Status

- `dune test` fully green: unit tests (including the real-process runner
  tests, the no-stdin test, and the spawn-failure mapping test) and all
  **ten** real-PTY e2e scenarios (jq-flow, scroll-resize-cancel,
  manual-mode, fixed-args-lines, null-output, command-output, vim-mode,
  vim-scroll, no-command, no-stdin).
- Verified stable over three consecutive full runs.

## What carried over from the first port (still valid)

The architecture findings of the first experiment transferred wholesale;
none needed revisiting:

- **Notty bridge**: `Notty_lwt` → `Notty_unix.Term` rendered directly from
  the main fiber, plus a dedicated `Thread.create` event thread blocking in
  `select`/`Term.event` and pushing into an unbounded `Eio.Stream`
  (capacity `max_int` so `Stream.add` never suspends from the non-eio
  thread). The thread is never joined; eio 1.3 systhreads are not
  cancellable, so `Eio_unix.run_in_systhread` around `Term.event` remains a
  non-starter (it would stall `Switch.run` at exit).
- **SIGWINCH self-pipe** registered via `Term.Winch.add`, drained by the
  event thread, synthesizing a `` `Resize `` message regardless of which
  thread the signal handler ran on. The e2e resize test passes unchanged.
- **Runner**: `Eio.Process.spawn` + three `Eio.Process.pipe`s; parent closes
  its copies of the child's ends; stdout/stderr collected line-by-line into
  one shared arrival-order accumulator via `Buf_read.lines`; `terminate`
  sends SIGKILL (no-op after exit, so superseding stays race-free).
- **Spawn-failure mapping**: `Eio.Process.spawn` raises
  `Executable_not_found` in the parent where Lwt's fork/exec produced a
  child exiting 127; the port still catches spawn exceptions and resolves
  the outcome promise with the synthetic "cud: …" / exit-127 outcome
  (unit-tested).
- **Debounce**: `Fiber.fork_daemon` + `Eio.Time.sleep`; daemons are
  cancelled when the switch ends, so pending debounces cannot delay exit.

## What the new features forced (deltas vs the first port)

All three were straightforward; nothing fought the eio structure:

- **`~input : string option` (no-piped-stdin)**. New semantics: `None`
  means the child's stdin must be closed immediately for instant EOF. In
  eio this is one line: the stdin pipe is still created and passed to
  `spawn`, but `feed_stdin` skips `Flow.copy_string` and just closes the
  write end. (Not creating the pipe at all is *not* an option: omitting
  `~stdin` would make the child inherit the parent's stdin — the tty or the
  consumed pipe — exactly what the feature exists to prevent.) The Lwt
  version needed nested `Lwt.catch` around `Lwt_io.close`; the eio version
  is a plain `match` + `close`.
- **Optional command / `Model.command` returning `Ok None` → `set_idle`**.
  `start_run` gained the third branch: terminate any in-flight run, return
  `(Model.set_idle model ~note:…, None)`. No fiber-lifetime interaction at
  all, and for a clean reason: the killed run's `Run_done` daemon fiber
  still fires later, but `set_idle` bumps the model generation, so
  `Model.finish_run` discards the stale outcome — the same gen-based
  superseding that already protected ordinary re-runs. The daemon fiber
  then stops; nothing holds the switch open. (Had `set_idle` *not* bumped
  the generation, the stale `Signaled 9` outcome would have overwritten the
  idle note — but that would be equally true under Lwt; it's a model
  property, not a concurrency-layer one.)
- **Vim mode / raw-input layer**. Invisible to the port: the old
  `key_of_event` (which interpreted ctrl-keys in the tui) had already moved
  into the pure model as the `Model.input` type; the tui's `input_of_event`
  is a dumber, purely syntactic translation. The eio side is byte-for-byte
  the Lwt side here. Same for kill register/undo/redo (pure model) and
  `Bar_mode` rendering (one more style arm). `bin/main.ml`'s cmdliner
  `Term.Syntax` code is also untouched — only `Lwt_main.run (Tui.run opts)`
  became `Eio_main.run (fun env -> Tui.run ~env opts)`.

Net effect: the second port was mostly mechanical — re-applying the known
bridge around a bigger but unchanged-in-shape event loop. The diff between
the Lwt and eio `tui.ml` is concentrated in the same ~70-line bridge block
as last time; the loop body differs only in `let*`/`Lwt_stream` vs direct
calls/`Eio.Stream`.

## Code comparison vs current Lwt master

| file | Lwt master | eio port |
|---|---|---|
| `lib/runner.ml` | 76 | 83 |
| `lib/tui.ml` | 192 | 258 |
| `bin/main.ml` | 143 | 143 |
| total | 411 | 484 |

As before, the growth is entirely the Notty bridge (~70 lines of event
thread + winch self-pipe) that `Notty_lwt` provided for free; the event
loop proper is the same length or slightly shorter, and the runner's +7 is
the explicit pipe bookkeeping plus the spawn-failure arm. The eio tax is a
constant ~70 lines: the app grew substantially since round 1 (vim mode
etc.) while the bridge did not change at all, so the relative overhead
shrinks as the project evolves.

## Updated recommendation

Unchanged in direction, slightly stronger in confidence:

- The port is viable, fully green against the implementation-agnostic
  ten-scenario e2e suite, and was *cheap to redo* against a heavily evolved
  codebase — evidence that the eio layer tracks this app's growth well. The
  reason is architectural: cud keeps pushing logic into the pure model
  (vim input interpretation, set_idle/gen superseding), so the concurrency
  layer stays a thin shell that is easy to swap.
- The one standing cost is still the hand-rolled Notty bridge. It is a
  fixed, self-contained ~70 lines that did not need a single change in
  semantics between rounds — which is also the strongest argument that it
  belongs in a small reusable `notty_eio` library rather than in this app.
- If the project wants to drop the Lwt dependency tree or grow multicore
  features, merge this branch (ideally extracting the bridge). If not,
  master loses nothing by staying on `Notty_lwt`: behavior, performance and
  test results remain indistinguishable for this workload. OCaml 5.4 is
  still not a factor; everything compiles and runs on 5.3.0.
