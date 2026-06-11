# ine — INteractive Executor

Pipe data in, point `ine` at a command, and edit the command's arguments
interactively while the output refreshes live. The captured stdin is replayed
to the command on every run.

```
swaymsg -t get_outputs | ine jq
```

```
┌──────────────────────────────────────────────┐
│ jq> .[0].name                                │  ← editable argument line
│ "eDP-1"                                      │  ← scrollable output
│ ...                                          │
│ exit 0          1-1/1  enter run · ^D accept │  ← status bar
└──────────────────────────────────────────────┘
```

## Usage

```
ine [FLAGS] -- CMD [FIXED_ARG...]
```

The argument line is word-split shell-style (single/double quotes and
backslashes work); the words are appended after the fixed arguments. In
single-argument mode (`-1`/`--single`, or Ctrl-T at runtime) the whole line
is passed verbatim as one argument instead — handy for jq filters with pipes.

Flags:

- `-i TEXT`, `--initial=TEXT` — initial contents of the argument line
- `-m`, `--manual` — re-run only on Enter instead of after every edit
- `--debounce=SECONDS` — delay before the automatic re-run (default 0.3)
- `-1`, `--single` — start in single-argument mode

On exit the final arguments are printed to stdout, controlled by mutually
exclusive flags:

- (default) shell-quoted, one line — splice with `jq $(ine ... )`
- `-q`, `--quiet` — print nothing
- `-l`, `--lines` — one argument per line, unquoted
- `-0`, `--null` — NUL-separated, for `xargs -0`
- `-c`, `--command` — the whole command including fixed args, ready to paste

Exit code 0 when accepted with Ctrl-D, 130 when cancelled (Escape / Ctrl-C).

## Keys

| Key | Action |
| --- | --- |
| printable chars | edit the argument line |
| Left/Right, C-b/C-f, Home/End, C-a/C-e | move cursor |
| M-b/M-f, C-Left/C-Right | move by word |
| Backspace/Delete, C-w, C-u, C-k | delete char / word / to start / to end |
| Enter | re-run now |
| C-t | toggle single-argument mode |
| Up/Down, PgUp/PgDn, C-p/C-n | scroll output |
| C-d | accept: exit 0 and print the arguments |
| Esc, C-c | cancel: exit 130 |

The UI reads keys from `/dev/tty`, so stdin stays free for the piped data.
While a re-run is in flight the previous output stays on screen (no flicker);
a superseded run is killed.

## Build & test

```
dune build
dune test       # unit tests + end-to-end PTY tests
```

The end-to-end tests run the real binary on a pseudo-terminal (C stub around
`openpty`/`login_tty`) and assert on the rendered screen and cursor through a
small VT emulator (`test/e2e/vt.ml`) — layout, cursor motion, scrolling,
resize (SIGWINCH), exit codes and the printed-arguments formats are all
covered without any external tooling.

## Architecture

- `lib/editor.ml` — pure zipper line editor (Unicode-aware)
- `lib/shellwords.ml` — pure shell-style word splitting / quoting
- `lib/model.ml` — pure application state + key handling
- `lib/render.ml` — pure state → styled frame renderer (fixed layout:
  input line / output viewport / status bar)
- `lib/runner.ml` — Lwt process execution with stdin replay
- `lib/tui.ml` — thin Notty/Lwt shell around the pure core
- `bin/main.ml` — cmdliner CLI
