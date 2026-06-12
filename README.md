# cud

> **cud** /kʌd/ *n.* — food that a ruminant (such as a camel) brings back up
> from its first stomach to chew over again, until it can be properly
> digested.

`cud` does to your data what a camel does to its lunch: it captures stdin
once, then feeds the *same* input to a command over and over while you chew
on the arguments in an interactive editor, watching the output refresh live —
until the command line is properly digested.

```
swaymsg -t get_outputs | cud jq
```

![cud demo: swaymsg -t get_outputs | cud --ansi -- jq -C — live filter editing with colors, single-arg mode toggle](demo.gif)

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
cud [FLAGS] [-- CMD [FIXED_ARG...]]
```

The argument line is word-split shell-style (single/double quotes and
backslashes work); the words are appended after the fixed arguments. In
single-argument mode (`-1`/`--single`, or Ctrl-T at runtime) the whole line
is passed verbatim as one argument instead — handy for jq filters with pipes.

With `-I STR` (like `xargs -I`) the editable arguments go where `STR` sits in
the fixed arguments instead of the end: `cud -I{} -- kubectl get {} -o json`
splices the words in place of `{}`, and a fixed argument that merely contains
`STR` (e.g. `--glob '*.{}'`) gets it substituted as text.

The fixed arguments are editable too: Tab moves the cursor into the
fixed-args region before the `>` and back; all editing keys (and vim mode)
work there, and changes re-run the command.

`CMD` itself is optional: with no command at all the input line *is* the
command line (its first word is the program; in single-argument mode the
line runs via `sh -c`).

When stdin is a terminal (nothing piped), the command's stdin is closed
immediately, so it reads instant EOF instead of fighting you for the tty.

Flags:

- `-i TEXT`, `--initial=TEXT` — initial contents of the argument line
- `-m`, `--manual` — re-run only on Enter instead of after every edit
- `--debounce=SECONDS` — delay before the automatic re-run (default 0.3)
- `-1`, `--single` — start in single-argument mode
- `-I STR`, `--placeholder=STR` — substitution point in the fixed arguments
- `-e`, `--enter-accept` — Enter accepts and exits (like Ctrl-D)
- `-M`, `--multiline` — multi-line argument editor (see below)
- `-A`, `--ansi` — respect ANSI SGR sequences (colors, bold, …) in the command's
  output instead of stripping them; other escape sequences are still removed
- `--lens CMD`, `--hint CMD` — second pane with derived views (see below;
  repeatable)
- `--vim` — vim keybindings (see below)

On accept (Ctrl-D) the final arguments are printed to stdout — cancelling
prints nothing — controlled by mutually
exclusive flags:

- (default) shell-quoted, one line — splice with `jq $(cud ... )`
- `-q`, `--quiet` — print nothing
- `-l`, `--lines` — one argument per line, unquoted
- `-0`, `--null` — NUL-separated, for `xargs -0`
- `-c`, `--command` — the whole command including fixed args, ready to paste
- `-o`, `--output` — the command's output instead of the arguments

Exit code on accept (Ctrl-D): the last run's exit code (0 on success,
128+signal if it was killed); 130 when cancelled (Escape / Ctrl-C).

## Keys

| Key | Action |
| --- | --- |
| printable chars | edit the argument line |
| Tab | move the cursor between the args field and the fixed args (and the output with `-M`) |
| Alt-Enter, C-o | accept/run, bypassing the multiline line break |
| Left/Right, C-b/C-f, Home/End, C-a/C-e | move cursor |
| M-b/M-f, C-Left/C-Right | move by word |
| Backspace/Delete, C-w, C-u, C-k | delete char / word / to start / to end |
| C-y | yank the last killed text |
| Enter | re-run now (`-M`: insert a line break; `-e`: accept) |
| C-t | toggle single-argument mode |
| M-a | toggle ANSI color rendering (`--ansi`) |
| Up/Down, PgUp/PgDn, C-p/C-n | scroll output |
| Ctrl/Shift+Up/Down | scroll output (even from the input in `-M`) |
| C-d | accept: exit 0 and print the arguments |
| Esc, C-c | cancel: exit 130 |

### Multiline mode (`-M`)

The argument editor holds multiple lines; combined with `-1` the whole
multi-line text is passed as one argument — handy for long jq filters:

```
kubectl get pods -o json | cud -M -1 jq
```

![cud multiline demo: building a three-line jq filter with Enter](demo-multiline.gif)

- Enter inserts a line break; Alt-Enter (or Ctrl-O) does what Enter would
  do otherwise — accept with `--enter-accept`, re-run without. The priority
  for plain Enter is: multiline line break > accept (`-e`) > re-run
- Up/Down (and vim `j`/`k`) move the cursor across the input lines,
  preserving the column where possible; PgUp/PgDn — and Ctrl/Shift+Up/Down —
  still scroll the output
- Tab cycles the focus: args → fixed args → output → args (the fixed-args
  stop is skipped when there is no fixed command); with the output focused,
  Up/Down and PgUp/PgDn scroll it and printable keys are ignored
- the input area grows with the text up to a third of the screen, then
  scrolls vertically to follow the cursor; continuation lines are shown
  indented by two spaces

### Lenses and hints (`--lens`, `--hint`)

With at least one `--lens` or `--hint` the output area splits: the left
half is the usual (scrollable) output, the right half shows one section per
lens/hint command — lenses first, then hints, each under a header with its
command string.

![cud lens/hint demo: jq with a `jq keys` lens and a cursor-following hint](demo-lens.gif)

Every lens/hint runs via `sh -c CMD` with the main command's latest output
on stdin, and re-runs whenever a main run finishes — good for live summaries
of what you are looking at:

```
kubectl get pods -o json | cud --lens 'jq keys' --lens 'wc -l' jq
```

Hints additionally re-run (debounced) every time the input line changes *or
the cursor moves*, and see the editing context in their environment:

- `CUD_BEFORE` / `CUD_AFTER` — the argument text before/after the cursor
- `CUD_FIXED` — the fixed-arguments line
- `CUD_CMD` — the command name (empty when the line itself is the command)

That makes them a place for as-you-type suggestions or documentation
lookups, e.g. showing the manual paragraph for the jq function you are
typing: `--hint 'man -P cat jq | grep -F -A3 "$CUD_BEFORE"'`.

Below 20 columns the panes are dropped and the full width goes back to the
output.

### Vim mode (`--vim`)

Starts in insert mode (all keys above work there); Escape enters normal
mode, shown in the status bar:

- motions: `h` `l` `0` `^` `$` `w` `b` `e` `f`*c* `F`*c* (WORD-wise)
- edits: `x` `X` `s` `r`*c* `D` `C` `dd` `cc`, operators `d`/`c` with any
  motion (`cw` behaves like `ce`, as in vim)
- `i` `a` `I` `A` enter insert mode (`-M`: also `o`/`O` to open a line
  below/above); `p`/`P` paste the register;
  `u`/`C-r` undo/redo
- output scrolling: `j` `k` `G` `gg`
- Enter re-runs; C-d accepts; C-c cancels (Escape never quits in vim mode)

## Build & test

```
dune build
dune test       # unit tests + end-to-end PTY tests
```

The end-to-end tests run the real binary on a pseudo-terminal (C stub around
`openpty`/`login_tty`) and assert on the rendered screen and cursor through a
small VT emulator (`test/e2e/vt.ml`) — layout, cursor motion, editing keys,
vim mode, scrolling, resize (SIGWINCH), exit codes and the printed-arguments
formats are all covered without any external tooling.

## Architecture

- `lib/editor.ml` — pure zipper line editor (Unicode-aware)
- `lib/shellwords.ml` — pure shell-style word splitting / quoting
- `lib/model.ml` — pure application state, key interpretation (emacs and
  vim state machines), undo/redo, register
- `lib/render.ml` — pure state → styled frame renderer (fixed layout:
  input line / output viewport / status bar)
- `lib/runner.ml` — Lwt process execution with stdin replay
- `lib/tui.ml` — thin Notty/Lwt shell around the pure core
- `bin/main.ml` — cmdliner CLI
