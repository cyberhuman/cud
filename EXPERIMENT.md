# Experiment: replace lib/editor.ml with `mew` (or `zed`)

Branch `exp-mew`. Question: can the opam library `mew` (suspected to be a
line editor) replace ine's hand-rolled zipper editor (`lib/editor.ml`,
124 LoC) and/or simplify key handling in `lib/model.ml` / `lib/tui.ml`?

## What `mew` actually is

`mew` 0.1.0 is **not a line editor**. It is a *"modal editing witch — a
general modal editing engine generator"*: a functor
`Mew.Make (Modal) (Concurrent)` that turns key streams into actions via a
trie of key-sequence bindings, with per-mode timeouts, parameterised over a
key type, a mode-name type, and a concurrency monad (threads + mailboxes).
It contains **no text-editing code at all** — no buffer, no cursor, no
insert/delete. Its only purpose in the wild is `mew_vi`, the vi-mode
*keybinding interpreter* used by lambda-term: `mew_vi` consumes keys and
emits abstract `Vi_action`/`Edit_action` values that something else (zed,
in lambda-term's case) must execute. Deps: `result`, `trie`.

For ine — a single-mode, emacs-style editor with ~20 flat keybindings
dispatched by one pattern match in `tui.ml` (`key_of_event`, ~45 lines) —
mew would replace that pattern match with two functor instantiations, a
key type with modifier sets, a mode table, and a mailbox-driven thread,
and would still leave all of `editor.ml` in place. It solves a problem ine
does not have (multi-key modal sequences like `d2w` with ambiguity
timeouts). **Not applicable.**

## What `zed` offers

`zed` ("abstract engine for text edition", the editing core of
lambda-term) is the plausible candidate: `Zed_edit` has every operation
ine needs (insert, delete_prev/next_char, prev/next_word,
delete_prev_word, delete_prev/next_line as kill-to-start/end, plus
clipboard, undo, marks, multi-cursor, case ops). It is genuinely more
capable than `editor.ml`.

But its shape is the opposite of ine's core:

- **Mutable + reactive.** `'a Zed_edit.t` is a stateful engine; change
  notification flows through React signals/events (`Zed_cursor.changes`,
  `update : 'a t -> Zed_cursor.t list -> unit event`). ine's `Model` is a
  pure value; `Model.with_edit` detects no-op edits with physical
  equality (`ed == t.editor`) to decide whether to bump `edit_seq` and
  schedule a debounced re-run. A mutable engine cannot give that signal;
  adopting zed idiomatically means making `Model` imperative and moving
  change detection into React, i.e. giving up the pure, unit-testable
  core that is the point of the design.
- **Word semantics are wrong for shell arguments.** zed's word ops use
  UAX#29 word segmentation (via `uuseg` + a rope zipper). ine deliberately
  uses whitespace-delimited words, which is what shells do for Ctrl-W /
  M-b. Overriding this needs a custom `match_word : Zed_rope.t -> int ->
  int option` — and even then `prev_word`/`delete_prev_word` scan with
  per-index `match_word` probes, so the override is awkward to write
  correctly.
- **Different counting unit.** zed counts `Zed_char.t` (printable core +
  combining marks), ine counts Unicode scalar values. Cursor positions and
  lengths disagree on combining sequences (see prototype output). zed's
  behaviour is arguably *nicer* (backspace deletes a whole composed
  character), but it would ripple into `Render` cursor placement and the
  e2e VT assertions.
- **Dependency weight.** zed requires `react uchar uucp uuseg uutf`;
  `uucp` alone is ~23 MB of Unicode property tables in the switch.
  Today `ine_lib` needs only notty + lwt (notty has no uucp dependency).
  The comparison binary linking zed came out roughly the same size as
  `main.exe`, so binary bloat is modest, but the build/dep footprint grows
  substantially for a 124-line module.

## What was done

`prototype/zed_editor.ml` (60 LoC) implements ine's full `Editor`
interface on top of `Zed_edit`, keeping purity by materialising a fresh
engine per operation (load text → goto cursor → one action → read back).
`prototype/compare.ml` runs both implementations side by side on
shell-flavoured inputs (`opam exec -- dune exec prototype/compare.exe`):

- **Agree** on all plain motions/edits and on word ops over space-separated
  alphabetic words: left/right, home/end, backspace, delete,
  kill-to-start/end, insert mid-line, word_left/right and kill_prev_word on
  `"hello world"`, including trailing-blank handling.
- **Differ** exactly where predicted:
  - `kill_prev_word "--filter=name"` → ine `""`, zed `"--filter="`
  - `kill_prev_word "path/to/file.txt"` → ine `""`, zed `"path/to/file."`
  - `kill_prev_word "a-b"` → ine `""`, zed `"a"`
  - `word_left "git log --one-line"` → ine cursor 8, zed cursor 14
  - combining char `e + U+0301`: ine cursor 3 / backspace leaves `"e"`,
    zed cursor 2 / backspace deletes the whole composed char.

The main library is untouched; `opam exec -- dune test` passes (all unit
tests + all 6 e2e tests).

## LoC / type-safety comparison

| | hand-rolled `editor.ml` | zed-backed |
|---|---|---|
| our code | 124 LoC (incl. doc comments), stdlib only | 60 LoC wrapper — but unidiomatic (engine rebuilt per keystroke); idiomatic use means rewriting `Model` around a mutable engine + React, touching model/render/tui/tests |
| purity / testability | pure values, `==` no-op detection drives debounce | mutable, signal-driven; no structural no-op signal |
| word semantics | shell-style (whitespace) — what users expect at a command prompt | UAX#29; custom `match_word` needed, and the scan API makes whitespace-words awkward |
| unicode | scalar values; combining marks counted separately | `Zed_char.t` grapheme-ish handling — genuinely better, but changes cursor arithmetic and e2e expectations |
| deps | none beyond stdlib | + react, uutf, uucp (~23 MB), uuseg |
| extras we'd gain | — | undo, clipboard/kill-ring, marks, case ops (none currently in ine's UI) |

## Recommendation

**Don't adopt.** `mew` is a modal-keybinding engine generator, not an
editor — irrelevant to ine. `zed` can express everything `editor.ml` does,
but it inverts the architecture (mutable + React vs pure values), defaults
to the wrong word semantics for a shell-argument editor, and adds a heavy
Unicode dependency stack — all to replace 124 dependency-free, fully
unit-tested lines. The trade only becomes interesting if ine grows
features zed gives for free (undo, kill-ring/yank, multi-line editing);
revisit then. One idea worth stealing regardless of library: treating a
base character plus combining marks as a single unit for backspace/cursor
motion (zed's `Zed_char`) — implementable in `editor.ml` in a few lines.
