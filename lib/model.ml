(** Application state and its pure update function.

    The TUI layer translates terminal events into {!key} values and performs
    the {!effect_}s requested by {!handle_key}; everything here is pure and
    can be exercised in unit tests without a terminal. *)

type line_kind = Out | Err | Info
type line = { kind : line_kind; text : string }

type status = Exited of int | Signaled of int

type key =
  | Insert of Uchar.t
  | Left
  | Right
  | Home
  | End
  | Word_left
  | Word_right
  | Backspace
  | Delete
  | Kill_to_end
  | Kill_to_start
  | Kill_prev_word
  | Enter
  | Toggle_single
  | Scroll_up
  | Scroll_down
  | Page_up
  | Page_down
  | Accept
  | Quit
  | Redraw

type t = {
  cmd : string;
  fixed_args : string list;
  editor : Editor.t;
  lines : line array;  (** output of the last finished run *)
  scroll : int;  (** index of the first visible output line *)
  running : bool;
  status : status option;  (** of the last finished run *)
  parse_error : Shellwords.error option;
  single : bool;
      (** pass the whole line as one argument instead of word-splitting *)
  gen : int;  (** run generation, bumped by [start_run] *)
  edit_seq : int;  (** bumped on every text change, for debouncing *)
}

let parse_error_of ~single text =
  if single then None
  else match Shellwords.split text with Ok _ -> None | Error e -> Some e

let create ?(single = false) ~cmd ~fixed_args ~initial () =
  {
    cmd;
    fixed_args;
    editor = Editor.of_string initial;
    lines = [||];
    scroll = 0;
    running = false;
    status = None;
    parse_error = parse_error_of ~single initial;
    single;
    gen = 0;
    edit_seq = 0;
  }

(** Arguments to run: fixed args followed by the input line — word-split, or
    taken verbatim as a single argument in [single] mode (empty line: no
    argument at all). *)
let args t =
  let text = Editor.to_string t.editor in
  if t.single then Ok (t.fixed_args @ (if text = "" then [] else [ text ]))
  else
    match Shellwords.split text with
    | Ok words -> Ok (t.fixed_args @ words)
    | Error e -> Error e

(** The arguments the user provided in the editor, for printing at exit. In
    [single] mode the line is one argument; in split mode an unparseable
    line is returned as-is, best effort. *)
let user_args t =
  let text = Editor.to_string t.editor in
  if t.single then (if text = "" then [] else [ text ])
  else
    match Shellwords.split text with
    | Ok words -> words
    | Error _ -> [ text ]

(** The full command as a shell-quoted string. If the input line doesn't
    parse, it is appended verbatim. *)
let command_string t =
  match args t with
  | Ok words -> Shellwords.join_command (t.cmd :: words)
  | Error _ ->
      let fixed = Shellwords.join_command (t.cmd :: t.fixed_args) in
      let raw = Editor.to_string t.editor in
      if raw = "" then fixed else fixed ^ " " ^ raw

type effect_ = Schedule_rerun | Start_run
type reaction = Continue of t * effect_ list | Accept_exit | Quit_exit

let max_scroll ~view_h t = max 0 (Array.length t.lines - max 1 view_h)

let clamp_scroll ~view_h t s =
  let s = min s (max_scroll ~view_h t) in
  max 0 s

let with_motion t ed = Continue ({ t with editor = ed }, [])

let with_edit t ed =
  if ed == t.editor then Continue (t, [])
  else
    Continue
      ( {
          t with
          editor = ed;
          parse_error = parse_error_of ~single:t.single (Editor.to_string ed);
          edit_seq = t.edit_seq + 1;
        },
        [ Schedule_rerun ] )

let with_scroll ~view_h t s =
  Continue ({ t with scroll = clamp_scroll ~view_h t s }, [])

let handle_key ~view_h t key =
  let ed = t.editor in
  match key with
  | Insert u -> with_edit t (Editor.insert u ed)
  | Backspace -> with_edit t (Editor.backspace ed)
  | Delete -> with_edit t (Editor.delete ed)
  | Kill_to_end -> with_edit t (Editor.kill_to_end ed)
  | Kill_to_start -> with_edit t (Editor.kill_to_start ed)
  | Kill_prev_word -> with_edit t (Editor.kill_prev_word ed)
  | Left -> with_motion t (Editor.left ed)
  | Right -> with_motion t (Editor.right ed)
  | Home -> with_motion t (Editor.home ed)
  | End -> with_motion t (Editor.end_ ed)
  | Word_left -> with_motion t (Editor.word_left ed)
  | Word_right -> with_motion t (Editor.word_right ed)
  | Enter -> Continue (t, [ Start_run ])
  | Toggle_single ->
      let single = not t.single in
      Continue
        ( {
            t with
            single;
            parse_error = parse_error_of ~single (Editor.to_string ed);
          },
          [ Start_run ] )
  | Scroll_up -> with_scroll ~view_h t (t.scroll - 1)
  | Scroll_down -> with_scroll ~view_h t (t.scroll + 1)
  | Page_up -> with_scroll ~view_h t (t.scroll - max 1 view_h)
  | Page_down -> with_scroll ~view_h t (t.scroll + max 1 view_h)
  | Redraw -> Continue (t, [])
  | Accept -> Accept_exit
  | Quit -> Quit_exit

(** Mark a new run started; the previous output stays visible until the new
    run finishes, so the screen never flashes empty. *)
let start_run t = { t with running = true; gen = t.gen + 1 }

(** Record the outcome of run [gen]; outcomes of superseded runs are
    ignored. *)
let finish_run t ~gen ~lines ~status =
  if gen <> t.gen then t
  else
    let lines =
      if Array.length lines = 0 then [| { kind = Info; text = "(no output)" } |]
      else lines
    in
    { t with running = false; status = Some status; lines; scroll = 0 }
