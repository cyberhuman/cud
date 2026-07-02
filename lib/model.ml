(** Application state and its pure update function.

    The TUI layer translates terminal events into {!input} values and
    performs the {!effect_}s requested by {!handle_input}; everything here is
    pure and can be exercised in unit tests without a terminal. Key
    interpretation — including the whole vim state machine — lives here. *)

type line_kind = Out | Err | Info
type line = { kind : line_kind; text : string }

type status = Exited of int | Signaled of int

(** Semantic editing/UI actions (the "emacs" layer). *)
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
  | Yank
  | Enter
  | Newline  (** insert a line break (multiline mode only) *)
  | Submit
      (** accept (with --enter-accept) or run — what Enter does, ignoring
          the multiline newline binding *)
  | Toggle_single
  | Toggle_ansi
  | Focus_next  (** Tab: move the focus toward the output *)
  | Focus_prev  (** Shift-Tab: move the focus toward the fixed command *)
  | Step_prev  (** focus the previous/next pipeline step ([--pipe]) *)
  | Step_next
  | Scroll_up
  | Scroll_down
  | Cursor_up
      (** Up/Down (vim [gk]/[gj]): move the cursor vertically through the
          whole input area — across a multiline step's lines, then on into
          the neighbouring step (keeping the on-screen column); scrolls the
          output when there is nowhere to move *)
  | Cursor_down
  | Scroll_output_up  (** always scrolls, even when Up/Down move the cursor *)
  | Scroll_output_down
  | Page_up
  | Page_down
  | Scroll_top
  | Scroll_bottom
  | Accept
  | Quit
  | Redraw

(** Raw terminal input, as delivered by the TUI layer. *)
type ispecial =
  | S_enter
  | S_meta_enter
  | S_mod_up (* Ctrl/Shift+Up: scroll the output regardless of focus *)
  | S_mod_down
  | S_backspace
  | S_delete
  | S_left
  | S_right
  | S_up
  | S_down
  | S_home
  | S_end
  | S_pgup
  | S_pgdn
  | S_ctrl_left
  | S_ctrl_right
  | S_tab
  | S_backtab  (** Shift+Tab (CSI Z) *)
  | S_escape

type input =
  | I_char of Uchar.t  (** printable character, no modifiers *)
  | I_ctrl of char  (** lowercased *)
  | I_meta of char  (** lowercased *)
  | I_special of ispecial

type vmode = V_insert | V_normal

type vim_pending =
  | P_none
  | P_op of char  (** 'd' or 'c' *)
  | P_find of { back : bool; op : char option }
  | P_replace
  | P_g
  | P_z

type focus = F_args | F_fixed | F_output
(** [F_output]: the output viewport has the focus and Up/Down scroll it. *)

(** A second-pane section ([--lens]/[--hint]): a command run with [sh -c]
    over the main command's latest output. Hints additionally re-run as the
    input line or cursor moves, and see CUD_BEFORE/CUD_AFTER/CUD_FIXED/
    CUD_CMD in their environment. *)
type pane = { spec : string; hint : bool; plines : line array }

type step = {
  fixed : Editor.t;
      (** the fixed command line — program and arguments — as an editable,
          shell-split line; empty means the args line itself is the
          command *)
  args : Editor.t;
}
(** One pipeline step ([--pipe]): its fixed command line and its editable
    argument line. Without [--pipe] there is exactly one. *)

type t = {
  steps : step array;  (** the pipeline steps, in order; never empty *)
  cur : int;  (** index of the step being edited *)
  focus : focus;  (** which editor the cursor is in *)
  placeholder : string option;
      (** xargs -I style: where in [fixed_args] the editable args go *)
  lines : line array;  (** output of the last finished run *)
  panes : pane array;  (** lens/hint sections, lenses first *)
  scroll : int;  (** index of the first visible output line *)
  running : bool;
  status : status option;  (** of the last finished run *)
  parse_error : Shellwords.error option;
  single : bool;
      (** pass the whole line as one argument instead of word-splitting *)
  pipefail : bool;
      (** the pipeline fails if any step fails, not just the last one *)
  vim : bool;  (** vim keybindings enabled *)
  enter_accept : bool;  (** Enter accepts and exits instead of re-running *)
  ansi : bool;  (** respect SGR sequences in the output instead of stripping *)
  multiline : bool;  (** the args editor holds multiple lines *)
  vmode : vmode;
  vpending : vim_pending;
  register : string;  (** last killed/deleted text, for paste/yank *)
  undo : (int * focus * Editor.t) list;  (** step index, field, value *)
  redo : (int * focus * Editor.t) list;
  gen : int;  (** run generation, bumped by [start_run] *)
  edit_seq : int;  (** bumped on every text change, for debouncing *)
}

let parse_error_of ~single text =
  if single then None
  else match Shellwords.split text with Ok _ -> None | Error e -> Some e

let nsteps t = Array.length t.steps
let cur_step t = t.steps.(t.cur)
let editor t = (cur_step t).args
let fixed_editor t = (cur_step t).fixed
let fixed_text t = Editor.to_string (fixed_editor t)

let set_step t i st =
  let steps = Array.copy t.steps in
  steps.(i) <- st;
  { t with steps }

let set_editor t ed = set_step t t.cur { (cur_step t) with args = ed }
let set_fixed_editor t ed = set_step t t.cur { (cur_step t) with fixed = ed }

let focused t =
  match t.focus with
  | F_args | F_output -> editor t
  | F_fixed -> fixed_editor t

let set_focused t ed =
  match t.focus with
  | F_args | F_output -> set_editor t ed
  | F_fixed -> set_fixed_editor t ed

(** First problem among a step's two editable lines: the args line, then the
    fixed-args line (only meaningful with a fixed command). *)
let step_parse_error ~single (st : step) =
  match parse_error_of ~single (Editor.to_string st.args) with
  | Some _ as e -> e
  | None -> (
      match Shellwords.split (Editor.to_string st.fixed) with
      | Ok _ -> None
      | Error e -> Some e)

(** First problem across all steps, in pipeline order. *)
let compute_parse_error t =
  Array.fold_left
    (fun acc st ->
      match acc with
      | Some _ -> acc
      | None -> step_parse_error ~single:t.single st)
    None t.steps

(** [steps] ([--pipe]): one [(fixed_line, initial_args)] pair per pipeline
    step; when empty, a single step is built from [cmd]/[fixed_args]/
    [initial]. *)
let create ?cmd ?placeholder ?(single = false) ?(pipefail = false)
    ?(vim = false) ?(enter_accept = false) ?(ansi = false)
    ?(multiline = false) ?(lenses = []) ?(hints = []) ?(steps = [])
    ~fixed_args ~initial () =
  let mk (fixed_line, init) =
    { fixed = Editor.of_string fixed_line; args = Editor.of_string init }
  in
  let steps =
    match steps with
    | [] ->
        [|
          mk
            ( Shellwords.join_command
                (match cmd with Some c -> c :: fixed_args | None -> fixed_args),
              initial );
        |]
    | l -> Array.of_list (List.map mk l)
  in
  let t =
  {
    steps;
    cur = 0;
    focus = F_args;
    placeholder;
    lines = [||];
    panes =
      (let pane hint spec = { spec; hint; plines = [||] } in
       Array.of_list
         (List.map (pane false) lenses @ List.map (pane true) hints));
    scroll = 0;
    running = false;
    status = None;
    parse_error = None;
    single;
    pipefail;
    vim;
    enter_accept;
    ansi;
    multiline;
    vmode = V_insert;
    vpending = P_none;
    register = "";
    undo = [];
    redo = [];
    gen = 0;
    edit_seq = 0;
  }
  in
  { t with parse_error = compute_parse_error t }

let contains_sub s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  m > 0 && go 0

let replace_all s ~sub ~by =
  let b = Buffer.create (String.length s) in
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i >= n then ()
    else if i + m <= n && String.sub s i m = sub then begin
      Buffer.add_string b by;
      go (i + m)
    end
    else begin
      Buffer.add_char b s.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents b

(** Place the editable [words] into [fixed] (xargs -I style): a fixed
    argument that IS the placeholder is replaced by the words, spliced in
    place; one that merely contains it gets an inline textual substitution
    (words joined with spaces). Without a placeholder — or if it occurs
    nowhere — the words are appended at the end. *)
let merge_args ~placeholder ~fixed words =
  match placeholder with
  | Some ph when ph <> "" && List.exists (fun a -> contains_sub a ph) fixed ->
      let joined = String.concat " " words in
      List.concat_map
        (fun a ->
          if a = ph then words
          else if contains_sub a ph then [ replace_all a ~sub:ph ~by:joined ]
          else [ a ])
        fixed
  | _ -> fixed @ words

(** What one step runs: program and arguments, or nothing (empty line and no
    fixed command), or a parse error. With no fixed command the line itself
    is the command — its first word in split mode, or [sh -c LINE] in
    single-arg mode. *)
let step_command ~single ~placeholder (st : step) =
  let text = Editor.to_string st.args in
  match Shellwords.split (Editor.to_string st.fixed) with
  | Error e -> Error e
  | Ok [] ->
      (* no fixed command line: the args line is the whole command *)
      if single then
        if text = "" then Ok None else Ok (Some ("sh", [ "-c"; text ]))
      else (
        match Shellwords.split text with
        | Ok [] -> Ok None
        | Ok (prog :: args) -> Ok (Some (prog, args))
        | Error e -> Error e)
  | Ok (prog :: fixed) ->
      let merge words = merge_args ~placeholder ~fixed words in
      if single then
        Ok (Some (prog, merge (if text = "" then [] else [ text ])))
      else (
        match Shellwords.split text with
        | Ok words -> Ok (Some (prog, merge words))
        | Error e -> Error e)

(** What to run. With one step, its command directly; with several
    ([--pipe]), the non-empty steps joined into a shell pipeline run via
    [sh -c] (a step still being empty simply drops out). *)
let command t =
  let step_command st =
    step_command ~single:t.single ~placeholder:t.placeholder st
  in
  if nsteps t = 1 then step_command t.steps.(0)
  else
    let rec frags acc = function
      | [] -> Ok (List.rev acc)
      | st :: rest -> (
          match step_command st with
          | Error e -> Error e
          | Ok None -> frags acc rest
          | Ok (Some (prog, args)) ->
              frags (Shellwords.join_command (prog :: args) :: acc) rest)
    in
    match frags [] (Array.to_list t.steps) with
    | Error e -> Error e
    | Ok [] -> Ok None
    | Ok fs ->
        let joined = String.concat " | " fs in
        let joined =
          if t.pipefail then "set -o pipefail; " ^ joined else joined
        in
        Ok (Some ("sh", [ "-c"; joined ]))

(** The command of the step being edited (for the hint environment). *)
let current_command t =
  step_command ~single:t.single ~placeholder:t.placeholder (cur_step t)

(** The arguments the user provided in a step's editor, for printing at
    exit. In [single] mode the line is one argument; in split mode an
    unparseable line is returned as-is, best effort. *)
let step_user_args ~single (st : step) =
  let text = Editor.to_string st.args in
  if single then (if text = "" then [] else [ text ])
  else
    match Shellwords.split text with
    | Ok words -> words
    | Error _ -> [ text ]

let user_args t = step_user_args ~single:t.single (cur_step t)

(** Every step's arguments, in pipeline order. *)
let all_args t =
  Array.to_list t.steps |> List.map (step_user_args ~single:t.single)

let step_command_string t (st : step) =
  match step_command ~single:t.single ~placeholder:t.placeholder st with
  | Ok (Some (prog, args)) -> Shellwords.join_command (prog :: args)
  | Ok None -> ""
  | Error _ ->
      let raw = Editor.to_string st.args in
      let ft = Editor.to_string st.fixed in
      if ft = "" then raw else if raw = "" then ft else ft ^ " " ^ raw

(** The full command (pipeline) as a shell-quoted string. If an input line
    doesn't parse, it is appended verbatim. *)
let command_string t =
  if nsteps t = 1 then step_command_string t t.steps.(0)
  else
    Array.to_list t.steps
    |> List.map (step_command_string t)
    |> List.filter (fun s -> s <> "")
    |> String.concat " | "

type effect_ = Schedule_rerun | Start_run
type reaction = Continue of t * effect_ list | Accept_exit | Quit_exit

let max_scroll ~view_h t = max 0 (Array.length t.lines - max 1 view_h)

let clamp_scroll ~view_h t s =
  let s = min s (max_scroll ~view_h t) in
  max 0 s

let with_motion t ed = Continue (set_focused t ed, [])

let undo_depth = 100

let take n l = List.filteri (fun i _ -> i < n) l

let push_undo t =
  {
    t with
    undo = take undo_depth ((t.cur, t.focus, focused t) :: t.undo);
    redo = [];
  }

(** Apply an edited editor value: snapshot for undo, recompute the parse
    state, request a (debounced) re-run. [register] records killed text. *)
let with_edit ?register t ed =
  if ed == focused t then Continue (t, [])
  else
    let t = set_focused (push_undo t) ed in
    let t =
      {
        t with
        register = Option.value register ~default:t.register;
        edit_seq = t.edit_seq + 1;
      }
    in
    Continue ({ t with parse_error = compute_parse_error t }, [ Schedule_rerun ])

let with_scroll ~view_h t s =
  Continue ({ t with scroll = clamp_scroll ~view_h t s }, [])

(* --- multiline cursor movement --- *)

let nl_uchar = Uchar.of_char '\n'

(* start/end position of the line containing [pos] in [us] *)
let line_start_of us pos =
  let rec go i =
    if i <= 0 then 0
    else if Uchar.equal us.(i - 1) nl_uchar then i
    else go (i - 1)
  in
  go pos

let line_end_of us pos =
  let len = Array.length us in
  let rec go i =
    if i >= len then len
    else if Uchar.equal us.(i) nl_uchar then i
    else go (i + 1)
  in
  go pos

(** Same text, cursor at column [col] of the line it is on (clamped to the
    line's length). *)
let with_col ed col =
  let us = Array.of_list (Editor.to_uchars ed) in
  let cur = Editor.cursor ed in
  let start = line_start_of us cur in
  Editor.with_cursor ed (start + min col (line_end_of us cur - start))

(** Move the cursor one line up ([dir = -1]) or down ([dir = 1]) within a
    multi-line text, preserving the column when possible (clamped to the
    target line's length). At the first/last line the editor is returned
    unchanged. *)
let cursor_vert ~dir ed =
  let us = Array.of_list (Editor.to_uchars ed) in
  let len = Array.length us in
  let cur = Editor.cursor ed in
  let start = line_start_of us cur in
  let col = cur - start in
  if dir < 0 then
    if start = 0 then ed
    else
      let pstart = line_start_of us (start - 1) in
      Editor.with_cursor ed (pstart + min col (start - 1 - pstart))
  else
    let e = line_end_of us cur in
    if e >= len then ed
    else
      let nstart = e + 1 in
      Editor.with_cursor ed (nstart + min col (line_end_of us nstart - nstart))

(* The on-screen width of a step's prompt region: the fixed command line
   plus "> " (mirroring the renderer). *)
let prompt_width t i =
  let flen = Editor.length t.steps.(i).fixed in
  if flen = 0 then 2 else flen + 2

(** On-screen column of the current step's args cursor: the prompt width on
    the step's first line, the 2-column indent on continuation lines. *)
let args_visual_col t =
  let ed = editor t in
  let us = Array.of_list (Editor.to_uchars ed) in
  let cur = Editor.cursor ed in
  let start = line_start_of us cur in
  (if start = 0 then prompt_width t t.cur else 2) + (cur - start)

(** Put the current step's args cursor at on-screen column [vcol], clamped
    into the line it is on. *)
let args_to_visual t vcol =
  let ed = editor t in
  let start =
    line_start_of (Array.of_list (Editor.to_uchars ed)) (Editor.cursor ed)
  in
  let off = if start = 0 then prompt_width t t.cur else 2 in
  set_editor t (with_col ed (max 0 (vcol - off)))

(* --- semantic actions --- *)

let handle_key ~view_h t key =
  let ed = focused t in
  let text_of us =
    let b = Buffer.create 32 in
    List.iter (Buffer.add_utf_8_uchar b) us;
    Buffer.contents b
  in
  let killed lo hi =
    let us = Editor.to_uchars ed in
    text_of (take (hi - lo) (List.filteri (fun i _ -> i >= lo) us))
  in
  let cur = Editor.cursor ed in
  let len = Editor.length ed in
  match key with
  | Insert u -> with_edit t (Editor.insert u ed)
  | Backspace -> with_edit t (Editor.backspace ed)
  | Delete -> with_edit t (Editor.delete ed)
  | Kill_to_end -> with_edit ~register:(killed cur len) t (Editor.kill_to_end ed)
  | Kill_to_start -> with_edit ~register:(killed 0 cur) t (Editor.kill_to_start ed)
  | Kill_prev_word ->
      let ed' = Editor.kill_prev_word ed in
      with_edit ~register:(killed (Editor.cursor ed') cur) t ed'
  | Yank ->
      if t.register = "" then Continue (t, [])
      else with_edit t (Editor.insert_string t.register ed)
  | Left -> with_motion t (Editor.left ed)
  | Right -> with_motion t (Editor.right ed)
  | Home -> with_motion t (Editor.home ed)
  | End -> with_motion t (Editor.end_ ed)
  | Word_left -> with_motion t (Editor.word_left ed)
  | Word_right -> with_motion t (Editor.word_right ed)
  | Enter ->
      (* priority: multiline newline > enter-accept > run *)
      if
        t.multiline && t.focus = F_args
        && not (t.vim && t.vmode = V_normal)
      then with_edit t (Editor.insert nl_uchar ed)
      else if t.enter_accept then Accept_exit
      else Continue (t, [ Start_run ])
  | Submit ->
      if t.enter_accept then Accept_exit else Continue (t, [ Start_run ])
  | Newline ->
      if t.multiline && t.focus = F_args then with_edit t (Editor.insert nl_uchar ed)
      else Continue (t, [])
  | Toggle_single ->
      let t = { t with single = not t.single } in
      Continue ({ t with parse_error = compute_parse_error t }, [ Start_run ])
  | Toggle_ansi -> Continue ({ t with ansi = not t.ansi }, [])
  | Step_prev | Step_next ->
      let target =
        let d = if key = Step_next then 1 else -1 in
        max 0 (min (nsteps t - 1) (t.cur + d))
      in
      if target = t.cur then Continue (t, [])
      else
        (* landing on another step keeps the cursor in the same field, at
           the same on-screen column (the prompts differ in width) *)
        let focus = match t.focus with F_output -> F_args | f -> f in
        if focus = F_fixed then
          (* the fixed region starts at column 0 in every step *)
          let col = Editor.cursor (fixed_editor t) in
          let t = { t with cur = target; vpending = P_none } in
          Continue (set_focused t (with_col (focused t) col), [])
        else
          let vcol = args_visual_col t in
          let t = { t with cur = target; focus; vpending = P_none } in
          Continue (args_to_visual t vcol, [])
  | Cursor_up | Cursor_down ->
      let dir = if key = Cursor_down then 1 else -1 in
      if t.focus <> F_args then with_scroll ~view_h t (t.scroll + dir)
      else
        let moved = if t.multiline then cursor_vert ~dir ed else ed in
        if moved != ed then with_motion t moved
        else
          let target = t.cur + dir in
          if target < 0 || target >= nsteps t then
            (* nothing above/below: scroll, unless motion keys would be
               expected to stop (several steps or several lines) *)
            if nsteps t > 1 || t.multiline then Continue (t, [])
            else with_scroll ~view_h t (t.scroll + dir)
          else
            (* cross into the adjacent step: nearest line, same on-screen
               column *)
            let vcol = args_visual_col t in
            let ted = t.steps.(target).args in
            let ted =
              Editor.with_cursor ted
                (if dir < 0 then Editor.length ted else 0)
            in
            let t = { t with cur = target; vpending = P_none } in
            Continue (args_to_visual (set_editor t ted) vcol, [])
  | Focus_next | Focus_prev ->
      (* the regions in on-screen order — fixed command, args, output; Tab
         moves toward the output, Shift-Tab toward the command, stopping at
         the ends *)
      let focus =
        if key = Focus_next then
          match t.focus with
          | F_fixed -> F_args
          | F_args | F_output -> F_output
        else
          match t.focus with
          | F_output -> F_args
          | F_args | F_fixed -> F_fixed
      in
      if focus = t.focus then Continue (t, [])
      else Continue ({ t with focus; vpending = P_none }, [])
  | Scroll_up ->
      if t.multiline && t.focus = F_args then
        with_motion t (cursor_vert ~dir:(-1) ed)
      else with_scroll ~view_h t (t.scroll - 1)
  | Scroll_down ->
      if t.multiline && t.focus = F_args then
        with_motion t (cursor_vert ~dir:1 ed)
      else with_scroll ~view_h t (t.scroll + 1)
  | Scroll_output_up -> with_scroll ~view_h t (t.scroll - 1)
  | Scroll_output_down -> with_scroll ~view_h t (t.scroll + 1)
  | Page_up -> with_scroll ~view_h t (t.scroll - max 1 view_h)
  | Page_down -> with_scroll ~view_h t (t.scroll + max 1 view_h)
  | Scroll_top -> with_scroll ~view_h t 0
  | Scroll_bottom -> with_scroll ~view_h t max_int
  | Redraw -> Continue (t, [])
  | Accept -> Accept_exit
  | Quit -> Quit_exit

(* --- input interpretation: emacs layer --- *)

(** [pipe]: more than one pipeline step exists, so C-p/C-n switch steps
    instead of scrolling. *)
let emacs_action ~pipe input : key option =
  match input with
  | I_char u -> Some (Insert u)
  | I_ctrl c -> (
      match c with
      | 'a' -> Some Home
      | 'e' -> Some End
      | 'b' -> Some Left
      | 'f' -> Some Right
      | 'k' -> Some Kill_to_end
      | 'u' -> Some Kill_to_start
      | 'w' -> Some Kill_prev_word
      | 'y' -> Some Yank
      | 'p' -> Some (if pipe then Step_prev else Scroll_up)
      | 'n' -> Some (if pipe then Step_next else Scroll_down)
      | 'l' -> Some Redraw
      | 'o' -> Some Submit
      | 't' -> Some Toggle_single
      | 'd' -> Some Accept
      | 'c' -> Some Quit
      | _ -> None)
  | I_meta c -> (
      match c with
      | 'a' -> Some Toggle_ansi
      | 'b' -> Some Word_left
      | 'f' -> Some Word_right
      | _ -> None)
  | I_special s -> (
      match s with
      | S_enter -> Some Enter
      | S_meta_enter -> Some Submit
      | S_mod_up -> Some Scroll_output_up
      | S_mod_down -> Some Scroll_output_down
      | S_backspace -> Some Backspace
      | S_delete -> Some Delete
      | S_left -> Some Left
      | S_right -> Some Right
      | S_up -> Some Cursor_up
      | S_down -> Some Cursor_down
      | S_home -> Some Home
      | S_end -> Some End
      | S_pgup -> Some Page_up
      | S_pgdn -> Some Page_down
      | S_ctrl_left -> Some Word_left
      | S_ctrl_right -> Some Word_right
      | S_tab -> Some Focus_next
      | S_backtab -> Some Focus_prev
      | S_escape -> Some Quit)

(* --- input interpretation: vim layer --- *)

(* In normal mode the cursor sits on a character: clamp to len-1. *)
let nclamp ed =
  let len = Editor.length ed in
  if Editor.cursor ed >= len && len > 0 then Editor.with_cursor ed (len - 1)
  else ed

let uchars_array ed = Array.of_list (Editor.to_uchars ed)

let string_of_slice us lo hi =
  let b = Buffer.create 32 in
  for i = lo to hi - 1 do
    Buffer.add_utf_8_uchar b us.(i)
  done;
  Buffer.contents b

(* Start of the next word (vim 'w', WORD-wise) — unlike the emacs-style
   [Editor.word_right], which stops at the end of the current word. *)
let word_fwd_pos ed =
  let us = uchars_array ed in
  let len = Array.length us in
  let blank i = Editor.is_blank us.(i) in
  let i = ref (Editor.cursor ed) in
  while !i < len && not (blank !i) do incr i done;
  while !i < len && blank !i do incr i done;
  !i

(* vim's small-word character classes: blanks separate everything; word
   characters (alphanumerics, '_', non-ASCII) and punctuation form distinct
   runs. *)
type wclass = C_blank | C_word | C_punct

let wclass u =
  if Editor.is_blank u then C_blank
  else
    let c = Uchar.to_int u in
    if
      (c >= 0x30 && c <= 0x39)
      || (c >= 0x41 && c <= 0x5a)
      || (c >= 0x61 && c <= 0x7a)
      || c = 0x5f || c >= 0x80
    then C_word
    else C_punct

(* Start of the next small word (vim 'w'). *)
let small_fwd_pos ed =
  let us = uchars_array ed in
  let len = Array.length us in
  let i = ref (Editor.cursor ed) in
  if !i < len then begin
    let k = wclass us.(!i) in
    if k <> C_blank then
      while !i < len && wclass us.(!i) = k do incr i done;
    while !i < len && wclass us.(!i) = C_blank do incr i done
  end;
  !i

(* End of the current/next small word (vim 'e'). *)
let small_end_pos ed =
  let us = uchars_array ed in
  let len = Array.length us in
  let i = ref (Editor.cursor ed + 1) in
  while !i < len && wclass us.(!i) = C_blank do incr i done;
  if !i >= len then max 0 (len - 1)
  else begin
    let k = wclass us.(!i) in
    while !i + 1 < len && wclass us.(!i + 1) = k do incr i done;
    !i
  end

(* Start of the previous small word (vim 'b'). *)
let small_back_pos ed =
  let us = uchars_array ed in
  let i = ref (Editor.cursor ed - 1) in
  while !i >= 0 && wclass us.(!i) = C_blank do decr i done;
  if !i < 0 then 0
  else begin
    let k = wclass us.(!i) in
    while !i - 1 >= 0 && wclass us.(!i - 1) = k do decr i done;
    max 0 !i
  end

(* End-of-word position (vim 'E', WORD-wise): the last character of the
   current/next word. *)
let word_end_pos ed =
  let us = uchars_array ed in
  let len = Array.length us in
  let blank i = Editor.is_blank us.(i) in
  let i = ref (Editor.cursor ed + 1) in
  while !i < len && blank !i do incr i done;
  if !i >= len then len - 1
  else begin
    while !i + 1 < len && not (blank (!i + 1)) do incr i done;
    !i
  end

let find_char ed ~back c =
  let us = uchars_array ed in
  let len = Array.length us in
  let cur = Editor.cursor ed in
  let rec fwd i = if i >= len then None else if Uchar.equal us.(i) c then Some i else fwd (i + 1) in
  let rec bwd i = if i < 0 then None else if Uchar.equal us.(i) c then Some i else bwd (i - 1) in
  if back then bwd (cur - 1) else fwd (cur + 1)

let enter_insert t ed =
  let t = set_focused t ed in
  { t with vmode = V_insert; vpending = P_none }

(* Delete [lo, hi) from the line into the register; change-ops continue in
   insert mode at [lo]. *)
let vim_delete_span ?(change = false) t lo hi =
  let us = uchars_array (focused t) in
  let len = Array.length us in
  let lo = max 0 lo and hi = min len hi in
  if hi <= lo then Continue ({ t with vpending = P_none }, [])
  else begin
    let removed = string_of_slice us lo hi in
    let kept = string_of_slice us 0 lo ^ string_of_slice us hi len in
    let ed' = Editor.with_cursor (Editor.of_string kept) lo in
    let ed' = if change then ed' else nclamp ed' in
    let t = { t with vpending = P_none; vmode = (if change then V_insert else t.vmode) } in
    with_edit ~register:removed t ed'
  end

let vim_paste t ~after =
  if t.register = "" then Continue (t, [])
  else begin
    let us = uchars_array (focused t) in
    let len = Array.length us in
    let cur = Editor.cursor (focused t) in
    let pos = if after && len > 0 then cur + 1 else cur in
    let text = string_of_slice us 0 pos ^ t.register ^ string_of_slice us pos len in
    let reg_len = List.length (Editor.to_uchars (Editor.of_string t.register)) in
    let ed' = Editor.with_cursor (Editor.of_string text) (pos + reg_len - 1) in
    with_edit t ed'
  end

let editor_at t i f =
  match f with F_args | F_output -> t.steps.(i).args | F_fixed -> t.steps.(i).fixed

let vim_undo t =
  match t.undo with
  | [] -> Continue (t, [])
  | (i, f, ed) :: rest ->
      let prev = (i, f, editor_at t i f) in
      let t = set_focused { t with cur = i; focus = f } (nclamp ed) in
      let t =
        { t with undo = rest; redo = prev :: t.redo; edit_seq = t.edit_seq + 1 }
      in
      Continue ({ t with parse_error = compute_parse_error t }, [ Schedule_rerun ])

let vim_redo t =
  match t.redo with
  | [] -> Continue (t, [])
  | (i, f, ed) :: rest ->
      let prev = (i, f, editor_at t i f) in
      let t = set_focused { t with cur = i; focus = f } (nclamp ed) in
      let t =
        {
          t with
          redo = rest;
          undo = take undo_depth (prev :: t.undo);
          edit_seq = t.edit_seq + 1;
        }
      in
      Continue ({ t with parse_error = compute_parse_error t }, [ Schedule_rerun ])

(* vim 'o'/'O' (multiline mode): open a new line below/above the cursor
   line and enter insert mode. *)
let vim_open_line t ~above =
  if not (t.multiline && t.focus = F_args) then Continue (t, [])
  else begin
    let ed = focused t in
    let us = uchars_array ed in
    let len = Array.length us in
    let cur = min (Editor.cursor ed) len in
    let is_nl i = Uchar.to_int us.(i) = 0x0a in
    let rec find_start i =
      if i > 0 && not (is_nl (i - 1)) then find_start (i - 1) else i
    in
    let rec find_end i =
      if i < len && not (is_nl i) then find_end (i + 1) else i
    in
    let pos, cursor =
      if above then
        let st = find_start cur in
        (st, st)
      else
        let en = find_end cur in
        (en, en + 1)
    in
    let text =
      string_of_slice us 0 pos ^ "\n" ^ string_of_slice us pos len
    in
    let ed' = Editor.with_cursor (Editor.of_string text) cursor in
    let t = { t with vmode = V_insert; vpending = P_none } in
    with_edit t ed'
  end

(* Span of an operator+motion, [lo, hi). 'w' used with the change operator
   behaves like 'e' (standard vim special case). *)
let operator_span t ~op motion =
  let ed = focused t in
  let cur = Editor.cursor ed in
  let len = Editor.length ed in
  match motion with
  | `W when op = 'c' -> Some (cur, small_end_pos ed + 1) (* cw acts as ce *)
  | `W -> Some (cur, small_fwd_pos ed)
  | `B -> Some (small_back_pos ed, cur)
  | `E -> Some (cur, small_end_pos ed + 1)
  | `WW when op = 'c' -> Some (cur, word_end_pos ed + 1) (* cW acts as cE *)
  | `WW -> Some (cur, word_fwd_pos ed)
  | `BB -> Some (Editor.cursor (Editor.word_left ed), cur)
  | `EE -> Some (cur, word_end_pos ed + 1)
  | `H -> Some (cur - 1, cur)
  | `L -> Some (cur, cur + 1)
  | `Zero -> Some (0, cur)
  | `Dollar -> Some (cur, len)
  | `Line -> Some (0, len)
  | `Find (back, c) -> (
      match find_char ed ~back c with
      | None -> None
      | Some idx -> if back then Some (idx, cur) else Some (cur, idx + 1))

let vim_apply_op t ~op motion =
  match operator_span t ~op motion with
  | None -> Continue ({ t with vpending = P_none }, [])
  | Some (lo, hi) -> vim_delete_span ~change:(op = 'c') t lo hi

let motion_of_char c =
  match c with
  | 'w' -> Some `W
  | 'b' -> Some `B
  | 'e' -> Some `E
  | 'W' -> Some `WW
  | 'B' -> Some `BB
  | 'E' -> Some `EE
  | 'h' -> Some `H
  | 'l' -> Some `L
  | '0' | '^' -> Some `Zero
  | '$' -> Some `Dollar
  | _ -> None

let vim_normal ~view_h t input =
  let ed = focused t in
  let cur = Editor.cursor ed in
  let len = Editor.length ed in
  let clear_pending = { t with vpending = P_none } in
  let motion_to pos =
    with_motion clear_pending
      (nclamp (Editor.with_cursor ed (max 0 (min (max 0 (len - 1)) pos))))
  in
  let char_input =
    match input with
    | I_char u when Uchar.is_char u -> Some (Uchar.to_char u)
    | _ -> None
  in
  match (t.vpending, input, char_input) with
  (* pending argument characters *)
  | P_replace, I_char u, _ ->
      if len = 0 then Continue (clear_pending, [])
      else begin
        let us = uchars_array ed in
        us.(cur) <- u;
        let text = string_of_slice us 0 len in
        with_edit clear_pending (Editor.with_cursor (Editor.of_string text) cur)
      end
  | P_find { back; op }, I_char u, _ -> (
      match op with
      | None -> (
          match find_char ed ~back u with
          | None -> Continue (clear_pending, [])
          | Some idx -> motion_to idx)
      | Some op -> vim_apply_op clear_pending ~op (`Find (back, u)))
  | P_op op, _, Some c when c = op -> vim_apply_op clear_pending ~op `Line
  | P_op op, _, Some ('f' | 'F') ->
      let back = char_input = Some 'F' in
      Continue ({ t with vpending = P_find { back; op = Some op } }, [])
  | P_op op, _, Some c -> (
      match motion_of_char c with
      | Some m -> vim_apply_op clear_pending ~op m
      | None -> Continue (clear_pending, []))
  | P_g, _, Some 'g' -> handle_key ~view_h clear_pending Scroll_top
  | P_g, _, Some 'j' -> handle_key ~view_h clear_pending Cursor_down
  | P_g, _, Some 'k' -> handle_key ~view_h clear_pending Cursor_up
  | P_z, _, Some 'Z' -> Accept_exit (* ZZ: accept and exit *)
  | P_z, _, Some 'Q' -> Quit_exit (* ZQ: cancel *)
  | (P_replace | P_find _ | P_op _ | P_g | P_z), I_special S_escape, _
  | (P_replace | P_find _ | P_op _ | P_g | P_z), _, _ ->
      Continue (clear_pending, [])
  (* no pending: normal-mode commands *)
  | P_none, I_special S_escape, _ -> Continue (t, [])
  | P_none, I_ctrl 'r', _ -> vim_redo t
  | P_none, _, Some c -> (
      match c with
      | 'i' -> Continue (enter_insert t ed, [])
      | 'a' ->
          Continue (enter_insert t (Editor.with_cursor ed (min len (cur + 1))), [])
      | 'I' -> Continue (enter_insert t (Editor.home ed), [])
      | 'A' -> Continue (enter_insert t (Editor.end_ ed), [])
      | 'h' -> motion_to (cur - 1)
      | 'l' -> motion_to (cur + 1)
      | '0' | '^' -> motion_to 0
      | '$' -> motion_to (max 0 (len - 1))
      | 'w' -> motion_to (small_fwd_pos ed)
      | 'b' -> motion_to (small_back_pos ed)
      | 'e' -> motion_to (small_end_pos ed)
      | 'W' -> motion_to (word_fwd_pos ed)
      | 'B' -> motion_to (Editor.cursor (Editor.word_left ed))
      | 'E' -> motion_to (word_end_pos ed)
      | 'x' -> if len = 0 then Continue (t, []) else vim_delete_span t cur (cur + 1)
      | 'X' -> if cur = 0 then Continue (t, []) else vim_delete_span t (cur - 1) cur
      | 'D' -> vim_delete_span t cur len
      | 'C' -> vim_delete_span ~change:true t cur len
      | 'd' -> Continue ({ t with vpending = P_op 'd' }, [])
      | 'c' -> Continue ({ t with vpending = P_op 'c' }, [])
      | 's' -> vim_delete_span ~change:true t cur (cur + 1)
      | 'r' -> Continue ({ t with vpending = P_replace }, [])
      | 'f' -> Continue ({ t with vpending = P_find { back = false; op = None } }, [])
      | 'F' -> Continue ({ t with vpending = P_find { back = true; op = None } }, [])
      | 'o' -> vim_open_line t ~above:false
      | 'O' -> vim_open_line t ~above:true
      | 'p' -> vim_paste t ~after:true
      | 'P' -> vim_paste t ~after:false
      | 'u' -> vim_undo t
      | 'j' -> handle_key ~view_h t Scroll_down
      | 'k' -> handle_key ~view_h t Scroll_up
      | 'J' -> handle_key ~view_h t Step_next
      | 'K' -> handle_key ~view_h t Step_prev
      | 'G' -> handle_key ~view_h t Scroll_bottom
      | 'g' -> Continue ({ t with vpending = P_g }, [])
      | 'Z' -> Continue ({ t with vpending = P_z }, [])
      | _ -> Continue (t, []))
  | P_none, _, None -> (
      (* control/special keys keep their emacs meaning in normal mode *)
      match emacs_action ~pipe:(nsteps t > 1) input with
      | Some (Insert _) | None -> Continue (t, [])
      | Some key -> handle_key ~view_h t key)

(* With the output focused, editing keys are ignored: Up/Down and PgUp/PgDn
   scroll, Shift-Tab moves back, and the global keys (Enter, C-t, C-d, C-c,
   ...) keep working. *)
let handle_output_focus ~view_h t input =
  match input with
  | I_special S_escape when t.vim ->
      (* Escape never quits in vim mode *)
      Continue ({ t with vmode = V_normal; vpending = P_none }, [])
  | _ -> (
      match emacs_action ~pipe:(nsteps t > 1) input with
      | Some
          (( Scroll_up | Scroll_down | Cursor_up | Cursor_down
           | Scroll_output_up | Scroll_output_down
           | Page_up | Page_down | Toggle_single | Toggle_ansi
           | Focus_next | Focus_prev | Step_prev | Step_next | Enter | Submit
           | Redraw | Accept | Quit ) as key)
        ->
          handle_key ~view_h t key
      | _ -> Continue (t, []))

(** Entry point for raw input. *)
let handle_input ~view_h t input =
  if t.focus = F_output then handle_output_focus ~view_h t input
  else if not t.vim then
    match emacs_action ~pipe:(nsteps t > 1) input with
    | Some key -> handle_key ~view_h t key
    | None -> Continue (t, [])
  else
    match t.vmode with
    | V_normal -> vim_normal ~view_h t input
    | V_insert -> (
        match input with
        | I_special S_escape ->
            (* leaving insert mode steps the cursor back onto a character *)
            let cur = focused t in
            let ed = if Editor.cursor cur > 0 then Editor.left cur else cur in
            let t = set_focused t (nclamp ed) in
            Continue ({ t with vmode = V_normal }, [])
        | _ -> (
            match emacs_action ~pipe:(nsteps t > 1) input with
            | Some key -> handle_key ~view_h t key
            | None -> Continue (t, [])))

(** Mark a new run started; the previous output stays visible until the new
    run finishes, so the screen never flashes empty. *)
let start_run t = { t with running = true; gen = t.gen + 1 }

(** Nothing to run (empty command line): show a note instead. Bumps the
    generation so an in-flight run's outcome is discarded. *)
let set_idle t ~note =
  {
    (start_run t) with
    running = false;
    status = None;
    lines = [| { kind = Info; text = note } |];
    scroll = 0;
  }

(** Replace the output of pane [i]. *)
let set_pane t i plines =
  let panes = Array.copy t.panes in
  panes.(i) <- { panes.(i) with plines };
  { t with panes }

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
