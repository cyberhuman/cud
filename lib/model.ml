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
  | Toggle_single
  | Toggle_focus
  | Scroll_up
  | Scroll_down
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

type focus = F_args | F_fixed

type t = {
  cmd : string option;
      (** [None]: the input line itself is the command to run *)
  fixed_editor : Editor.t;
      (** the fixed arguments as an editable, shell-split line *)
  focus : focus;  (** which editor the cursor is in *)
  placeholder : string option;
      (** xargs -I style: where in [fixed_args] the editable args go *)
  editor : Editor.t;
  lines : line array;  (** output of the last finished run *)
  scroll : int;  (** index of the first visible output line *)
  running : bool;
  status : status option;  (** of the last finished run *)
  parse_error : Shellwords.error option;
  single : bool;
      (** pass the whole line as one argument instead of word-splitting *)
  vim : bool;  (** vim keybindings enabled *)
  enter_accept : bool;  (** Enter accepts and exits instead of re-running *)
  vmode : vmode;
  vpending : vim_pending;
  register : string;  (** last killed/deleted text, for paste/yank *)
  undo : (focus * Editor.t) list;
  redo : (focus * Editor.t) list;
  gen : int;  (** run generation, bumped by [start_run] *)
  edit_seq : int;  (** bumped on every text change, for debouncing *)
}

let parse_error_of ~single text =
  if single then None
  else match Shellwords.split text with Ok _ -> None | Error e -> Some e

let fixed_text t = Editor.to_string t.fixed_editor

let focused t = match t.focus with F_args -> t.editor | F_fixed -> t.fixed_editor

let set_focused t ed =
  match t.focus with
  | F_args -> { t with editor = ed }
  | F_fixed -> { t with fixed_editor = ed }

(** First problem among the two editable lines: the args line, then the
    fixed-args line (only meaningful with a fixed command). *)
let compute_parse_error t =
  match parse_error_of ~single:t.single (Editor.to_string t.editor) with
  | Some _ as e -> e
  | None ->
      if t.cmd = None then None
      else (
        match Shellwords.split (fixed_text t) with
        | Ok _ -> None
        | Error e -> Some e)

let create ?cmd ?placeholder ?(single = false) ?(vim = false)
    ?(enter_accept = false) ~fixed_args
    ~initial () =
  let t =
  {
    cmd;
    fixed_editor = Editor.of_string (Shellwords.join_command fixed_args);
    focus = F_args;
    placeholder;
    editor = Editor.of_string initial;
    lines = [||];
    scroll = 0;
    running = false;
    status = None;
    parse_error = parse_error_of ~single initial;
    single;
    vim;
    enter_accept;
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

(** What to run: program and arguments, or nothing (empty line and no fixed
    command), or a parse error. With no fixed command the line itself is the
    command — its first word in split mode, or [sh -c LINE] in single-arg
    mode. *)
let command t =
  let text = Editor.to_string t.editor in
  match t.cmd with
  | Some cmd -> (
      match Shellwords.split (fixed_text t) with
      | Error e -> Error e
      | Ok fixed ->
          let merge words =
            merge_args ~placeholder:t.placeholder ~fixed words
          in
          if t.single then
            Ok (Some (cmd, merge (if text = "" then [] else [ text ])))
          else (
            match Shellwords.split text with
            | Ok words -> Ok (Some (cmd, merge words))
            | Error e -> Error e))
  | None ->
      if t.single then
        if text = "" then Ok None else Ok (Some ("sh", [ "-c"; text ]))
      else (
        match Shellwords.split text with
        | Ok [] -> Ok None
        | Ok (prog :: args) -> Ok (Some (prog, args))
        | Error e -> Error e)

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
  match command t with
  | Ok (Some (prog, args)) -> Shellwords.join_command (prog :: args)
  | Ok None -> ""
  | Error _ -> (
      let raw = Editor.to_string t.editor in
      match t.cmd with
      | Some cmd ->
          let ft = fixed_text t in
          let fixed = if ft = "" then cmd else cmd ^ " " ^ ft in
          if raw = "" then fixed else fixed ^ " " ^ raw
      | None -> raw)

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
  { t with undo = take undo_depth ((t.focus, focused t) :: t.undo); redo = [] }

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
      if t.enter_accept then Accept_exit else Continue (t, [ Start_run ])
  | Toggle_single ->
      let t = { t with single = not t.single } in
      Continue ({ t with parse_error = compute_parse_error t }, [ Start_run ])
  | Toggle_focus ->
      if t.cmd = None then Continue (t, [])
      else
        let focus = match t.focus with F_args -> F_fixed | F_fixed -> F_args in
        Continue ({ t with focus; vpending = P_none }, [])
  | Scroll_up -> with_scroll ~view_h t (t.scroll - 1)
  | Scroll_down -> with_scroll ~view_h t (t.scroll + 1)
  | Page_up -> with_scroll ~view_h t (t.scroll - max 1 view_h)
  | Page_down -> with_scroll ~view_h t (t.scroll + max 1 view_h)
  | Scroll_top -> with_scroll ~view_h t 0
  | Scroll_bottom -> with_scroll ~view_h t max_int
  | Redraw -> Continue (t, [])
  | Accept -> Accept_exit
  | Quit -> Quit_exit

(* --- input interpretation: emacs layer --- *)

let emacs_action input : key option =
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
      | 'p' -> Some Scroll_up
      | 'n' -> Some Scroll_down
      | 'l' -> Some Redraw
      | 't' -> Some Toggle_single
      | 'd' -> Some Accept
      | 'c' -> Some Quit
      | _ -> None)
  | I_meta c -> (
      match c with
      | 'b' -> Some Word_left
      | 'f' -> Some Word_right
      | _ -> None)
  | I_special s -> (
      match s with
      | S_enter -> Some Enter
      | S_backspace -> Some Backspace
      | S_delete -> Some Delete
      | S_left -> Some Left
      | S_right -> Some Right
      | S_up -> Some Scroll_up
      | S_down -> Some Scroll_down
      | S_home -> Some Home
      | S_end -> Some End
      | S_pgup -> Some Page_up
      | S_pgdn -> Some Page_down
      | S_ctrl_left -> Some Word_left
      | S_ctrl_right -> Some Word_right
      | S_tab -> Some Toggle_focus
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

(* End-of-word position (vim 'e', WORD-wise): the last character of the
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

let editor_at t f =
  match f with F_args -> t.editor | F_fixed -> t.fixed_editor

let vim_undo t =
  match t.undo with
  | [] -> Continue (t, [])
  | (f, ed) :: rest ->
      let prev = (f, editor_at t f) in
      let t = set_focused { t with focus = f } (nclamp ed) in
      let t =
        { t with undo = rest; redo = prev :: t.redo; edit_seq = t.edit_seq + 1 }
      in
      Continue ({ t with parse_error = compute_parse_error t }, [ Schedule_rerun ])

let vim_redo t =
  match t.redo with
  | [] -> Continue (t, [])
  | (f, ed) :: rest ->
      let prev = (f, editor_at t f) in
      let t = set_focused { t with focus = f } (nclamp ed) in
      let t =
        {
          t with
          redo = rest;
          undo = take undo_depth (prev :: t.undo);
          edit_seq = t.edit_seq + 1;
        }
      in
      Continue ({ t with parse_error = compute_parse_error t }, [ Schedule_rerun ])

(* Span of an operator+motion, [lo, hi). 'w' used with the change operator
   behaves like 'e' (standard vim special case). *)
let operator_span t ~op motion =
  let ed = focused t in
  let cur = Editor.cursor ed in
  let len = Editor.length ed in
  match motion with
  | `W when op = 'c' -> Some (cur, word_end_pos ed + 1)
  | `W -> Some (cur, word_fwd_pos ed)
  | `B -> Some (Editor.cursor (Editor.word_left ed), cur)
  | `E -> Some (cur, word_end_pos ed + 1)
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
  | (P_replace | P_find _ | P_op _ | P_g), I_special S_escape, _
  | (P_replace | P_find _ | P_op _ | P_g), _, _ ->
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
      | 'w' -> motion_to (word_fwd_pos ed)
      | 'b' -> motion_to (Editor.cursor (Editor.word_left ed))
      | 'e' -> motion_to (word_end_pos ed)
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
      | 'p' -> vim_paste t ~after:true
      | 'P' -> vim_paste t ~after:false
      | 'u' -> vim_undo t
      | 'j' -> handle_key ~view_h t Scroll_down
      | 'k' -> handle_key ~view_h t Scroll_up
      | 'G' -> handle_key ~view_h t Scroll_bottom
      | 'g' -> Continue ({ t with vpending = P_g }, [])
      | _ -> Continue (t, []))
  | P_none, _, None -> (
      (* control/special keys keep their emacs meaning in normal mode *)
      match emacs_action input with
      | Some (Insert _) | None -> Continue (t, [])
      | Some key -> handle_key ~view_h t key)

(** Entry point for raw input. *)
let handle_input ~view_h t input =
  if not t.vim then
    match emacs_action input with
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
            match emacs_action input with
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
