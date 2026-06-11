(lang dune 3.8)
(name cud)

(generate_opam_files true)

(source (github cyberhuman/cud))
(authors "Raman Varabets")
(maintainers "Raman Varabets")
(license GPL-3.0-or-later)

(package
 (name cud)
 (synopsis
  "Chew your input again and again: edit command arguments and re-run with captured stdin")
 (description
  "cud captures its standard input, then runs a command in a full-screen UI: the top line edits the command's arguments, the rest of the screen shows its output. Every edit re-runs the command with the captured input replayed. Supports shell-style word splitting or single-argument mode, xargs-style -I placeholders, vim keybindings, and prints the final arguments on exit.")
 (depends
  (ocaml (>= 5.2))
  (notty-community (>= 0.2.4))
  (lwt (>= 5.0))
  (cmdliner (>= 1.3))))
