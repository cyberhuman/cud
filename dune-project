(lang dune 3.8)
(name cud)

(package
 (name cud)
 (synopsis "Chew your input again and again: edit command arguments and re-run with captured stdin")
 (depends ocaml dune notty lwt cmdliner))
