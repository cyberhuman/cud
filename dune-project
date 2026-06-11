(lang dune 3.8)
(name ine)

(package
 (name ine)
 (synopsis "INteractive Executor: edit command arguments and re-run with captured stdin")
 (depends ocaml dune notty lwt cmdliner))
