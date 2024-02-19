
(rule
 (alias parsing-test)
 (deps
  (source_tree ./correct/parsing-test)
  (package base)
  (package core)
  (package menhir))
 (locks /recursive-dune-run)
 (action
  (system "dune build @all @runtest --root ./correct/parsing-test")))

(alias
 (name runtest)
 (deps (alias parsing-test)))
