
(rule
 (alias parsing-test)
 (deps
  (source_tree ./correct/parsing-test)
  (package base)
  (package core)
  (package menhir))
 (action
  (system "dune build @all @runtest --root ./correct/parsing-test")))

(alias
 (name runtest)
 (deps (alias parsing-test)))
