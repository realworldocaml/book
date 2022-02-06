
(rule
 (alias hello)
 (deps
  (source_tree ./correct/hello)
  (package base)
  (package ppx_inline_test))
 (action
  (system "dune build @all @runtest --root ./correct/hello")))

(alias
 (name runtest)
 (deps (alias hello)))
