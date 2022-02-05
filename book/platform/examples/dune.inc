
(rule
 (alias hello)
 (deps
  (source_tree ./correct/hello)
  (package alcotest))
 (action
  (system "dune build @all @runtest --root ./correct/hello")))

(alias
 (name runtest)
 (deps (alias hello)))
