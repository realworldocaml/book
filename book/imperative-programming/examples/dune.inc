
(rule
 (alias dictionary)
 (deps
  (source_tree ./correct/dictionary)
  (package base)
  (package core)
  (package ppx_jane))
 (action
  (run dune build @all @runtest --root ./correct/dictionary)))

(alias
 (name runtest)
 (deps (alias dictionary)))

(rule
 (alias dlist)
 (deps
  (source_tree ./correct/dlist)
  (package base))
 (action
  (run dune build @all @runtest --root ./correct/dlist)))

(alias
 (name runtest)
 (deps (alias dlist)))

(rule
 (alias time_converter)
 (deps
  (source_tree ./correct/time_converter)
  (package core))
 (action
  (run dune build @all @runtest --root ./correct/time_converter)))

(alias
 (name runtest)
 (deps (alias time_converter)))

(rule
 (alias time_converter2)
 (deps
  (source_tree ./correct/time_converter2)
  (package core))
 (action
  (run dune build @all @runtest --root ./correct/time_converter2)))

(alias
 (name runtest)
 (deps (alias time_converter2)))
