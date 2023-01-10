
(rule
 (alias dictionary)
 (locks /global)
 (deps
  (source_tree ./correct/dictionary)
  (package base)
  (package core)
  (package ppx_jane))
 (action
  (system "dune build @all @runtest --root ./correct/dictionary")))

(alias
 (name runtest)
 (deps (alias dictionary)))

(rule
 (alias dlist)
 (locks /global)
 (deps
  (source_tree ./correct/dlist)
  (package base))
 (action
  (system "dune build @all @runtest --root ./correct/dlist")))

(alias
 (name runtest)
 (deps (alias dlist)))

(rule
 (alias time_converter)
 (locks /global)
 (deps
  (source_tree ./correct/time_converter)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/time_converter")))

(alias
 (name runtest)
 (deps (alias time_converter)))

(rule
 (alias time_converter2)
 (locks /global)
 (deps
  (source_tree ./correct/time_converter2)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/time_converter2")))

(alias
 (name runtest)
 (deps (alias time_converter2)))
