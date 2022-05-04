
(rule
 (alias shapes)
 (deps
  (source_tree ./correct/shapes)
  (package base)
  (package async_graphics))
 (action
  (system "dune build @all @runtest --root ./correct/shapes")))

(alias
 (name runtest)
 (deps (alias shapes)))
