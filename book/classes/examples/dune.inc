
(rule
 (alias shapes)
 (deps
  (source_tree ./correct/shapes)
  (package core)
  (package async)
  (package async_graphics))
 (action
  (system "dune build @all @runtest --root ./correct/shapes")))

(alias
 (name runtest)
 (deps (alias shapes)))

(rule
 (alias verbose_shapes)
 (deps
  (source_tree ./correct/verbose_shapes)
  (package core)
  (package async)
  (package async_graphics))
 (action
  (system "dune build @all @runtest --root ./correct/verbose_shapes")))

(alias
 (name runtest)
 (deps (alias verbose_shapes)))
