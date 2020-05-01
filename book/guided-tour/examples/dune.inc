
(rule
 (alias sum)
 (deps
  (source_tree ./correct/sum)
  (package base)
  (package stdio))
 (action
  (run dune build @all @runtest --root ./correct/sum)))

(alias
 (name runtest)
 (deps (alias sum)))
