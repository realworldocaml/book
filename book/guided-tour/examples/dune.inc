
(rule
 (alias sum)
 (deps
  (source_tree ./correct/sum)
  (package base)
  (package stdio))
 (locks /recursive-dune-run)
 (action
  (system "dune build @all @runtest --root ./correct/sum")))

(alias
 (name runtest)
 (deps (alias sum)))
