
(rule
 (alias sum)
 (locks /global)
 (deps
  (source_tree ./correct/sum)
  (package base)
  (package stdio))
 (action
  (system "dune build @all @runtest --root ./correct/sum")))

(alias
 (name runtest)
 (deps (alias sum)))
