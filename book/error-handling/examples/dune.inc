
(rule
 (alias blow_up)
 (deps
  (source_tree ./correct/blow_up)
  (package base)
  (package stdio))
 (action
  (run dune build @all @runtest --root ./correct/blow_up)))

(alias
 (name runtest)
 (deps (alias blow_up)))

(rule
 (alias exn_cost)
 (deps
  (source_tree ./correct/exn_cost)
  (package core)
  (package core_bench))
 (action
  (run dune build @all @runtest --root ./correct/exn_cost)))

(alias
 (name runtest)
 (deps (alias exn_cost)))
