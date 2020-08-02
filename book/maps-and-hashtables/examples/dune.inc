
(rule
 (alias freq-fast)
 (deps
  (source_tree ./correct/freq-fast)
  (package base)
  (package stdio))
 (action
  (run dune build @all @runtest --root ./correct/freq-fast)))

(alias
 (name runtest)
 (deps (alias freq-fast)))

(rule
 (alias map_vs_hash)
 (deps
  (source_tree ./correct/map_vs_hash)
  (package base)
  (package core_bench))
 (action
  (run dune build @all @runtest --root ./correct/map_vs_hash)))

(alias
 (name runtest)
 (deps (alias map_vs_hash)))

(rule
 (alias map_vs_hash2)
 (deps
  (source_tree ./correct/map_vs_hash2)
  (package core_bench))
 (action
  (run dune build @all @runtest --root ./correct/map_vs_hash2)))

(alias
 (name runtest)
 (deps (alias map_vs_hash2)))
