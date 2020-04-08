
(rule
 (alias ext-list)
 (deps
  (source_tree ./correct/ext-list)
  (package base))
 (action
  (run dune build @all --root ./correct/ext-list)))

(alias
 (name runtest)
 (deps (alias ext-list)))

(rule
 (alias freq-dune)
 (deps
  (source_tree ./correct/freq-dune)
  (package base)
  (package stdio))
 (action
  (run dune build @all --root ./correct/freq-dune)))

(alias
 (name runtest)
 (deps (alias freq-dune)))

(rule
 (alias freq-fast)
 (deps
  (source_tree ./correct/freq-fast)
  (package base)
  (package stdio))
 (action
  (run dune build @all --root ./correct/freq-fast)))

(alias
 (name runtest)
 (deps (alias freq-fast)))

(rule
 (alias freq-median)
 (deps
  (source_tree ./correct/freq-median)
  (package base)
  (package stdio))
 (action
  (run dune build @all --root ./correct/freq-median)))

(alias
 (name runtest)
 (deps (alias freq-median)))

(rule
 (alias freq-with-counter)
 (deps
  (source_tree ./correct/freq-with-counter)
  (package base)
  (package stdio))
 (action
  (run dune build @all --root ./correct/freq-with-counter)))

(alias
 (name runtest)
 (deps (alias freq-with-counter)))

(rule
 (alias freq-with-sig)
 (deps
  (source_tree ./correct/freq-with-sig)
  (package base)
  (package stdio))
 (action
  (run dune build @all --root ./correct/freq-with-sig)))

(alias
 (name runtest)
 (deps (alias freq-with-sig)))

(rule
 (alias freq-with-sig-abstract-fixed)
 (deps
  (source_tree ./correct/freq-with-sig-abstract-fixed)
  (package base)
  (package stdio))
 (action
  (run dune build @all --root ./correct/freq-with-sig-abstract-fixed)))

(alias
 (name runtest)
 (deps (alias freq-with-sig-abstract-fixed)))
