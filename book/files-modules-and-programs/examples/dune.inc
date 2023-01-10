
(rule
 (alias abstract-username)
 (locks /global)
 (deps
  (source_tree ./correct/abstract-username)
  (package base))
 (action
  (system "dune build @all @runtest --root ./correct/abstract-username")))

(alias
 (name runtest)
 (deps (alias abstract-username)))

(rule
 (alias ext-option)
 (locks /global)
 (deps
  (source_tree ./correct/ext-option)
  (package base))
 (action
  (system "dune build @all @runtest --root ./correct/ext-option")))

(alias
 (name runtest)
 (deps (alias ext-option)))

(rule
 (alias freq-dune)
 (locks /global)
 (deps
  (source_tree ./correct/freq-dune)
  (package base)
  (package stdio))
 (action
  (system "dune build @all @runtest --root ./correct/freq-dune")))

(alias
 (name runtest)
 (deps (alias freq-dune)))

(rule
 (alias freq-fast)
 (locks /global)
 (deps
  (source_tree ./correct/freq-fast)
  (package base)
  (package stdio))
 (action
  (system "dune build @all @runtest --root ./correct/freq-fast")))

(alias
 (name runtest)
 (deps (alias freq-fast)))

(rule
 (alias freq-median)
 (locks /global)
 (deps
  (source_tree ./correct/freq-median)
  (package base)
  (package stdio))
 (action
  (system "dune build @all @runtest --root ./correct/freq-median")))

(alias
 (name runtest)
 (deps (alias freq-median)))

(rule
 (alias freq-with-counter)
 (locks /global)
 (deps
  (source_tree ./correct/freq-with-counter)
  (package base)
  (package stdio))
 (action
  (system "dune build @all @runtest --root ./correct/freq-with-counter")))

(alias
 (name runtest)
 (deps (alias freq-with-counter)))

(rule
 (alias freq-with-sig)
 (locks /global)
 (deps
  (source_tree ./correct/freq-with-sig)
  (package base)
  (package stdio))
 (action
  (system "dune build @all @runtest --root ./correct/freq-with-sig")))

(alias
 (name runtest)
 (deps (alias freq-with-sig)))

(rule
 (alias freq-with-sig-abstract-fixed)
 (locks /global)
 (deps
  (source_tree ./correct/freq-with-sig-abstract-fixed)
  (package base)
  (package stdio))
 (action
  (system "dune build @all @runtest --root ./correct/freq-with-sig-abstract-fixed")))

(alias
 (name runtest)
 (deps (alias freq-with-sig-abstract-fixed)))
