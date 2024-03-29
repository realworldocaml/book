
(rule
 (alias variants-termcol)
 (deps
  (source_tree ./correct/variants-termcol)
  (package base))
 (action
  (system "dune build @all @runtest --root ./correct/variants-termcol")))

(alias
 (name runtest)
 (deps (alias variants-termcol)))

(rule
 (alias variants-termcol-fixed)
 (deps
  (source_tree ./correct/variants-termcol-fixed)
  (package base))
 (action
  (system "dune build @all @runtest --root ./correct/variants-termcol-fixed")))

(alias
 (name runtest)
 (deps (alias variants-termcol-fixed)))
