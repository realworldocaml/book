
(rule
 (alias variants-termcol)
 (deps
  (source_tree ./correct/variants-termcol)
  (package base))
 (action
  (run dune build @all @runtest --root ./correct/variants-termcol)))

(alias
 (name runtest)
 (deps (alias variants-termcol)))

(rule
 (alias variants-termcol-fixed)
 (deps
  (source_tree ./correct/variants-termcol-fixed))
 (action
  (run dune build @all @runtest --root ./correct/variants-termcol-fixed)))

(alias
 (name runtest)
 (deps (alias variants-termcol-fixed)))
