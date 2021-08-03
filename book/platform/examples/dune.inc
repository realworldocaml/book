
(rule
 (alias opam-spin-hello)
 (deps
  (source_tree ./correct/opam-spin-hello))
 (action
  (system "dune build @all @runtest --root ./correct/opam-spin-hello")))

(alias
 (name runtest)
 (deps (alias opam-spin-hello)))

(rule
 (alias opam-spin-ls)
 (deps
  (source_tree ./correct/opam-spin-ls))
 (action
  (system "dune build @all @runtest --root ./correct/opam-spin-ls")))

(alias
 (name runtest)
 (deps (alias opam-spin-ls)))
