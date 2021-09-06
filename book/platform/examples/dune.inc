
(rule
 (alias hello-world)
 (deps
  (source_tree ./correct/hello-world)
  (package alcotest))
 (action
  (system "dune build @all @runtest --root ./correct/hello-world")))

(alias
 (name runtest)
 (deps (alias hello-world)))

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
