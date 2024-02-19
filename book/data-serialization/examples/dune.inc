
(rule
 (alias read_foo)
 (locks /global)
 (deps
  (source_tree ./correct/read_foo)
  (package core)
  (package sexplib))
 (action
  (system "dune build @all @runtest --root ./correct/read_foo")))

(alias
 (name runtest)
 (deps (alias read_foo)))

(rule
 (alias read_foo_better_errors)
 (locks /global)
 (deps
  (source_tree ./correct/read_foo_better_errors)
  (package core)
  (package sexplib))
 (action
  (system "dune build @all @runtest --root ./correct/read_foo_better_errors")))

(alias
 (name runtest)
 (deps (alias read_foo_better_errors)))

(rule
 (alias sexp)
 (locks /global)
 (deps
  (source_tree ./correct/sexp)
  (package base))
 (action
  (system "dune build @all @runtest --root ./correct/sexp")))

(alias
 (name runtest)
 (deps (alias sexp)))

(rule
 (alias test_interval)
 (locks /global)
 (deps
  (source_tree ./correct/test_interval)
  (package core)
  (package sexplib))
 (action
  (system "dune build @all @runtest --root ./correct/test_interval")))

(alias
 (name runtest)
 (deps (alias test_interval)))

(rule
 (alias test_interval_override_of_sexp)
 (locks /global)
 (deps
  (source_tree ./correct/test_interval_override_of_sexp)
  (package core)
  (package sexplib))
 (action
  (system "dune build @all @runtest --root ./correct/test_interval_override_of_sexp")))

(alias
 (name runtest)
 (deps (alias test_interval_override_of_sexp)))
