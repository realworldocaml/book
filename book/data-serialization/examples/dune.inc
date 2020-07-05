
(rule
 (alias int_interval_manual_sexp)
 (deps
  (source_tree ./correct/int_interval_manual_sexp))
 (action
  (run dune build @all @runtest --root ./correct/int_interval_manual_sexp)))

(alias
 (name runtest)
 (deps (alias int_interval_manual_sexp)))

(rule
 (alias int_interval_nosexp)
 (deps
  (source_tree ./correct/int_interval_nosexp))
 (action
  (run dune build @all @runtest --root ./correct/int_interval_nosexp)))

(alias
 (name runtest)
 (deps (alias int_interval_nosexp)))

(rule
 (alias read_foo)
 (deps
  (source_tree ./correct/read_foo))
 (action
  (run dune build @all @runtest --root ./correct/read_foo)))

(alias
 (name runtest)
 (deps (alias read_foo)))

(rule
 (alias read_foo_better_errors)
 (deps
  (source_tree ./correct/read_foo_better_errors))
 (action
  (run dune build @all @runtest --root ./correct/read_foo_better_errors)))

(alias
 (name runtest)
 (deps (alias read_foo_better_errors)))

(rule
 (alias sexp_override)
 (deps
  (source_tree ./correct/sexp_override))
 (action
  (run dune build @all @runtest --root ./correct/sexp_override)))

(alias
 (name runtest)
 (deps (alias sexp_override)))

(rule
 (alias test_interval)
 (deps
  (source_tree ./correct/test_interval))
 (action
  (run dune build @all @runtest --root ./correct/test_interval)))

(alias
 (name runtest)
 (deps (alias test_interval)))

(rule
 (alias test_interval_nosexp)
 (deps
  (source_tree ./correct/test_interval_nosexp))
 (action
  (run dune build @all @runtest --root ./correct/test_interval_nosexp)))

(alias
 (name runtest)
 (deps (alias test_interval_nosexp)))
