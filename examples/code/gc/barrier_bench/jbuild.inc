(jbuild_version 1)

(alias ((name sexp) (deps (show_barrier_bench_help.sh.sexp))))

(rule (
  (targets (show_barrier_bench_help.sh.sexp))
  (deps    (show_barrier_bench_help.sh))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (barrier_bench.ml.sexp))))

(rule (
  (targets (barrier_bench.ml.sexp))
  (deps    (barrier_bench.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_barrier_bench.sh.sexp))))

(rule (
  (targets (run_barrier_bench.sh.sexp))
  (deps    (run_barrier_bench.sh))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

