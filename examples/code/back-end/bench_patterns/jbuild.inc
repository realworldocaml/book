(jbuild_version 1)

(alias ((name sexp) (deps (run_bench_patterns.sh.sexp))))

(rule (
  (targets (run_bench_patterns.sh.sexp))
  (deps    (run_bench_patterns.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (bench_patterns.ml.sexp))))

(rule (
  (targets (bench_patterns.ml.sexp))
  (deps    (bench_patterns.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

