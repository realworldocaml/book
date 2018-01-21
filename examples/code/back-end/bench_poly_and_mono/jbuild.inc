(jbuild_version 1)

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_bench_poly_and_mono.sh.sexp))))

(rule (
  (targets (run_bench_poly_and_mono.sh.sexp))
  (deps    (run_bench_poly_and_mono.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (bench_poly_and_mono.ml.sexp))))

(rule (
  (targets (bench_poly_and_mono.ml.sexp))
  (deps    (bench_poly_and_mono.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

