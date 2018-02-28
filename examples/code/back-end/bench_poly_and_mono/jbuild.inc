(jbuild_version 1)

(alias ((name sexp) (deps (run_bench_poly_and_mono.sh.sexp))))

(rule (
  (targets (run_bench_poly_and_mono.sh.sexp))
  (deps    (run_bench_poly_and_mono.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

