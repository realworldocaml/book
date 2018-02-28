(jbuild_version 1)

(alias ((name sexp) (deps (run_bench_patterns.sh.sexp))))

(rule (
  (targets (run_bench_patterns.sh.sexp))
  (deps    (run_bench_patterns.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

