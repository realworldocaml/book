(jbuild_version 1)

(alias ((name sexp) (deps (build_test_interval.sh.sexp))))

(rule (
  (targets (build_test_interval.sh.sexp))
  (deps (build_test_interval.sh test_interval.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

