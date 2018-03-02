(jbuild_version 1)

(alias ((name sexp) (deps (build_test_interval_nosexp.errsh.sexp))))

(rule (
  (targets (build_test_interval_nosexp.errsh.sexp))
  (deps    (build_test_interval_nosexp.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

