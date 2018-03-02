(jbuild_version 1)

(alias ((name sexp) (deps (run_broken_test.errsh.sexp))))

(rule (
  (targets (run_broken_test.errsh.sexp))
  (deps    (run_broken_test.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_test.sh.sexp))))

(rule (
  (targets (build_test.sh.sexp))
  (deps    (build_test.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

