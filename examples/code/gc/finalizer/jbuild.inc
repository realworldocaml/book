(jbuild_version 1)

(alias ((name sexp) (deps (run_finalizer.sh.sexp))))

(rule (
  (targets (run_finalizer.sh.sexp))
  (deps    (run_finalizer.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

