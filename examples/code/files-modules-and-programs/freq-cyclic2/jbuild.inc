(jbuild_version 1)

(alias ((name sexp) (deps (build.errsh.sexp))))

(rule (
  (targets (build.errsh.sexp))
  (deps    (build.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

