(jbuild_version 1)

(alias ((name sexp) (deps (build.sh.sexp))))

(rule (
  (targets (build.sh.sexp))
  (deps    (build.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

