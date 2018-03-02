(jbuild_version 1)

(alias ((name sexp) (deps (build_datetime.sh.sexp))))

(rule (
  (targets (build_datetime.sh.sexp))
  (deps (build_datetime.sh datetime.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

