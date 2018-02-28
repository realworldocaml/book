(jbuild_version 1)

(alias ((name sexp) (deps (run_search.sh.sexp))))

(rule (
  (targets (run_search.sh.sexp))
  (deps (run_search.sh search.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

