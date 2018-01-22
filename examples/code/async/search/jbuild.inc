(jbuild_version 1)

(alias ((name sexp) (deps (search.ml.sexp))))

(rule (
  (targets (search.ml.sexp))
  (deps    (search.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_search.sh.sexp))))

(rule (
  (targets (run_search.sh.sexp))
  (deps    (run_search.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

