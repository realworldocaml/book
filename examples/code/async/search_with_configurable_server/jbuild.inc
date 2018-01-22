(jbuild_version 1)

(alias ((name sexp) (deps (search_with_configurable_server.ml.sexp))))

(rule (
  (targets (search_with_configurable_server.ml.sexp))
  (deps    (search_with_configurable_server.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_search_with_configurable_server.errsh.sexp))))

(rule (
  (targets (run_search_with_configurable_server.errsh.sexp))
  (deps (run_search_with_configurable_server.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

