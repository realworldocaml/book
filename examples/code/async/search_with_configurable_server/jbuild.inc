(jbuild_version 1)

(alias ((name sexp) (deps (run_search_with_configurable_server.errsh.sexp))))

(rule (
  (targets (run_search_with_configurable_server.errsh.sexp))
  (deps (
    run_search_with_configurable_server.errsh
    search_with_configurable_server.ml
    jbuild
    jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

