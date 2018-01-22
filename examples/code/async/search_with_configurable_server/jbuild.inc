(jbuild_version 1)

(alias ((name sexp) (deps (search_with_configurable_server.ml.sexp))))

(rule (
  (targets (search_with_configurable_server.ml.sexp))
  (deps    (search_with_configurable_server.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_search_with_configurable_server.errsh.sexp))))

(rule (
  (targets (run_search_with_configurable_server.errsh.sexp))
  (deps (run_search_with_configurable_server.errsh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

