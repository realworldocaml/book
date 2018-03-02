(jbuild_version 1)

(alias ((name sexp) (deps (build_query_handler_loader.sh.sexp))))

(rule (
  (targets (build_query_handler_loader.sh.sexp))
  (deps (
    build_query_handler_loader.sh
    query_handler.ml
    query_handler_core.ml
    query_handler_loader.ml
    jbuild
    jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

