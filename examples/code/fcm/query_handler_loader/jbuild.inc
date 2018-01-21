(jbuild_version 1)

(alias ((name sexp) (deps (query_handler_core.ml.sexp))))

(rule (
  (targets (query_handler_core.ml.sexp))
  (deps    (query_handler_core.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (query_handler_loader.ml.sexp))))

(rule (
  (targets (query_handler_loader.ml.sexp))
  (deps    (query_handler_loader.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (query_handler.ml.sexp))))

(rule (
  (targets (query_handler.ml.sexp))
  (deps    (query_handler.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_query_handler_loader.sh.sexp))))

(rule (
  (targets (build_query_handler_loader.sh.sexp))
  (deps    (build_query_handler_loader.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

