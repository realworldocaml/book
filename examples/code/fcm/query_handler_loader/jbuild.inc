(jbuild_version 1)

(alias ((name sexp) (deps (query_handler_core.ml.sexp))))

(rule (
  (targets (query_handler_core.ml.sexp))
  (deps    (query_handler_core.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (query_handler_loader.ml.sexp))))

(rule (
  (targets (query_handler_loader.ml.sexp))
  (deps    (query_handler_loader.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (query_handler.ml.sexp))))

(rule (
  (targets (query_handler.ml.sexp))
  (deps    (query_handler.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_query_handler_loader.sh.sexp))))

(rule (
  (targets (build_query_handler_loader.sh.sexp))
  (deps    (build_query_handler_loader.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

