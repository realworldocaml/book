(jbuild_version 1)

(alias ((name sexp) (deps (build_session_info.errsh.sexp))))

(rule (
  (targets (build_session_info.errsh.sexp))
  (deps (build_session_info.errsh session_info.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (session_info.ml.sexp))))

(rule (
  (targets (session_info.ml.sexp))
  (deps    (session_info.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

