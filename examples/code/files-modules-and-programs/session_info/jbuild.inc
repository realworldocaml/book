(jbuild_version 1)

(alias ((name sexp) (deps (build_session_info.errsh.sexp))))

(rule (
  (targets (build_session_info.errsh.sexp))
  (deps (build_session_info.errsh session_info.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

