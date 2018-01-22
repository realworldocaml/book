(jbuild_version 1)

(alias ((name sexp) (deps (build_session_info.errsh.sexp))))

(rule (
  (targets (build_session_info.errsh.sexp))
  (deps    (build_session_info.errsh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (session_info.ml.sexp))))

(rule (
  (targets (session_info.ml.sexp))
  (deps    (session_info.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

