(jbuild_version 1)

(alias ((name sexp) (deps (build_blow_up.errsh.sexp))))

(rule (
  (targets (build_blow_up.errsh.sexp))
  (deps    (build_blow_up.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (blow_up.ml.sexp))))

(rule (
  (targets (blow_up.ml.sexp))
  (deps    (blow_up.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_blow_up_notrace.errsh.sexp))))

(rule (
  (targets (build_blow_up_notrace.errsh.sexp))
  (deps    (build_blow_up_notrace.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

