(jbuild_version 1)

(alias ((name sexp) (deps (build_blow_up.errsh.sexp))))

(rule (
  (targets (build_blow_up.errsh.sexp))
  (deps    (build_blow_up.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_blow_up_notrace.errsh.sexp))))

(rule (
  (targets (build_blow_up_notrace.errsh.sexp))
  (deps    (build_blow_up_notrace.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

