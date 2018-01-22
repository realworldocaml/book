(jbuild_version 1)

(alias ((name sexp) (deps (terminal_color.ml.sexp))))

(rule (
  (targets (terminal_color.ml.sexp))
  (deps    (terminal_color.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (terminal_color.mli.sexp))))

(rule (
  (targets (terminal_color.mli.sexp))
  (deps    (terminal_color.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build.errsh.sexp))))

(rule (
  (targets (build.errsh.sexp))
  (deps    (build.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

