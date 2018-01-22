(jbuild_version 1)

(alias ((name sexp) (deps (multiple_inheritance_wrong.ml.sexp))))

(rule (
  (targets (multiple_inheritance_wrong.ml.sexp))
  (deps    (multiple_inheritance_wrong.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (verbose_shapes.ml.sexp))))

(rule (
  (targets (verbose_shapes.ml.sexp))
  (deps    (verbose_shapes.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (multiple_inheritance.ml.sexp))))

(rule (
  (targets (multiple_inheritance.ml.sexp))
  (deps    (multiple_inheritance.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

