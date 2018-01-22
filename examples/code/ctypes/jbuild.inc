(jbuild_version 1)

(alias ((name sexp) (deps (ctypes.ml.sexp))))

(rule (
  (targets (ctypes.ml.sexp))
  (deps    (ctypes.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (ctypes_impl.ml.sexp))))

(rule (
  (targets (ctypes_impl.ml.sexp))
  (deps    (ctypes_impl.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (ctypes.mli.sexp))))

(rule (
  (targets (ctypes.mli.sexp))
  (deps    (ctypes.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

