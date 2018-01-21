(jbuild_version 1)

(alias ((name sexp) (deps (time_converter.ml.sexp))))

(rule (
  (targets (time_converter.ml.sexp))
  (deps    (time_converter.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (time_converter.rawsh.sexp))))

(rule (
  (targets (time_converter.rawsh.sexp))
  (deps    (time_converter.rawsh))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

