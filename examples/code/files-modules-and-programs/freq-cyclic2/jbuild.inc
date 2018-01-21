(jbuild_version 1)

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (counter.ml.sexp))))

(rule (
  (targets (counter.ml.sexp))
  (deps    (counter.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build.errsh.sexp))))

(rule (
  (targets (build.errsh.sexp))
  (deps    (build.errsh))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (freq.ml.sexp))))

(rule (
  (targets (freq.ml.sexp))
  (deps    (freq.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

