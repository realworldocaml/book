(jbuild_version 1)

(alias ((name sexp) (deps (counter.mli.sexp))))

(rule (
  (targets (counter.mli.sexp))
  (deps    (counter.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (counter.ml.sexp))))

(rule (
  (targets (counter.ml.sexp))
  (deps    (counter.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build.errsh.sexp))))

(rule (
  (targets (build.errsh.sexp))
  (deps    (build.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (freq.ml.sexp))))

(rule (
  (targets (freq.ml.sexp))
  (deps    (freq.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

