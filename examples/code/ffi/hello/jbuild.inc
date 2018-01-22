(jbuild_version 1)

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_hello.sh.sexp))))

(rule (
  (targets (build_hello.sh.sexp))
  (deps    (build_hello.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (hello.ml.sexp))))

(rule (
  (targets (hello.ml.sexp))
  (deps    (hello.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

