(jbuild_version 1)

(alias ((name sexp) (deps (int_interval.ml.sexp))))

(rule (
  (targets (int_interval.ml.sexp))
  (deps    (int_interval.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (int_interval.mli.sexp))))

(rule (
  (targets (int_interval.mli.sexp))
  (deps    (int_interval.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (test_interval.ml.sexp))))

(rule (
  (targets (test_interval.ml.sexp))
  (deps    (test_interval.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_test_interval.sh.sexp))))

(rule (
  (targets (build_test_interval.sh.sexp))
  (deps    (build_test_interval.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

