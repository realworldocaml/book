(jbuild_version 1)

(alias ((name sexp) (deps (build_test_interval_nosexp.errsh.sexp))))

(rule (
  (targets (build_test_interval_nosexp.errsh.sexp))
  (deps    (build_test_interval_nosexp.errsh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (int_interval.ml.sexp))))

(rule (
  (targets (int_interval.ml.sexp))
  (deps    (int_interval.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (int_interval.mli.sexp))))

(rule (
  (targets (int_interval.mli.sexp))
  (deps    (int_interval.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (test_interval_nosexp.ml.sexp))))

(rule (
  (targets (test_interval_nosexp.ml.sexp))
  (deps    (test_interval_nosexp.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

