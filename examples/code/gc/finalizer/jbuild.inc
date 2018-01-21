(jbuild_version 1)

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_finalizer.sh.sexp))))

(rule (
  (targets (run_finalizer.sh.sexp))
  (deps    (run_finalizer.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (finalizer.ml.sexp))))

(rule (
  (targets (finalizer.ml.sexp))
  (deps    (finalizer.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

