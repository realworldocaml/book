(jbuild_version 1)

(alias ((name sexp) (deps (build_and_run_cal_add_interactive.sh.sexp))))

(rule (
  (targets (build_and_run_cal_add_interactive.sh.sexp))
  (deps (
    build_and_run_cal_add_interactive.sh
    cal_add_interactive.ml
    jbuild
    jbuild.inc))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (cal_add_interactive.ml.sexp))))

(rule (
  (targets (cal_add_interactive.ml.sexp))
  (deps    (cal_add_interactive.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

