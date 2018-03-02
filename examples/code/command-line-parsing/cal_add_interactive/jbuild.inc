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
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

