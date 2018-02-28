(jbuild_version 1)

(alias ((name sexp) (deps (build_cal_add_sub_days.sh.sexp))))

(rule (
  (targets (build_cal_add_sub_days.sh.sexp))
  (deps (build_cal_add_sub_days.sh cal_add_sub_days.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (run_cal_add_sub_days.sh.sexp))))

(rule (
  (targets (run_cal_add_sub_days.sh.sexp))
  (deps (run_cal_add_sub_days.sh build_cal_add_sub_days.sh.sexp))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

