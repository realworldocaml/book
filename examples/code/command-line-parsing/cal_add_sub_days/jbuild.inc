(jbuild_version 1)

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_cal_add_sub_days.sh.sexp))))

(rule (
  (targets (build_cal_add_sub_days.sh.sexp))
  (deps    (build_cal_add_sub_days.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (run_cal_add_sub_days.sh.sexp))))

(rule (
  (targets (run_cal_add_sub_days.sh.sexp))
  (deps    (run_cal_add_sub_days.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (cal_add_sub_days.ml.sexp))))

(rule (
  (targets (cal_add_sub_days.ml.sexp))
  (deps    (cal_add_sub_days.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

