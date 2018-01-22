(jbuild_version 1)

(alias ((name sexp) (deps (build_cal_append_broken.errsh.sexp))))

(rule (
  (targets (build_cal_append_broken.errsh.sexp))
  (deps (build_cal_append_broken.errsh cal_append_broken.ml))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (cal_append_broken.ml.sexp))))

(rule (
  (targets (cal_append_broken.ml.sexp))
  (deps    (cal_append_broken.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

