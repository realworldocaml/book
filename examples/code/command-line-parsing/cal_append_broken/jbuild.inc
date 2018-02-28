(jbuild_version 1)

(alias ((name sexp) (deps (build_cal_append_broken.errsh.sexp))))

(rule (
  (targets (build_cal_append_broken.errsh.sexp))
  (deps (build_cal_append_broken.errsh cal_append_broken.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

