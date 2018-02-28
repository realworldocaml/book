(jbuild_version 1)

(alias ((name sexp) (deps (build_sum.sh.sexp))))

(rule (
  (targets (build_sum.sh.sexp))
  (deps (build_sum.sh jbuild jbuild.inc sum.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

