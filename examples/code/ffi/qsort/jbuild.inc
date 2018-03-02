(jbuild_version 1)

(alias ((name sexp) (deps (build_qsort.sh.sexp))))

(rule (
  (targets (build_qsort.sh.sexp))
  (deps (build_qsort.sh input.txt jbuild jbuild.inc qsort.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

