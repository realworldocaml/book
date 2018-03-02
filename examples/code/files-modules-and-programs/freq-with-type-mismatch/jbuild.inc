(jbuild_version 1)

(alias ((name sexp) (deps (build.errsh.sexp))))

(rule (
  (targets (build.errsh.sexp))
  (deps (build.errsh counter.ml counter.mli freq.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

