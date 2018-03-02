(jbuild_version 1)

(alias ((name sexp) (deps (build.sh.sexp))))

(rule (
  (targets (build.sh.sexp))
  (deps (build.sh freq.ml counter.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (infer_mli.sh.sexp))))

(rule (
  (targets (infer_mli.sh.sexp))
  (deps    (infer_mli.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

