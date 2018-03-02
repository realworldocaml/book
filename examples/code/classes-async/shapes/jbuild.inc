(jbuild_version 1)

(alias ((name sexp) (deps (build_shapes.sh.sexp))))

(rule (
  (targets (build_shapes.sh.sexp))
  (deps    (build_shapes.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

