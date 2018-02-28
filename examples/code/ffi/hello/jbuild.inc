(jbuild_version 1)

(alias ((name sexp) (deps (build_hello.sh.sexp))))

(rule (
  (targets (build_hello.sh.sexp))
  (deps (build_hello.sh jbuild jbuild.inc hello.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

