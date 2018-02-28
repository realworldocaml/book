(jbuild_version 1)

(alias ((name sexp) (deps (build.sh.sexp))))

(rule (
  (targets (build.sh.sexp))
  (deps (build.sh jbuild.inc jbuild freq.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (test.sh.sexp))))

(rule (
  (targets (test.sh.sexp))
  (deps (test.sh build.sh.sexp))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

