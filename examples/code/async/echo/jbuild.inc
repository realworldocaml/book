(jbuild_version 1)

(alias ((name sexp) (deps (run_echo.sh.sexp))))

(rule (
  (targets (run_echo.sh.sexp))
  (deps (run_echo.sh jbuild jbuild.inc echo.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

