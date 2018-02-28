(jbuild_version 1)

(alias ((name sexp) (deps (run_search_with_timeout_no_leak.sh.sexp))))

(rule (
  (targets (run_search_with_timeout_no_leak.sh.sexp))
  (deps (
    run_search_with_timeout_no_leak.sh
    search_with_timeout_no_leak.ml
    jbuild
    jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

