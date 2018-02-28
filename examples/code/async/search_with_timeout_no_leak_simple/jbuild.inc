(jbuild_version 1)

(alias ((name sexp) (deps (run_search_with_timeout_no_leak_simple.sh.sexp))))

(rule (
  (targets (run_search_with_timeout_no_leak_simple.sh.sexp))
  (deps (run_search_with_timeout_no_leak_simple.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

