(jbuild_version 1)

(alias ((name sexp) (deps (run_search_with_error_handling.sh.sexp))))

(rule (
  (targets (run_search_with_error_handling.sh.sexp))
  (deps (
    run_search_with_error_handling.sh
    search_with_error_handling.ml
    jbuild
    jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

