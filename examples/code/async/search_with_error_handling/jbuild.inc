(jbuild_version 1)

(alias ((name sexp) (deps (search_with_error_handling.ml.sexp))))

(rule (
  (targets (search_with_error_handling.ml.sexp))
  (deps    (search_with_error_handling.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_search_with_error_handling.sh.sexp))))

(rule (
  (targets (run_search_with_error_handling.sh.sexp))
  (deps    (run_search_with_error_handling.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

