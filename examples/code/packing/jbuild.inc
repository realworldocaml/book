(jbuild_version 1)

(alias ((name sexp) (deps (build_test.sh.sexp))))

(rule (
  (targets (build_test.sh.sexp))
  (deps    (build_test.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (show_files.sh.sexp))))

(rule (
  (targets (show_files.sh.sexp))
  (deps    (show_files.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

