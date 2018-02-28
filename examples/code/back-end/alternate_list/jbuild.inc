(jbuild_version 1)

(alias ((name sexp) (deps (run_alternate_list.sh.sexp))))

(rule (
  (targets (run_alternate_list.sh.sexp))
  (deps    (run_alternate_list.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

