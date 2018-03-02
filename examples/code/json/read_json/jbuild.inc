(jbuild_version 1)

(alias ((name sexp) (deps (run_read_json.sh.sexp))))

(rule (
  (targets (run_read_json.sh.sexp))
  (deps (run_read_json.sh book.json read_json.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

