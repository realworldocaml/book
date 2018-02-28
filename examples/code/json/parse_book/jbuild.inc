(jbuild_version 1)

(alias ((name sexp) (deps (run_parse_book.sh.sexp))))

(rule (
  (targets (run_parse_book.sh.sexp))
  (deps (run_parse_book.sh book.json parse_book.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

