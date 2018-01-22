(jbuild_version 1)

(alias ((name sexp) (deps (parse_book.ml.sexp))))

(rule (
  (targets (parse_book.ml.sexp))
  (deps    (parse_book.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (book.json.sexp))))

(rule (
  (targets (book.json.sexp))
  (deps    (book.json))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_parse_book.sh.sexp))))

(rule (
  (targets (run_parse_book.sh.sexp))
  (deps (run_parse_book.sh book.json parse_book.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

