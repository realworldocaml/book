(jbuild_version 1)

(alias ((name sexp) (deps (parse_book.ml.sexp))))

(rule (
  (targets (parse_book.ml.sexp))
  (deps    (parse_book.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (book.json.sexp))))

(rule (
  (targets (book.json.sexp))
  (deps    (book.json))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_parse_book.sh.sexp))))

(rule (
  (targets (run_parse_book.sh.sexp))
  (deps    (run_parse_book.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

