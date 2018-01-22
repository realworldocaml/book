(jbuild_version 1)

(alias ((name sexp) (deps (read_json.ml.sexp))))

(rule (
  (targets (read_json.ml.sexp))
  (deps    (read_json.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_read_json.sh.sexp))))

(rule (
  (targets (run_read_json.sh.sexp))
  (deps (book.json run_read_json.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

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

