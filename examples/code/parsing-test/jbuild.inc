(jbuild_version 1)

(alias ((name sexp) (deps (lexer.mll.sexp))))

(rule (
  (targets (lexer.mll.sexp))
  (deps    (lexer.mll))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (test1.json.sexp))))

(rule (
  (targets (test1.json.sexp))
  (deps    (test1.json))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (json.ml.sexp))))

(rule (
  (targets (json.ml.sexp))
  (deps    (json.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_broken_test.errsh.sexp))))

(rule (
  (targets (run_broken_test.errsh.sexp))
  (deps    (run_broken_test.errsh))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (parser.mly.sexp))))

(rule (
  (targets (parser.mly.sexp))
  (deps    (parser.mly))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (test.ml.sexp))))

(rule (
  (targets (test.ml.sexp))
  (deps    (test.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (test2.json.sexp))))

(rule (
  (targets (test2.json.sexp))
  (deps    (test2.json))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_test.sh.sexp))))

(rule (
  (targets (build_test.sh.sexp))
  (deps    (build_test.sh))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

