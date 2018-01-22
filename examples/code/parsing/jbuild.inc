(jbuild_version 1)

(alias ((name sexp) (deps (right_rec_rule.mly.sexp))))

(rule (
  (targets (right_rec_rule.mly.sexp))
  (deps    (right_rec_rule.mly))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (lexer_int_fragment.mll.sexp))))

(rule (
  (targets (lexer_int_fragment.mll.sexp))
  (deps    (lexer_int_fragment.mll))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_short_parser.sh.sexp))))

(rule (
  (targets (build_short_parser.sh.sexp))
  (deps    (build_short_parser.sh))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (lexer.mll.sexp))))

(rule (
  (targets (lexer.mll.sexp))
  (deps    (lexer.mll))
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

(alias ((name sexp) (deps (parsed_example.ml.sexp))))

(rule (
  (targets (parsed_example.ml.sexp))
  (deps    (parsed_example.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (example.json.sexp))))

(rule (
  (targets (example.json.sexp))
  (deps    (example.json))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (prog.mli.sexp))))

(rule (
  (targets (prog.mli.sexp))
  (deps    (prog.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (prog.ml.sexp))))

(rule (
  (targets (prog.ml.sexp))
  (deps    (prog.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (quadratic_rule.mly.sexp))))

(rule (
  (targets (quadratic_rule.mly.sexp))
  (deps    (quadratic_rule.mly))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (manual_token_type.ml.sexp))))

(rule (
  (targets (manual_token_type.ml.sexp))
  (deps    (manual_token_type.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (parser.mly.sexp))))

(rule (
  (targets (parser.mly.sexp))
  (deps    (parser.mly))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (short_parser.mly.sexp))))

(rule (
  (targets (short_parser.mly.sexp))
  (deps    (short_parser.mly))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (tokens.ml.sexp))))

(rule (
  (targets (tokens.ml.sexp))
  (deps    (tokens.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

