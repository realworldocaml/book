(jbuild_version 1)

(alias ((name sexp) (deps (build_short_parser.sh.sexp))))

(rule (
  (targets (build_short_parser.sh.sexp))
  (deps (
    build_short_parser.sh
    short_parser.mly
    jbuild
    jbuild.inc
    json.ml
    lexer.mll
    prog.ml
    prog.mli))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

