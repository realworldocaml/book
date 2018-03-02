(jbuild_version 1)

(alias ((name sexp) (deps (tune.topscript.sexp))))

(rule (
  (targets (tune.topscript.sexp))
  (deps    (tune.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (tune.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

