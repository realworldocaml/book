(jbuild_version 1)

(alias ((name sexp) (deps (basic.topscript.sexp))))

(rule (
  (targets (basic.topscript.sexp))
  (deps    (basic.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (basic.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (step.topscript.sexp))))

(rule (
  (targets (step.topscript.sexp))
  (deps    (step.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (step.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (command_types.topscript.sexp))))

(rule (
  (targets (command_types.topscript.sexp))
  (deps    (command_types.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (command_types.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (group.topscript.sexp))))

(rule (
  (targets (group.topscript.sexp))
  (deps    (group.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (group.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

