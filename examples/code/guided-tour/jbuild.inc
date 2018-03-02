(jbuild_version 1)

(alias ((name code) (deps (main.topscript.stamp))))

(alias ((name sexp) (deps (main.topscript.sexp))))

(rule (
  (targets (main.topscript.sexp))
  (deps    (main.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (main.topscript.stamp))
  (deps    (main.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}           "")
    (diff?      main.topscript main.topscript.corrected)))))

(alias ((name code) (deps (local_let.topscript.stamp))))

(alias ((name sexp) (deps (local_let.topscript.sexp))))

(rule (
  (targets (local_let.topscript.sexp))
  (deps    (local_let.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (local_let.topscript.stamp))
  (deps    (local_let.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? local_let.topscript local_let.topscript.corrected)))))

