(jbuild_version 1)

(alias ((name code) (deps (poly.topscript.stamp))))

(alias ((name sexp) (deps (poly.topscript.sexp))))

(rule (
  (targets (poly.topscript.sexp))
  (deps    (poly.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (poly.topscript.stamp))
  (deps    (poly.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}           "")
    (diff?      poly.topscript poly.topscript.corrected)))))

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

