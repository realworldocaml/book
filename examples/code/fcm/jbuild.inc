(jbuild_version 1)

(alias (
  (name code)
  (deps (query_handler.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name code)
  (deps (main.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

