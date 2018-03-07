(jbuild_version 1)

(alias (
  (name code)
  (deps (tune.mlt))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

