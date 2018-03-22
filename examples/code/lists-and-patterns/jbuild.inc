(jbuild_version 1)

(alias (
  (name runtest)
  (deps (main.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (main.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -non-deterministic -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest)
  (deps (poly.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (poly.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -non-deterministic -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

