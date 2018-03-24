(jbuild_version 1)

(alias (
  (name runtest)
  (deps (step.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (step.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -non-deterministic -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest)
  (deps (basic.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (basic.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -non-deterministic -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

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

