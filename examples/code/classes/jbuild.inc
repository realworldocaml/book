(jbuild_version 1)

(alias (
  (name runtest)
  (deps (iter.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (iter.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -non-deterministic -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest)
  (deps (istack.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (istack.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -non-deterministic -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest)
  (deps (stack.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (stack.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -non-deterministic -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest)
  (deps (binary.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (binary.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -non-deterministic -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest)
  (deps (initializer.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (initializer.mlt (files_recursively_in .)))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -non-deterministic -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

