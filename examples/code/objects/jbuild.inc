(jbuild_version 1)

(alias ((name code) (deps (polymorphism.topscript.stamp))))

(alias ((name sexp) (deps (polymorphism.topscript.sexp))))

(rule (
  (targets (polymorphism.topscript.sexp))
  (deps    (polymorphism.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (polymorphism.topscript.stamp))
  (deps    (polymorphism.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? polymorphism.topscript polymorphism.topscript.corrected)))))

(alias ((name code) (deps (stack.topscript.stamp))))

(alias ((name sexp) (deps (stack.topscript.sexp))))

(rule (
  (targets (stack.topscript.sexp))
  (deps    (stack.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (stack.topscript.stamp))
  (deps    (stack.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      stack.topscript stack.topscript.corrected)))))

(alias ((name code) (deps (subtyping.topscript.stamp))))

(alias ((name sexp) (deps (subtyping.topscript.sexp))))

(rule (
  (targets (subtyping.topscript.sexp))
  (deps    (subtyping.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (subtyping.topscript.stamp))
  (deps    (subtyping.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? subtyping.topscript subtyping.topscript.corrected)))))

(alias ((name code) (deps (immutable.topscript.stamp))))

(alias ((name sexp) (deps (immutable.topscript.sexp))))

(rule (
  (targets (immutable.topscript.sexp))
  (deps    (immutable.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (immutable.topscript.stamp))
  (deps    (immutable.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? immutable.topscript immutable.topscript.corrected)))))

(alias ((name code) (deps (row_polymorphism.topscript.stamp))))

(alias ((name sexp) (deps (row_polymorphism.topscript.sexp))))

(rule (
  (targets (row_polymorphism.topscript.sexp))
  (deps    (row_polymorphism.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (row_polymorphism.topscript.stamp))
  (deps    (row_polymorphism.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? row_polymorphism.topscript row_polymorphism.topscript.corrected)))))

