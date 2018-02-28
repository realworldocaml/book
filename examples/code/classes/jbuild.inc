(jbuild_version 1)

(alias ((name code) (deps (iter.topscript.stamp))))

(alias ((name sexp) (deps (iter.topscript.sexp))))

(rule (
  (targets (iter.topscript.sexp))
  (deps    (iter.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (iter.topscript.stamp))
  (deps    (iter.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}           "")
    (diff?      iter.topscript iter.topscript.corrected)))))

(alias ((name code) (deps (binary.topscript.stamp))))

(alias ((name sexp) (deps (binary.topscript.sexp))))

(rule (
  (targets (binary.topscript.sexp))
  (deps    (binary.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (binary.topscript.stamp))
  (deps    (binary.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? binary.topscript binary.topscript.corrected)))))

(alias ((name code) (deps (istack.topscript.stamp))))

(alias ((name sexp) (deps (istack.topscript.sexp))))

(rule (
  (targets (istack.topscript.sexp))
  (deps    (istack.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (istack.topscript.stamp))
  (deps    (istack.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? istack.topscript istack.topscript.corrected)))))

(alias ((name code) (deps (initializer.topscript.stamp))))

(alias ((name sexp) (deps (initializer.topscript.sexp))))

(rule (
  (targets (initializer.topscript.sexp))
  (deps    (initializer.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (initializer.topscript.stamp))
  (deps    (initializer.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? initializer.topscript initializer.topscript.corrected)))))

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

