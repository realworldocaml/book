(jbuild_version 1)

(alias ((name code) (deps (qsort.topscript.stamp))))

(alias ((name sexp) (deps (qsort.topscript.sexp))))

(rule (
  (targets (qsort.topscript.sexp))
  (deps    (qsort.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (qsort.topscript.stamp))
  (deps    (qsort.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      qsort.topscript qsort.topscript.corrected)))))

(alias ((name code) (deps (posix.topscript.stamp))))

(alias ((name sexp) (deps (posix.topscript.sexp))))

(rule (
  (targets (posix.topscript.sexp))
  (deps    (posix.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (posix.topscript.stamp))
  (deps    (posix.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      posix.topscript posix.topscript.corrected)))))

