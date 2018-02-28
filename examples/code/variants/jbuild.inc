(jbuild_version 1)

(alias ((name code) (deps (blang.topscript.stamp))))

(alias ((name sexp) (deps (blang.topscript.sexp))))

(rule (
  (targets (blang.topscript.sexp))
  (deps    (blang.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (blang.topscript.stamp))
  (deps    (blang.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      blang.topscript blang.topscript.corrected)))))

(alias ((name code) (deps (logger.topscript.stamp))))

(alias ((name sexp) (deps (logger.topscript.sexp))))

(rule (
  (targets (logger.topscript.sexp))
  (deps    (logger.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (logger.topscript.stamp))
  (deps    (logger.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? logger.topscript logger.topscript.corrected)))))

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

(alias ((name code) (deps (catch_all.topscript.stamp))))

(alias ((name sexp) (deps (catch_all.topscript.sexp))))

(rule (
  (targets (catch_all.topscript.sexp))
  (deps    (catch_all.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (catch_all.topscript.stamp))
  (deps    (catch_all.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? catch_all.topscript catch_all.topscript.corrected)))))

