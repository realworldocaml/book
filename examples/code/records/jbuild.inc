(jbuild_version 1)

(alias ((name sexp) (deps (warn_help.sh.sexp))))

(rule (
  (targets (warn_help.sh.sexp))
  (deps    (warn_help.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

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

(alias ((name code) (deps (main2.topscript.stamp))))

(alias ((name sexp) (deps (main2.topscript.sexp))))

(rule (
  (targets (main2.topscript.sexp))
  (deps    (main2.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (main2.topscript.stamp))
  (deps    (main2.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      main2.topscript main2.topscript.corrected)))))

