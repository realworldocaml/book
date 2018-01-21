(jbuild_version 1)

(alias ((name sexp) (deps (result.ml.sexp))))

(rule (
  (targets (result.ml.sexp))
  (deps    (result.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (sexpr.scm.sexp))))

(rule (
  (targets (sexpr.scm.sexp))
  (deps    (sexpr.scm))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (try_with.syntax.sexp))))

(rule (
  (targets (try_with.syntax.sexp))
  (deps    (try_with.syntax))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (result.mli.sexp))))

(rule (
  (targets (result.mli.sexp))
  (deps    (result.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

