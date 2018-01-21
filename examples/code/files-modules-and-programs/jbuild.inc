(jbuild_version 1)

(alias ((name sexp) (deps (ext_list.ml.sexp))))

(rule (
  (targets (ext_list.ml.sexp))
  (deps    (ext_list.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (common.ml.sexp))))

(rule (
  (targets (common.ml.sexp))
  (deps    (common.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (abstract_username.ml.sexp))))

(rule (
  (targets (abstract_username.ml.sexp))
  (deps    (abstract_username.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (module.syntax.sexp))))

(rule (
  (targets (module.syntax.sexp))
  (deps    (module.syntax))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (val.syntax.sexp))))

(rule (
  (targets (val.syntax.sexp))
  (deps    (val.syntax))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
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

(alias ((name code) (deps (intro.topscript.stamp))))

(alias ((name sexp) (deps (intro.topscript.sexp))))

(rule (
  (targets (intro.topscript.sexp))
  (deps    (intro.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (intro.topscript.stamp))
  (deps    (intro.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      intro.topscript intro.topscript.corrected)))))

(alias ((name sexp) (deps (ext_list.mli.sexp))))

(rule (
  (targets (ext_list.mli.sexp))
  (deps    (ext_list.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

