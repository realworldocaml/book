(jbuild_version 1)

(alias ((name sexp) (deps (htable_sig2.ml.sexp))))

(rule (
  (targets (htable_sig2.ml.sexp))
  (deps    (htable_sig2.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (substring_sig1.ml.sexp))))

(rule (
  (targets (substring_sig1.ml.sexp))
  (deps    (substring_sig1.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (numerical_deriv_alt_sig.ml.sexp))))

(rule (
  (targets (numerical_deriv_alt_sig.ml.sexp))
  (deps    (numerical_deriv_alt_sig.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (abs_diff.ml.sexp))))

(rule (
  (targets (abs_diff.ml.sexp))
  (deps    (abs_diff.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (let.syntax.sexp))))

(rule (
  (targets (let.syntax.sexp))
  (deps    (let.syntax))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (let_in.syntax.sexp))))

(rule (
  (targets (let_in.syntax.sexp))
  (deps    (let_in.syntax))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (operators.syntax.sexp))))

(rule (
  (targets (operators.syntax.sexp))
  (deps    (operators.syntax))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (numerical_derv_alt_sig.ml.sexp))))

(rule (
  (targets (numerical_derv_alt_sig.ml.sexp))
  (deps    (numerical_derv_alt_sig.ml))
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

(alias ((name sexp) (deps (substring_sig2.ml.sexp))))

(rule (
  (targets (substring_sig2.ml.sexp))
  (deps    (substring_sig2.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (abs_diff.mli.sexp))))

(rule (
  (targets (abs_diff.mli.sexp))
  (deps    (abs_diff.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (htable_sig1.ml.sexp))))

(rule (
  (targets (htable_sig1.ml.sexp))
  (deps    (htable_sig1.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (numerical_deriv_alt_sig.mli.sexp))))

(rule (
  (targets (numerical_deriv_alt_sig.mli.sexp))
  (deps    (numerical_deriv_alt_sig.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

