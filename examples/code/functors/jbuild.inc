(jbuild_version 1)

(alias ((name sexp) (deps (multi_sharing_constraint.syntax.sexp))))

(rule (
  (targets (multi_sharing_constraint.syntax.sexp))
  (deps    (multi_sharing_constraint.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (extended_fqueue.mli.sexp))))

(rule (
  (targets (extended_fqueue.mli.sexp))
  (deps    (extended_fqueue.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (extended_fqueue.ml.sexp))))

(rule (
  (targets (extended_fqueue.ml.sexp))
  (deps    (extended_fqueue.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (destructive_sub.syntax.sexp))))

(rule (
  (targets (destructive_sub.syntax.sexp))
  (deps    (destructive_sub.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (compare_example.ml.sexp))))

(rule (
  (targets (compare_example.ml.sexp))
  (deps    (compare_example.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (fqueue.mli.sexp))))

(rule (
  (targets (fqueue.mli.sexp))
  (deps    (fqueue.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (main.15.rawtopscript.sexp))))

(rule (
  (targets (main.15.rawtopscript.sexp))
  (deps    (main.15.rawtopscript))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (sharing_constraint.syntax.sexp))))

(rule (
  (targets (sharing_constraint.syntax.sexp))
  (deps    (sharing_constraint.syntax))
  (fallback)
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

(alias ((name sexp) (deps (fqueue.ml.sexp))))

(rule (
  (targets (fqueue.ml.sexp))
  (deps    (fqueue.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (sexpable.ml.sexp))))

(rule (
  (targets (sexpable.ml.sexp))
  (deps    (sexpable.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (foldable.ml.sexp))))

(rule (
  (targets (foldable.ml.sexp))
  (deps    (foldable.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

