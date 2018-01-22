(jbuild_version 1)

(alias ((name sexp) (deps (cal_add_days.ml.sexp))))

(rule (
  (targets (cal_add_days.ml.sexp))
  (deps    (cal_add_days.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name code) (deps (basic.topscript.stamp))))

(alias ((name sexp) (deps (basic.topscript.sexp))))

(rule (
  (targets (basic.topscript.sexp))
  (deps    (basic.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (basic.topscript.stamp))
  (deps    (basic.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      basic.topscript basic.topscript.corrected)))))

(alias ((name sexp) (deps (opam.rawsh.sexp))))

(rule (
  (targets (opam.rawsh.sexp))
  (deps    (opam.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name code) (deps (step.topscript.stamp))))

(alias ((name sexp) (deps (step.topscript.sexp))))

(rule (
  (targets (step.topscript.sexp))
  (deps    (step.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (step.topscript.stamp))
  (deps    (step.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}           "")
    (diff?      step.topscript step.topscript.corrected)))))

(alias ((name code) (deps (command_types.topscript.stamp))))

(alias ((name sexp) (deps (command_types.topscript.sexp))))

(rule (
  (targets (command_types.topscript.sexp))
  (deps    (command_types.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (command_types.topscript.stamp))
  (deps    (command_types.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? command_types.topscript command_types.topscript.corrected)))))

(alias ((name sexp) (deps (cal_append.ml.sexp))))

(rule (
  (targets (cal_append.ml.sexp))
  (deps    (cal_append.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name code) (deps (group.topscript.stamp))))

(alias ((name sexp) (deps (group.topscript.sexp))))

(rule (
  (targets (group.topscript.sexp))
  (deps    (group.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (group.topscript.stamp))
  (deps    (group.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      group.topscript group.topscript.corrected)))))

(alias ((name sexp) (deps (cal_add_labels.ml.sexp))))

(rule (
  (targets (cal_add_labels.ml.sexp))
  (deps    (cal_add_labels.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (cal_completion.rawsh.sexp))))

(rule (
  (targets (cal_completion.rawsh.sexp))
  (deps    (cal_completion.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (basic_md5_succinct.ml.sexp))))

(rule (
  (targets (basic_md5_succinct.ml.sexp))
  (deps    (basic_md5_succinct.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (basic_md5_sequence.ml.sexp))))

(rule (
  (targets (basic_md5_sequence.ml.sexp))
  (deps    (basic_md5_sequence.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

