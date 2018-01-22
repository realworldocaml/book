(jbuild_version 1)

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_basic_md5_with_custom_arg.errsh.sexp))))

(rule (
  (targets (run_basic_md5_with_custom_arg.errsh.sexp))
  (deps (
    run_basic_md5_with_custom_arg.errsh
    jbuild
    jbuild.inc
    basic_md5_with_custom_arg.ml))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (basic_md5_with_custom_arg.ml.sexp))))

(rule (
  (targets (basic_md5_with_custom_arg.ml.sexp))
  (deps    (basic_md5_with_custom_arg.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

