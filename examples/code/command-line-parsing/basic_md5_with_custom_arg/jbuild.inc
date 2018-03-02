(jbuild_version 1)

(alias ((name sexp) (deps (run_basic_md5_with_custom_arg.errsh.sexp))))

(rule (
  (targets (run_basic_md5_with_custom_arg.errsh.sexp))
  (deps (
    run_basic_md5_with_custom_arg.errsh
    jbuild
    jbuild.inc
    basic_md5_with_custom_arg.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

