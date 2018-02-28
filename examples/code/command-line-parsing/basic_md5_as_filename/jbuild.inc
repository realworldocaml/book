(jbuild_version 1)

(alias ((name sexp) (deps (run_basic_md5_as_filename.errsh.sexp))))

(rule (
  (targets (run_basic_md5_as_filename.errsh.sexp))
  (deps (
    run_basic_md5_as_filename.errsh
    basic_md5_as_filename.ml
    jbuild
    jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

