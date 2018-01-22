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

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (basic_md5_as_filename.ml.sexp))))

(rule (
  (targets (basic_md5_as_filename.ml.sexp))
  (deps    (basic_md5_as_filename.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

