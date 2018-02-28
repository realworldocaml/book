(jbuild_version 1)

(alias (
  (name sexp) (deps (build_basic_md5_with_optional_file_broken.errsh.sexp))))

(rule (
  (targets (build_basic_md5_with_optional_file_broken.errsh.sexp))
  (deps (
    build_basic_md5_with_optional_file_broken.errsh
    basic_md5_with_optional_file_broken.ml
    jbuild
    jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

