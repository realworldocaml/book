(jbuild_version 1)

(alias ((name sexp) (deps (build_basic_and_default_md5.sh.sexp))))

(rule (
  (targets (build_basic_and_default_md5.sh.sexp))
  (deps (
    build_basic_and_default_md5.sh
    basic_md5_with_default_file.ml
    jbuild
    jbuild.inc
    basic_md5_with_optional_file.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (run_basic_and_default_md5.sh.sexp))))

(rule (
  (targets (run_basic_and_default_md5.sh.sexp))
  (deps (run_basic_and_default_md5.sh build_basic_and_default_md5.sh.sexp))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

