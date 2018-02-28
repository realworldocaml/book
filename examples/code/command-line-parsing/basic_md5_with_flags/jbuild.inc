(jbuild_version 1)

(alias ((name sexp) (deps (build_basic_md5_with_flags.sh.sexp))))

(rule (
  (targets (build_basic_md5_with_flags.sh.sexp))
  (deps (
    build_basic_md5_with_flags.sh basic_md5_with_flags.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (run_basic_md5_flags_help.sh.sexp))))

(rule (
  (targets (run_basic_md5_flags_help.sh.sexp))
  (deps (
    run_basic_md5_flags_help.sh
    build_basic_md5_with_flags.sh.sexp
    jbuild
    jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (md5_completion.sh.sexp))))

(rule (
  (targets (md5_completion.sh.sexp))
  (deps (
    md5_completion.sh build_basic_md5_with_flags.sh.sexp jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

