(jbuild_version 1)

(alias ((name sexp) (deps (run_basic_md5.sh.sexp))))

(rule (
  (targets (run_basic_md5.sh.sexp))
  (deps (run_basic_md5.sh build_basic_md5.sh.sexp))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_basic_md5.sh.sexp))))

(rule (
  (targets (build_basic_md5.sh.sexp))
  (deps (build_basic_md5.sh basic_md5.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (get_basic_md5_help.errsh.sexp))))

(rule (
  (targets (get_basic_md5_help.errsh.sexp))
  (deps (get_basic_md5_help.errsh build_basic_md5.sh.sexp))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (get_basic_md5_version.sh.sexp))))

(rule (
  (targets (get_basic_md5_version.sh.sexp))
  (deps (get_basic_md5_version.sh build_basic_md5.sh.sexp))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

