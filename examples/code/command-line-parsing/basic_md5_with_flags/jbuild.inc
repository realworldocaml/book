(jbuild_version 1)

(alias ((name sexp) (deps (build_basic_md5_with_flags.sh.sexp))))

(rule (
  (targets (build_basic_md5_with_flags.sh.sexp))
  (deps    (build_basic_md5_with_flags.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (basic_md5_with_flags.ml.sexp))))

(rule (
  (targets (basic_md5_with_flags.ml.sexp))
  (deps    (basic_md5_with_flags.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_basic_md5_flags_help.sh.sexp))))

(rule (
  (targets (run_basic_md5_flags_help.sh.sexp))
  (deps    (run_basic_md5_flags_help.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (md5_completion.sh.sexp))))

(rule (
  (targets (md5_completion.sh.sexp))
  (deps    (md5_completion.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

