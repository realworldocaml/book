(jbuild_version 1)

(alias ((name sexp) (deps (basic_md5_with_default_file.ml.sexp))))

(rule (
  (targets (basic_md5_with_default_file.ml.sexp))
  (deps    (basic_md5_with_default_file.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_basic_and_default_md5.sh.sexp))))

(rule (
  (targets (build_basic_and_default_md5.sh.sexp))
  (deps    (build_basic_and_default_md5.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_basic_and_default_md5.sh.sexp))))

(rule (
  (targets (run_basic_and_default_md5.sh.sexp))
  (deps    (run_basic_and_default_md5.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (basic_md5_with_optional_file.ml.sexp))))

(rule (
  (targets (basic_md5_with_optional_file.ml.sexp))
  (deps    (basic_md5_with_optional_file.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

