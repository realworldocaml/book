(jbuild_version 1)

(alias ((name sexp) (deps (github_org.atd.sexp))))

(rule (
  (targets (github_org.atd.sexp))
  (deps    (github_org.atd))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (generate_github_org_types.sh.sexp))))

(rule (
  (targets (generate_github_org_types.sh.sexp))
  (deps    (generate_github_org_types.sh))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (generate_github_org_json.sh.sexp))))

(rule (
  (targets (generate_github_org_json.sh.sexp))
  (deps    (generate_github_org_json.sh))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (run_github_org.sh.sexp))))

(rule (
  (targets (run_github_org.sh.sexp))
  (deps    (run_github_org.sh))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (github_org_info.ml.sexp))))

(rule (
  (targets (github_org_info.ml.sexp))
  (deps    (github_org_info.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_github_org.sh.sexp))))

(rule (
  (targets (build_github_org.sh.sexp))
  (deps    (build_github_org.sh))
  (fallback)
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

