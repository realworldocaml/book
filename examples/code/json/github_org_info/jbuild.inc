(jbuild_version 1)

(alias ((name sexp) (deps (generate_github_org_types.sh.sexp))))

(rule (
  (targets (generate_github_org_types.sh.sexp))
  (deps (generate_github_org_types.sh github_org_t.mli jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (generate_github_org_json.sh.sexp))))

(rule (
  (targets (generate_github_org_json.sh.sexp))
  (deps (generate_github_org_json.sh github_org_j.mli jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (run_github_org.sh.sexp))))

(rule (
  (targets (run_github_org.sh.sexp))
  (deps (run_github_org.sh build_github_org.sh.sexp jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_github_org.sh.sexp))))

(rule (
  (targets (build_github_org.sh.sexp))
  (deps (build_github_org.sh github_org_info.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

