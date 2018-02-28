(jbuild_version 1)

(alias ((name sexp) (deps (run_map_vs_hash.sh.sexp))))

(rule (
  (targets (run_map_vs_hash.sh.sexp))
  (deps (run_map_vs_hash.sh map_vs_hash.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

