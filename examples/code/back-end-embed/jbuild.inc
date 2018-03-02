(jbuild_version 1)

(alias ((name sexp) (deps (build_embed.sh.sexp))))

(rule (
  (targets (build_embed.sh.sexp))
  (deps (build_embed.sh embed_me1.ml embed_me2.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_embed_c.sh.sexp))))

(rule (
  (targets (build_embed_c.sh.sexp))
  (deps (build_embed_c.sh embed_me1.ml embed_me2.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (run_debug_hello.sh.sexp))))

(rule (
  (targets (run_debug_hello.sh.sexp))
  (deps    (run_debug_hello.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

