(jbuild_version 1)

(alias ((name sexp) (deps (infer_ncurses.sh.sexp))))

(rule (
  (targets (infer_ncurses.sh.sexp))
  (deps    (infer_ncurses.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

