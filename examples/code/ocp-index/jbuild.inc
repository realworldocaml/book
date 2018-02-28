(jbuild_version 1)

(alias ((name sexp) (deps (index_ncurses.sh.sexp))))

(rule (
  (targets (index_ncurses.sh.sexp))
  (deps    (index_ncurses.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

