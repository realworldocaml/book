(jbuild_version 1)

(alias ((name sexp) (deps (infer_ncurses.sh.sexp))))

(rule (
  (targets (infer_ncurses.sh.sexp))
  (deps    (infer_ncurses.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (ncurses.mli.sexp))))

(rule (
  (targets (ncurses.mli.sexp))
  (deps    (ncurses.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (ncurses.inferred.mli.sexp))))

(rule (
  (targets (ncurses.inferred.mli.sexp))
  (deps    (ncurses.inferred.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (ncurses.h.sexp))))

(rule (
  (targets (ncurses.h.sexp))
  (deps    (ncurses.h))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (ncurses.ml.sexp))))

(rule (
  (targets (ncurses.ml.sexp))
  (deps    (ncurses.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

