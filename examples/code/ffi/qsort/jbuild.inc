(jbuild_version 1)

(alias ((name sexp) (deps (qsort_typedef.h.sexp))))

(rule (
  (targets (qsort_typedef.h.sexp))
  (deps    (qsort_typedef.h))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_qsort.sh.sexp))))

(rule (
  (targets (build_qsort.sh.sexp))
  (deps (input.txt build_qsort.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (qsort.ml.sexp))))

(rule (
  (targets (qsort.ml.sexp))
  (deps    (qsort.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (qsort.mli.sexp))))

(rule (
  (targets (qsort.mli.sexp))
  (deps    (qsort.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (qsort.h.sexp))))

(rule (
  (targets (qsort.h.sexp))
  (deps    (qsort.h))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

