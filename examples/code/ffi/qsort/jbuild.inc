(jbuild_version 1)

(alias ((name sexp) (deps (qsort_typedef.h.sexp))))

(rule (
  (targets (qsort_typedef.h.sexp))
  (deps    (qsort_typedef.h))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_qsort.sh.sexp))))

(rule (
  (targets (build_qsort.sh.sexp))
  (deps (build_qsort.sh input.txt jbuild jbuild.inc qsort.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (qsort.ml.sexp))))

(rule (
  (targets (qsort.ml.sexp))
  (deps    (qsort.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (qsort.mli.sexp))))

(rule (
  (targets (qsort.mli.sexp))
  (deps    (qsort.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (qsort.h.sexp))))

(rule (
  (targets (qsort.h.sexp))
  (deps    (qsort.h))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

