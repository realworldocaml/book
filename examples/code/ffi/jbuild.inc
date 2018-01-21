(jbuild_version 1)

(alias ((name code) (deps (qsort.topscript.stamp))))

(alias ((name sexp) (deps (qsort.topscript.sexp))))

(rule (
  (targets (qsort.topscript.sexp))
  (deps    (qsort.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (qsort.topscript.stamp))
  (deps    (qsort.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      qsort.topscript qsort.topscript.corrected)))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name code) (deps (posix.topscript.stamp))))

(alias ((name sexp) (deps (posix.topscript.sexp))))

(rule (
  (targets (posix.topscript.sexp))
  (deps    (posix.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (posix.topscript.stamp))
  (deps    (posix.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      posix.topscript posix.topscript.corrected)))))

(alias ((name sexp) (deps (return_c_uncurried.c.sexp))))

(rule (
  (targets (return_c_uncurried.c.sexp))
  (deps    (return_c_uncurried.c))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (return_c_frag.c.sexp))))

(rule (
  (targets (return_c_frag.c.sexp))
  (deps    (return_c_frag.c))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (timeval_headers.h.sexp))))

(rule (
  (targets (timeval_headers.h.sexp))
  (deps    (timeval_headers.h))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (posix_headers.h.sexp))))

(rule (
  (targets (posix_headers.h.sexp))
  (deps    (posix_headers.h))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (install.rawsh.sexp))))

(rule (
  (targets (install.rawsh.sexp))
  (deps    (install.rawsh))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (return_frag.ml.sexp))))

(rule (
  (targets (return_frag.ml.sexp))
  (deps    (return_frag.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (return_c_frag.h.sexp))))

(rule (
  (targets (return_c_frag.h.sexp))
  (deps    (return_c_frag.h))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

