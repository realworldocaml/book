(jbuild_version 1)

(alias ((name sexp) (deps (query-syntax.scm.sexp))))

(rule (
  (targets (query-syntax.scm.sexp))
  (deps    (query-syntax.scm))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (pack.syntax.sexp))))

(rule (
  (targets (pack.syntax.sexp))
  (deps    (pack.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (loader_cli3.rawsh.sexp))))

(rule (
  (targets (loader_cli3.rawsh.sexp))
  (deps    (loader_cli3.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (loader_cli1.rawsh.sexp))))

(rule (
  (targets (loader_cli1.rawsh.sexp))
  (deps    (loader_cli1.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (query_example.rawscript.sexp))))

(rule (
  (targets (query_example.rawscript.sexp))
  (deps    (query_example.rawscript))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (loader_cli4.rawsh.sexp))))

(rule (
  (targets (loader_cli4.rawsh.sexp))
  (deps    (loader_cli4.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (loader_cli2.rawsh.sexp))))

(rule (
  (targets (loader_cli2.rawsh.sexp))
  (deps    (loader_cli2.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (unpack.syntax.sexp))))

(rule (
  (targets (unpack.syntax.sexp))
  (deps    (unpack.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name code) (deps (query_handler.topscript.stamp))))

(alias ((name sexp) (deps (query_handler.topscript.sexp))))

(rule (
  (targets (query_handler.topscript.sexp))
  (deps    (query_handler.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (query_handler.topscript.stamp))
  (deps    (query_handler.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? query_handler.topscript query_handler.topscript.corrected)))))

(alias ((name code) (deps (main.topscript.stamp))))

(alias ((name sexp) (deps (main.topscript.sexp))))

(rule (
  (targets (main.topscript.sexp))
  (deps    (main.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (main.topscript.stamp))
  (deps    (main.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}           "")
    (diff?      main.topscript main.topscript.corrected)))))

