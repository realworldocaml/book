(jbuild_version 1)

(alias ((name sexp) (deps (better_echo.ml.sexp))))

(rule (
  (targets (better_echo.ml.sexp))
  (deps    (better_echo.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (pipe_write_break.rawscript.sexp))))

(rule (
  (targets (pipe_write_break.rawscript.sexp))
  (deps    (pipe_write_break.rawscript))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_native_code_log_delays.rawsh.sexp))))

(rule (
  (targets (run_native_code_log_delays.rawsh.sexp))
  (deps    (run_native_code_log_delays.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (search_out_of_order.ml.sexp))))

(rule (
  (targets (search_out_of_order.ml.sexp))
  (deps    (search_out_of_order.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_echo.rawsh.sexp))))

(rule (
  (targets (run_echo.rawsh.sexp))
  (deps    (run_echo.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (search_with_timeout.ml.sexp))))

(rule (
  (targets (search_with_timeout.ml.sexp))
  (deps    (search_with_timeout.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

