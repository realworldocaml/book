(jbuild_version 1)

(alias ((name code) (deps (examples.topscript.stamp))))

(alias ((name sexp) (deps (examples.topscript.sexp))))

(rule (
  (targets (examples.topscript.sexp))
  (deps    (examples.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (examples.topscript.stamp))
  (deps    (examples.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? examples.topscript examples.topscript.corrected)))))

(alias ((name code) (deps (value_restriction.topscript.stamp))))

(alias ((name sexp) (deps (value_restriction.topscript.sexp))))

(rule (
  (targets (value_restriction.topscript.sexp))
  (deps    (value_restriction.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (value_restriction.topscript.stamp))
  (deps    (value_restriction.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? value_restriction.topscript value_restriction.topscript.corrected)))))

(alias ((name code) (deps (weak.topscript.stamp))))

(alias ((name sexp) (deps (weak.topscript.sexp))))

(rule (
  (targets (weak.topscript.sexp))
  (deps    (weak.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (weak.topscript.stamp))
  (deps    (weak.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}           "")
    (diff?      weak.topscript weak.topscript.corrected)))))

(alias ((name code) (deps (file2.topscript.stamp))))

(alias ((name sexp) (deps (file2.topscript.sexp))))

(rule (
  (targets (file2.topscript.sexp))
  (deps    (file2.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (file2.topscript.stamp))
  (deps    (file2.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      file2.topscript file2.topscript.corrected)))))

(alias ((name code) (deps (lazy.topscript.stamp))))

(alias ((name sexp) (deps (lazy.topscript.sexp))))

(rule (
  (targets (lazy.topscript.sexp))
  (deps    (lazy.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (lazy.topscript.stamp))
  (deps    (lazy.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}           "")
    (diff?      lazy.topscript lazy.topscript.corrected)))))

(alias ((name code) (deps (file.topscript.stamp))))

(alias ((name sexp) (deps (file.topscript.sexp))))

(rule (
  (targets (file.topscript.sexp))
  (deps    (file.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (file.topscript.stamp))
  (deps    (file.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}           "")
    (diff?      file.topscript file.topscript.corrected)))))

(alias ((name code) (deps (ref.topscript.stamp))))

(alias ((name sexp) (deps (ref.topscript.sexp))))

(rule (
  (targets (ref.topscript.sexp))
  (deps    (ref.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (ref.topscript.stamp))
  (deps    (ref.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}          "")
    (diff?      ref.topscript ref.topscript.corrected)))))

(alias ((name code) (deps (printf.topscript.stamp))))

(alias ((name sexp) (deps (printf.topscript.sexp))))

(rule (
  (targets (printf.topscript.sexp))
  (deps    (printf.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (printf.topscript.stamp))
  (deps    (printf.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? printf.topscript printf.topscript.corrected)))))

(alias ((name code) (deps (letrec.topscript.stamp))))

(alias ((name sexp) (deps (letrec.topscript.sexp))))

(rule (
  (targets (letrec.topscript.sexp))
  (deps    (letrec.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (letrec.topscript.stamp))
  (deps    (letrec.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? letrec.topscript letrec.topscript.corrected)))))

(alias ((name code) (deps (order.topscript.stamp))))

(alias ((name sexp) (deps (order.topscript.sexp))))

(rule (
  (targets (order.topscript.sexp))
  (deps    (order.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (order.topscript.stamp))
  (deps    (order.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}            "")
    (diff?      order.topscript order.topscript.corrected)))))

(alias ((name code) (deps (for.topscript.stamp))))

(alias ((name sexp) (deps (for.topscript.sexp))))

(rule (
  (targets (for.topscript.sexp))
  (deps    (for.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (for.topscript.stamp))
  (deps    (for.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}          "")
    (diff?      for.topscript for.topscript.corrected)))))

(alias ((name code) (deps (fib.topscript.stamp))))

(alias ((name sexp) (deps (fib.topscript.sexp))))

(rule (
  (targets (fib.topscript.sexp))
  (deps    (fib.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (fib.topscript.stamp))
  (deps    (fib.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}          "")
    (diff?      fib.topscript fib.topscript.corrected)))))

(alias ((name code) (deps (memo.topscript.stamp))))

(alias ((name sexp) (deps (memo.topscript.sexp))))

(rule (
  (targets (memo.topscript.sexp))
  (deps    (memo.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (memo.topscript.stamp))
  (deps    (memo.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@}           "")
    (diff?      memo.topscript memo.topscript.corrected)))))

