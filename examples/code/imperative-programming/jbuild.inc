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

(alias ((name sexp) (deps (time_converter2.rawsh.sexp))))

(rule (
  (targets (time_converter2.rawsh.sexp))
  (deps    (time_converter2.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

(alias ((name sexp) (deps (dictionary.mli.sexp))))

(rule (
  (targets (dictionary.mli.sexp))
  (deps    (dictionary.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (remember_type.ml.sexp))))

(rule (
  (targets (remember_type.ml.sexp))
  (deps    (remember_type.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

(alias ((name sexp) (deps (dictionary.ml.sexp))))

(rule (
  (targets (dictionary.ml.sexp))
  (deps    (dictionary.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (let-unit.syntax.sexp))))

(rule (
  (targets (let-unit.syntax.sexp))
  (deps    (let-unit.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (let_rec.ml.sexp))))

(rule (
  (targets (let_rec.ml.sexp))
  (deps    (let_rec.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

(alias ((name sexp) (deps (dlist.mli.sexp))))

(rule (
  (targets (dlist.mli.sexp))
  (deps    (dlist.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

(alias ((name sexp) (deps (bigarray.syntax.sexp))))

(rule (
  (targets (bigarray.syntax.sexp))
  (deps    (bigarray.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (semicolon.syntax.sexp))))

(rule (
  (targets (semicolon.syntax.sexp))
  (deps    (semicolon.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

(alias ((name sexp) (deps (dictionary2.ml.sexp))))

(rule (
  (targets (dictionary2.ml.sexp))
  (deps    (dictionary2.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

(alias ((name sexp) (deps (string.syntax.sexp))))

(rule (
  (targets (string.syntax.sexp))
  (deps    (string.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (dlist.ml.sexp))))

(rule (
  (targets (dlist.ml.sexp))
  (deps    (dlist.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (array-set.syntax.sexp))))

(rule (
  (targets (array-set.syntax.sexp))
  (deps    (array-set.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (time_converter2.ml.sexp))))

(rule (
  (targets (time_converter2.ml.sexp))
  (deps    (time_converter2.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (array-get.syntax.sexp))))

(rule (
  (targets (array-get.syntax.sexp))
  (deps    (array-get.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

