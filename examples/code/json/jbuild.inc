(jbuild_version 1)

(alias ((name code) (deps (install.topscript.stamp))))

(alias ((name sexp) (deps (install.topscript.sexp))))

(rule (
  (targets (install.topscript.sexp))
  (deps    (install.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (install.topscript.stamp))
  (deps    (install.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? install.topscript install.topscript.corrected)))))

(alias ((name code) (deps (parse_book.topscript.stamp))))

(alias ((name sexp) (deps (parse_book.topscript.sexp))))

(rule (
  (targets (parse_book.topscript.sexp))
  (deps    (parse_book.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (parse_book.topscript.stamp))
  (deps (parse_book.topscript book.json))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? parse_book.topscript parse_book.topscript.corrected)))))

(alias ((name code) (deps (build_json.topscript.stamp))))

(alias ((name sexp) (deps (build_json.topscript.sexp))))

(rule (
  (targets (build_json.topscript.sexp))
  (deps    (build_json.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (build_json.topscript.stamp))
  (deps    (build_json.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? build_json.topscript build_json.topscript.corrected)))))

(alias ((name sexp) (deps (build_github_atd.sh.sexp))))

(rule (
  (targets (build_github_atd.sh.sexp))
  (deps    (build_github_atd.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

