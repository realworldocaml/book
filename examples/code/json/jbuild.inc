(jbuild_version 1)

(alias (
  (name code)
  (deps (install.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name code)
  (deps (parse_book.topscript book.json))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name code)
  (deps (build_json.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (build_github_atd.sh.sexp))))

(rule (
  (targets (build_github_atd.sh.sexp))
  (deps    (build_github_atd.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

