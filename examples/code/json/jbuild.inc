(jbuild_version 1)

(alias ((name sexp) (deps (yojson_safe.ml.sexp))))

(rule (
  (targets (yojson_safe.ml.sexp))
  (deps    (yojson_safe.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (yojson_basic.ml.sexp))))

(rule (
  (targets (yojson_basic.ml.sexp))
  (deps    (yojson_basic.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (install_atdgen.rawsh.sexp))))

(rule (
  (targets (install_atdgen.rawsh.sexp))
  (deps    (install_atdgen.rawsh))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (list_excerpt.mli.sexp))))

(rule (
  (targets (list_excerpt.mli.sexp))
  (deps    (list_excerpt.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (yojson_basic_simple.mli.sexp))))

(rule (
  (targets (yojson_basic_simple.mli.sexp))
  (deps    (yojson_basic_simple.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

(alias ((name sexp) (deps (list_excerpt.ml.sexp))))

(rule (
  (targets (list_excerpt.ml.sexp))
  (deps    (list_excerpt.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (yojson_basic.mli.sexp))))

(rule (
  (targets (yojson_basic.mli.sexp))
  (deps    (yojson_basic.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (book.json.sexp))))

(rule (
  (targets (book.json.sexp))
  (deps    (book.json))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

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

(alias ((name sexp) (deps (github_t.mli.sexp))))

(rule (
  (targets (github_t.mli.sexp))
  (deps    (github_t.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (github.atd.sexp))))

(rule (
  (targets (github.atd.sexp))
  (deps    (github.atd))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (yojson_safe.mli.sexp))))

(rule (
  (targets (yojson_safe.mli.sexp))
  (deps    (yojson_safe.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (github_t.ml.sexp))))

(rule (
  (targets (github_t.ml.sexp))
  (deps    (github_t.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (github_j_excerpt.ml.sexp))))

(rule (
  (targets (github_j_excerpt.ml.sexp))
  (deps    (github_j_excerpt.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_github_atd.sh.sexp))))

(rule (
  (targets (build_github_atd.sh.sexp))
  (deps    (build_github_atd.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (github_j.mli.sexp))))

(rule (
  (targets (github_j.mli.sexp))
  (deps    (github_j.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (github_j.ml.sexp))))

(rule (
  (targets (github_j.ml.sexp))
  (deps    (github_j.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (yojson_basic_simple.ml.sexp))))

(rule (
  (targets (yojson_basic_simple.ml.sexp))
  (deps    (yojson_basic_simple.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (github_j_excerpt.mli.sexp))))

(rule (
  (targets (github_j_excerpt.mli.sexp))
  (deps    (github_j_excerpt.mli))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

