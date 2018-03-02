(jbuild_version 1)

(alias ((name sexp) (deps (inline_sexp.topscript.sexp))))

(rule (
  (targets (inline_sexp.topscript.sexp))
  (deps    (inline_sexp.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (inline_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (sexp_default.topscript.sexp))))

(rule (
  (targets (sexp_default.topscript.sexp))
  (deps    (sexp_default.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (sexp_default.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (to_from_sexp.topscript.sexp))))

(rule (
  (targets (to_from_sexp.topscript.sexp))
  (deps    (to_from_sexp.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (to_from_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (sexp_option.topscript.sexp))))

(rule (
  (targets (sexp_option.topscript.sexp))
  (deps    (sexp_option.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (sexp_option.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (sexp_opaque.topscript.sexp))))

(rule (
  (targets (sexp_opaque.topscript.sexp))
  (deps    (sexp_opaque.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (sexp_opaque.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (print_sexp.topscript.sexp))))

(rule (
  (targets (print_sexp.topscript.sexp))
  (deps    (print_sexp.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (print_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (manually_making_sexp.topscript.sexp))))

(rule (
  (targets (manually_making_sexp.topscript.sexp))
  (deps    (manually_making_sexp.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (manually_making_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (list_top_packages.sh.sexp))))

(rule (
  (targets (list_top_packages.sh.sexp))
  (deps    (list_top_packages.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (example_load.topscript.sexp))))

(rule (
  (targets (example_load.topscript.sexp))
  (deps    (example_load.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (
    example_load.topscript example.scm example_broken.scm comment_heavy.scm))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (auto_making_sexp.topscript.sexp))))

(rule (
  (targets (auto_making_sexp.topscript.sexp))
  (deps    (auto_making_sexp.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (auto_making_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (sexp_list.topscript.sexp))))

(rule (
  (targets (sexp_list.topscript.sexp))
  (deps    (sexp_list.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (sexp_list.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (sexp_printer.topscript.sexp))))

(rule (
  (targets (sexp_printer.topscript.sexp))
  (deps    (sexp_printer.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(alias (
  (name code)
  (deps (sexp_printer.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

