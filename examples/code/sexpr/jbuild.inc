(jbuild_version 1)

(alias ((name code) (deps (inline_sexp.topscript.stamp))))

(alias ((name sexp) (deps (inline_sexp.topscript.sexp))))

(rule (
  (targets (inline_sexp.topscript.sexp))
  (deps    (inline_sexp.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (inline_sexp.topscript.stamp))
  (deps    (inline_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? inline_sexp.topscript inline_sexp.topscript.corrected)))))

(alias ((name code) (deps (sexp_default.topscript.stamp))))

(alias ((name sexp) (deps (sexp_default.topscript.sexp))))

(rule (
  (targets (sexp_default.topscript.sexp))
  (deps    (sexp_default.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (sexp_default.topscript.stamp))
  (deps    (sexp_default.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? sexp_default.topscript sexp_default.topscript.corrected)))))

(alias ((name code) (deps (to_from_sexp.topscript.stamp))))

(alias ((name sexp) (deps (to_from_sexp.topscript.sexp))))

(rule (
  (targets (to_from_sexp.topscript.sexp))
  (deps    (to_from_sexp.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (to_from_sexp.topscript.stamp))
  (deps    (to_from_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? to_from_sexp.topscript to_from_sexp.topscript.corrected)))))

(alias ((name code) (deps (sexp_option.topscript.stamp))))

(alias ((name sexp) (deps (sexp_option.topscript.sexp))))

(rule (
  (targets (sexp_option.topscript.sexp))
  (deps    (sexp_option.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (sexp_option.topscript.stamp))
  (deps    (sexp_option.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? sexp_option.topscript sexp_option.topscript.corrected)))))

(alias ((name code) (deps (sexp_opaque.topscript.stamp))))

(alias ((name sexp) (deps (sexp_opaque.topscript.sexp))))

(rule (
  (targets (sexp_opaque.topscript.sexp))
  (deps    (sexp_opaque.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (sexp_opaque.topscript.stamp))
  (deps    (sexp_opaque.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? sexp_opaque.topscript sexp_opaque.topscript.corrected)))))

(alias ((name code) (deps (print_sexp.topscript.stamp))))

(alias ((name sexp) (deps (print_sexp.topscript.sexp))))

(rule (
  (targets (print_sexp.topscript.sexp))
  (deps    (print_sexp.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (print_sexp.topscript.stamp))
  (deps    (print_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? print_sexp.topscript print_sexp.topscript.corrected)))))

(alias ((name code) (deps (manually_making_sexp.topscript.stamp))))

(alias ((name sexp) (deps (manually_making_sexp.topscript.sexp))))

(rule (
  (targets (manually_making_sexp.topscript.sexp))
  (deps    (manually_making_sexp.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (manually_making_sexp.topscript.stamp))
  (deps    (manually_making_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff?
     manually_making_sexp.topscript
     manually_making_sexp.topscript.corrected)))))

(alias ((name sexp) (deps (list_top_packages.sh.sexp))))

(rule (
  (targets (list_top_packages.sh.sexp))
  (deps    (list_top_packages.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name code) (deps (example_load.topscript.stamp))))

(alias ((name sexp) (deps (example_load.topscript.sexp))))

(rule (
  (targets (example_load.topscript.sexp))
  (deps    (example_load.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (example_load.topscript.stamp))
  (deps (
    example_load.topscript example.scm example_broken.scm comment_heavy.scm))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? example_load.topscript example_load.topscript.corrected)))))

(alias ((name code) (deps (auto_making_sexp.topscript.stamp))))

(alias ((name sexp) (deps (auto_making_sexp.topscript.sexp))))

(rule (
  (targets (auto_making_sexp.topscript.sexp))
  (deps    (auto_making_sexp.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (auto_making_sexp.topscript.stamp))
  (deps    (auto_making_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? auto_making_sexp.topscript auto_making_sexp.topscript.corrected)))))

(alias ((name code) (deps (sexp_list.topscript.stamp))))

(alias ((name sexp) (deps (sexp_list.topscript.sexp))))

(rule (
  (targets (sexp_list.topscript.sexp))
  (deps    (sexp_list.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (sexp_list.topscript.stamp))
  (deps    (sexp_list.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? sexp_list.topscript sexp_list.topscript.corrected)))))

(alias ((name code) (deps (sexp_printer.topscript.stamp))))

(alias ((name sexp) (deps (sexp_printer.topscript.sexp))))

(rule (
  (targets (sexp_printer.topscript.sexp))
  (deps    (sexp_printer.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (sexp_printer.topscript.stamp))
  (deps    (sexp_printer.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? sexp_printer.topscript sexp_printer.topscript.corrected)))))

