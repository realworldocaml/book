(jbuild_version 1)

(alias ((name sexp) (deps (alice.mli.sexp))))

(rule (
  (targets (alice.mli.sexp))
  (deps    (alice.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (inconsistent_compilation_units.rawsh.sexp))))

(rule (
  (targets (inconsistent_compilation_units.rawsh.sexp))
  (deps    (inconsistent_compilation_units.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (follow_on_function.ml.sexp))))

(rule (
  (targets (follow_on_function.ml.sexp))
  (deps    (follow_on_function.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (doc.ml.sexp))))

(rule (
  (targets (doc.ml.sexp))
  (deps    (doc.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (broken_poly_with_annot.ml.sexp))))

(rule (
  (targets (broken_poly_with_annot.ml.sexp))
  (deps    (broken_poly_with_annot.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (install_ocp_index.rawsh.sexp))))

(rule (
  (targets (install_ocp_index.rawsh.sexp))
  (deps    (install_ocp_index.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (infer_typedef.sh.sexp))))

(rule (
  (targets (infer_typedef.sh.sexp))
  (deps    (infer_typedef.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_type_conv_with_camlp4.rawsh.sexp))))

(rule (
  (targets (build_type_conv_with_camlp4.rawsh.sexp))
  (deps    (build_type_conv_with_camlp4.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_principal_corebuild.sh.sexp))))

(rule (
  (targets (build_principal_corebuild.sh.sexp))
  (deps    (build_principal_corebuild.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (comparelib_test.ml.sexp))))

(rule (
  (targets (comparelib_test.ml.sexp))
  (deps    (comparelib_test.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (unused_var.ml.sexp))))

(rule (
  (targets (unused_var.ml.sexp))
  (deps    (unused_var.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_broken_poly_with_annot.errsh.sexp))))

(rule (
  (targets (build_broken_poly_with_annot.errsh.sexp))
  (deps    (build_broken_poly_with_annot.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (indent_follow_on_function.sh.sexp))))

(rule (
  (targets (indent_follow_on_function.sh.sexp))
  (deps    (indent_follow_on_function.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (fixed_module.ml.sexp))))

(rule (
  (targets (fixed_module.ml.sexp))
  (deps    (fixed_module.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (typedtree_typedef.sh.sexp))))

(rule (
  (targets (typedtree_typedef.sh.sexp))
  (deps    (typedtree_typedef.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (broken_poly.ml.sexp))))

(rule (
  (targets (broken_poly.ml.sexp))
  (deps    (broken_poly.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (process_comparelib_interface.sh.sexp))))

(rule (
  (targets (process_comparelib_interface.sh.sexp))
  (deps    (process_comparelib_interface.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (conflicting_interface.ml.sexp))))

(rule (
  (targets (conflicting_interface.ml.sexp))
  (deps    (conflicting_interface.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (alice_combined.ml.sexp))))

(rule (
  (targets (alice_combined.ml.sexp))
  (deps    (alice_combined.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_type_conv_without_camlp4.errsh.sexp))))

(rule (
  (targets (build_type_conv_without_camlp4.errsh.sexp))
  (deps    (build_type_conv_without_camlp4.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (comparelib_test.mli.sexp))))

(rule (
  (targets (comparelib_test.mli.sexp))
  (deps    (comparelib_test.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_principal.sh.sexp))))

(rule (
  (targets (build_principal.sh.sexp))
  (deps    (build_principal.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (let_unit.syntax.sexp))))

(rule (
  (targets (let_unit.syntax.sexp))
  (deps    (let_unit.syntax))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (short_paths_3.rawsh.sexp))))

(rule (
  (targets (short_paths_3.rawsh.sexp))
  (deps    (short_paths_3.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (short_paths_1.rawsh.sexp))))

(rule (
  (targets (short_paths_1.rawsh.sexp))
  (deps    (short_paths_1.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (let_notunit.ml.sexp))))

(rule (
  (targets (let_notunit.ml.sexp))
  (deps    (let_notunit.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (conflicting_interfaces.errsh.sexp))))

(rule (
  (targets (conflicting_interfaces.errsh.sexp))
  (deps    (conflicting_interfaces.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_broken_module.errsh.sexp))))

(rule (
  (targets (build_broken_module.errsh.sexp))
  (deps    (build_broken_module.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (principal.ml.sexp))))

(rule (
  (targets (principal.ml.sexp))
  (deps    (principal.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (indent_follow_on_function_fixed.sh.sexp))))

(rule (
  (targets (indent_follow_on_function_fixed.sh.sexp))
  (deps (indent_follow_on_function_fixed.sh follow_on_function_fixed.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (camlp4_dump.cmd.sexp))))

(rule (
  (targets (camlp4_dump.cmd.sexp))
  (deps    (camlp4_dump.cmd))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (parsetree_typedef.sh.sexp))))

(rule (
  (targets (parsetree_typedef.sh.sexp))
  (deps    (parsetree_typedef.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (typedef_objinfo.sh.sexp))))

(rule (
  (targets (typedef_objinfo.sh.sexp))
  (deps    (typedef_objinfo.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (typedef.ml.sexp))))

(rule (
  (targets (typedef.ml.sexp))
  (deps    (typedef.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_non_principal.sh.sexp))))

(rule (
  (targets (build_non_principal.sh.sexp))
  (deps    (build_non_principal.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (short_paths_2.rawsh.sexp))))

(rule (
  (targets (short_paths_2.rawsh.sexp))
  (deps    (short_paths_2.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (alice.ml.sexp))))

(rule (
  (targets (alice.ml.sexp))
  (deps    (alice.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_ocamldoc.rawsh.sexp))))

(rule (
  (targets (build_ocamldoc.rawsh.sexp))
  (deps    (build_ocamldoc.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (conflicting_interface.mli.sexp))))

(rule (
  (targets (conflicting_interface.mli.sexp))
  (deps    (conflicting_interface.mli))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (type_conv_example.ml.sexp))))

(rule (
  (targets (type_conv_example.ml.sexp))
  (deps    (type_conv_example.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (follow_on_function_fixed.ml.sexp))))

(rule (
  (targets (follow_on_function_fixed.ml.sexp))
  (deps    (follow_on_function_fixed.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (non_principal.ml.sexp))))

(rule (
  (targets (non_principal.ml.sexp))
  (deps    (non_principal.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name code) (deps (camlp4_toplevel.topscript.stamp))))

(alias ((name sexp) (deps (camlp4_toplevel.topscript.sexp))))

(rule (
  (targets (camlp4_toplevel.topscript.sexp))
  (deps    (camlp4_toplevel.topscript))
  (action (
    with-stdout-to ${@} (
      run ocaml-topexpect -dry-run -sexp -short-paths -verbose ${<})))))

(rule (
  (targets (camlp4_toplevel.topscript.stamp))
  (deps    (camlp4_toplevel.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (write-file ${@} "")
    (diff? camlp4_toplevel.topscript camlp4_toplevel.topscript.corrected)))))

(alias ((name sexp) (deps (build_follow_on_function.errsh.sexp))))

(rule (
  (targets (build_follow_on_function.errsh.sexp))
  (deps    (build_follow_on_function.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (broken_module.ml.sexp))))

(rule (
  (targets (broken_module.ml.sexp))
  (deps    (broken_module.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_broken_poly.errsh.sexp))))

(rule (
  (targets (build_broken_poly.errsh.sexp))
  (deps    (build_broken_poly.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (process_comparelib_test.sh.sexp))))

(rule (
  (targets (process_comparelib_test.sh.sexp))
  (deps    (process_comparelib_test.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

