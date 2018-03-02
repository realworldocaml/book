(jbuild_version 1)

(alias ((name sexp) (deps (infer_typedef.sh.sexp))))

(rule (
  (targets (infer_typedef.sh.sexp))
  (deps    (infer_typedef.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_principal_corebuild.sh.sexp))))

(rule (
  (targets (build_principal_corebuild.sh.sexp))
  (deps    (build_principal_corebuild.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

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

(alias ((name sexp) (deps (typedtree_typedef.sh.sexp))))

(rule (
  (targets (typedtree_typedef.sh.sexp))
  (deps    (typedtree_typedef.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (process_comparelib_interface.sh.sexp))))

(rule (
  (targets (process_comparelib_interface.sh.sexp))
  (deps    (process_comparelib_interface.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_type_conv_without_camlp4.errsh.sexp))))

(rule (
  (targets (build_type_conv_without_camlp4.errsh.sexp))
  (deps    (build_type_conv_without_camlp4.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (build_principal.sh.sexp))))

(rule (
  (targets (build_principal.sh.sexp))
  (deps    (build_principal.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

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

(alias ((name sexp) (deps (indent_follow_on_function_fixed.sh.sexp))))

(rule (
  (targets (indent_follow_on_function_fixed.sh.sexp))
  (deps (indent_follow_on_function_fixed.sh follow_on_function_fixed.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

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

(alias ((name sexp) (deps (build_non_principal.sh.sexp))))

(rule (
  (targets (build_non_principal.sh.sexp))
  (deps    (build_non_principal.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias (
  (name code)
  (deps (camlp4_toplevel.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ${<} ${<}.corrected)))))

(alias ((name sexp) (deps (build_follow_on_function.errsh.sexp))))

(rule (
  (targets (build_follow_on_function.errsh.sexp))
  (deps    (build_follow_on_function.errsh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

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

