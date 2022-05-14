open Base
open Ppxlib
open Ast_builder.Default
module Attrs = Attrs
module Record_field_attrs = Record_field_attrs
open Expand_sexp_of
open Expand_of_sexp

module Sexp_of = struct
  let type_extension ty =
    Sig_generate_sexp_of.type_of_sexp_of ~loc:{ ty.ptyp_loc with loc_ghost = true } ty
  ;;

  let core_type ty = Str_generate_sexp_of.sexp_of_core_type ty
  let sig_type_decl = Sig_generate_sexp_of.mk_sig
  let sig_exception = Sig_generate_sexp_of.mk_sig_exn
  let str_type_decl = Str_generate_sexp_of.sexp_of_tds
  let str_exception = Str_generate_sexp_of.sexp_of_exn
end

module Sexp_grammar = Ppx_sexp_conv_grammar

module Of_sexp = struct
  let type_extension ty = Sig_generate_of_sexp.type_of_of_sexp ~loc:ty.ptyp_loc ty
  let core_type = Str_generate_of_sexp.core_type_of_sexp

  let sig_type_decl ~poly ~loc ~path tds =
    Sig_generate_of_sexp.mk_sig ~poly ~loc ~path tds
  ;;

  let str_type_decl ~loc ~poly ~path tds =
    Str_generate_of_sexp.tds_of_sexp ~loc ~poly ~path tds
  ;;
end

module Sig_sexp = struct
  let mk_sig ~loc ~path decls =
    List.concat
      [ Sig_generate_sexp_of.mk_sig ~loc ~path decls
      ; Sig_generate_of_sexp.mk_sig ~poly:false ~loc ~path decls
      ]
  ;;

  let sig_type_decl ~loc ~path ((_rf, tds) as decls) =
    match
      mk_named_sig
        ~loc
        ~sg_name:"Sexplib0.Sexpable.S"
        ~handle_polymorphic_variant:false
        tds
    with
    | Some include_infos -> [ psig_include ~loc include_infos ]
    | None -> mk_sig ~loc ~path decls
  ;;
end
