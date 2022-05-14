open! Base
open! Ppxlib

module To_lift = struct
  type 'a t = { to_lift : 'a } [@@unboxed]
end

open To_lift

let default =
  Attribute.declare
    "sexp.default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> { to_lift = x })
;;

let drop_default =
  Attribute.declare
    "sexp.sexp_drop_default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (alt_option (pstr_eval __ nil ^:: nil) nil))
    (function
      | None -> None
      | Some x -> Some { to_lift = x })
;;

let drop_default_equal =
  Attribute.declare
    "sexp.@sexp_drop_default.equal"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let drop_default_compare =
  Attribute.declare
    "sexp.@sexp_drop_default.compare"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let drop_default_sexp =
  Attribute.declare
    "sexp.@sexp_drop_default.sexp"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let drop_if =
  Attribute.declare
    "sexp.sexp_drop_if"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> { to_lift = x })
;;

let opaque =
  Attribute.declare "sexp.opaque" Attribute.Context.core_type Ast_pattern.(pstr nil) ()
;;

let omit_nil =
  Attribute.declare
    "sexp.omit_nil"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let option =
  Attribute.declare
    "sexp.option"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let list =
  Attribute.declare
    "sexp.list"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let array =
  Attribute.declare
    "sexp.array"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let bool =
  Attribute.declare
    "sexp.bool"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let list_variant =
  Attribute.declare
    "sexp.list"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let list_exception =
  Attribute.declare "sexp.list" Attribute.Context.type_exception Ast_pattern.(pstr nil) ()
;;

let list_poly =
  Attribute.declare "sexp.list" Attribute.Context.rtag Ast_pattern.(pstr nil) ()
;;

let allow_extra_fields_td =
  Attribute.declare
    "sexp.allow_extra_fields"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let allow_extra_fields_cd =
  Attribute.declare
    "sexp.allow_extra_fields"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let tag_attribute_for_context context =
  let open Ast_pattern in
  let key_equals_value =
    Ast_pattern.(
      pexp_apply (pexp_ident (lident (string "="))) (no_label __ ^:: no_label __ ^:: nil)
      |> pack2)
  in
  let get_captured_values ast_pattern context expression =
    Ast_pattern.to_func ast_pattern context expression.pexp_loc expression (fun x -> x)
  in
  let rec collect_sequence expression =
    match expression.pexp_desc with
    | Pexp_sequence (l, r) -> l :: collect_sequence r
    | _ -> [ expression ]
  in
  let esequence ast_pattern =
    Ast_pattern.of_func (fun context _loc expression k ->
      collect_sequence expression
      |> List.map ~f:(get_captured_values ast_pattern context)
      |> k)
  in
  Attribute.declare
    "sexp_grammar.tag"
    context
    (pstr (pstr_eval (esequence key_equals_value) nil ^:: nil))
    (fun x -> x)
;;

let tag_type = tag_attribute_for_context Core_type
let tag_ld = tag_attribute_for_context Label_declaration
let tag_cd = tag_attribute_for_context Constructor_declaration
let tag_poly = tag_attribute_for_context Rtag

let invalid_attribute ~loc attr description =
  Location.raise_errorf
    ~loc
    "ppx_sexp_conv: [@%s] is only allowed on type [%s]."
    (Attribute.name attr)
    description
;;

let fail_if_allow_extra_field_cd ~loc x =
  if Option.is_some (Attribute.get allow_extra_fields_cd x)
  then
    Location.raise_errorf
      ~loc
      "ppx_sexp_conv: [@@allow_extra_fields] is only allowed on inline records."
;;

let fail_if_allow_extra_field_td ~loc x =
  if Option.is_some (Attribute.get allow_extra_fields_td x)
  then (
    match x.ptype_kind with
    | Ptype_variant cds
      when List.exists cds ~f:(fun cd ->
        match cd.pcd_args with
        | Pcstr_record _ -> true
        | _ -> false) ->
      Location.raise_errorf
        ~loc
        "ppx_sexp_conv: [@@@@allow_extra_fields] only works on records. For inline \
         records, do: type t = A of { a : int } [@@allow_extra_fields] | B [@@@@deriving \
         sexp]"
    | _ ->
      Location.raise_errorf
        ~loc
        "ppx_sexp_conv: [@@@@allow_extra_fields] is only allowed on records.")
;;
