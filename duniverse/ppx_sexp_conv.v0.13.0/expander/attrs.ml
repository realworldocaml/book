open! Base
open! Ppxlib

let default =
  Attribute.declare "sexp.default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)

let drop_default =
  Attribute.declare "sexp.sexp_drop_default"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (alt_option (pstr_eval __ nil ^:: nil) nil))
    (fun x -> x)

let drop_default_equal =
  Attribute.declare "sexp.@sexp_drop_default.equal"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let drop_default_compare =
  Attribute.declare "sexp.@sexp_drop_default.compare"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let drop_default_sexp =
  Attribute.declare "sexp.@sexp_drop_default.sexp"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let drop_if =
  Attribute.declare "sexp.sexp_drop_if"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)

let opaque =
  Attribute.declare "sexp.opaque"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()

let omit_nil =
  Attribute.declare "sexp.omit_nil"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let option =
  Attribute.declare "sexp.option"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let list =
  Attribute.declare "sexp.list"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let array =
  Attribute.declare "sexp.array"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let bool =
  Attribute.declare "sexp.bool"
    Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

let list_variant =
  Attribute.declare "sexp.list"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()

let list_exception =
  Attribute.declare "sexp.list"
    Attribute.Context.type_exception
    Ast_pattern.(pstr nil)
    ()

let list_poly =
  Attribute.declare "sexp.list"
    Attribute.Context.rtag
    Ast_pattern.(pstr nil)
    ()

let allow_extra_fields_td =
  Attribute.declare "sexp.allow_extra_fields"
    Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    ()

let allow_extra_fields_cd =
  Attribute.declare "sexp.allow_extra_fields"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()


let invalid_attribute ~loc attr description =
  Location.raise_errorf ~loc
    "ppx_sexp_conv: [@%s] is only allowed on type [%s]."
    (Attribute.name attr)
    description

let fail_if_allow_extra_field_cd ~loc x =
  if Option.is_some (Attribute.get allow_extra_fields_cd x)
  then
    Location.raise_errorf ~loc
      "ppx_sexp_conv: [@@allow_extra_fields] is only allowed on \
       inline records."

let fail_if_allow_extra_field_td ~loc x =
  if Option.is_some (Attribute.get allow_extra_fields_td x)
  then
    match x.ptype_kind with
    | Ptype_variant cds
      when List.exists cds
             ~f:(fun cd -> match cd.pcd_args with Pcstr_record _ -> true | _ -> false)
      ->
      Location.raise_errorf ~loc
        "ppx_sexp_conv: [@@@@allow_extra_fields] only works on records. \
         For inline records, do: type t = A of { a : int } [@@allow_extra_fields] | B \
         [@@@@deriving sexp]"
    | _ ->
      Location.raise_errorf ~loc
        "ppx_sexp_conv: [@@@@allow_extra_fields] is only allowed on \
         records."

module Record_field_handler = struct

  type common =
    [ `omit_nil
    | `sexp_array of core_type
    | `sexp_bool
    | `sexp_list of core_type
    | `sexp_option of core_type
    ]

  let get_attribute attr ld ~f =
    Option.map (Attribute.get attr ld) ~f:(fun x -> f x, Attribute.name attr)
  ;;

  let create ~loc getters ld =
    let common_getters =
      [ get_attribute omit_nil ~f:(fun () -> `omit_nil)
      ; (fun ld ->
           match ld.pld_type with
           | [%type: sexp_bool ] -> Some (`sexp_bool, "sexp_bool")
           | [%type: [%t? ty] sexp_option ] -> Some (`sexp_option ty, "sexp_option")
           | [%type: [%t? ty] sexp_list ] -> Some (`sexp_list ty, "sexp_list")
           | [%type: [%t? ty] sexp_array ] -> Some (`sexp_array ty, "sexp_array")
           | ty when Option.is_some (Attribute.get bool ld) ->
             (match ty with
              | [%type: bool] -> Some (`sexp_bool, "[@sexp.bool]")
              | _ -> invalid_attribute ~loc bool "bool")
           | ty when Option.is_some (Attribute.get option ld) ->
             (match ty with
              | [%type: [%t? ty] option] -> Some (`sexp_option ty, "[@sexp.option]")
              | _ -> invalid_attribute ~loc option "_ option")
           | ty when Option.is_some (Attribute.get list ld) ->
             (match ty with
              | [%type: [%t? ty] list] -> Some (`sexp_list ty, "[@sexp.list]")
              | _ -> invalid_attribute ~loc list "_ list")
           | ty when Option.is_some (Attribute.get array ld) ->
             (match ty with
              | [%type: [%t? ty] array] -> Some (`sexp_array ty, "[@sexp.array]")
              | _ -> invalid_attribute ~loc array "_ array")
           | _ -> None)
      ]
    in
    match List.filter_map (getters @ common_getters) ~f:(fun f -> f ld) with
    | [] -> None
    | [ (v, _) ] -> Some v
    | _ :: _ :: _ as attributes ->
      Location.raise_errorf ~loc "The following elements are mutually exclusive: %s"
        (String.concat ~sep:" " (List.map attributes ~f:snd))
  ;;

  module Of_sexp = struct
    type t =
      [ common
      | `default of expression
      ]

    let create ~loc ld =
      create ~loc [ get_attribute default ~f:(fun default -> `default default) ] ld
  end

  module Sexp_of = struct
    type t =
      [ common
      | `drop_default of [ `no_arg | `compare | `equal | `sexp | `func of expression ]
      | `drop_if of expression
      | `keep
      ]

    let create ~loc ld =
      create
        ~loc
        [ get_attribute drop_default ~f:(function
            | None -> `drop_default `no_arg
            | Some e -> `drop_default (`func e))
        ; get_attribute drop_default_equal ~f:(fun () -> `drop_default `equal)
        ; get_attribute drop_default_compare
            ~f:(fun () -> `drop_default `compare)
        ; get_attribute drop_default_sexp
            ~f:(fun () -> `drop_default `sexp)
        ; get_attribute drop_if ~f:(fun x -> `drop_if x)
        ]
        ld
    |> Option.value ~default:`keep

  end
end
