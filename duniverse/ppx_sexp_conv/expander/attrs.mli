open! Base
open! Ppxlib

val default : (label_declaration, expression) Attribute.t

val drop_default : (label_declaration, expression option) Attribute.t

val drop_if : (label_declaration, expression) Attribute.t

val opaque : (core_type, unit) Attribute.t

val list_variant : (constructor_declaration, unit) Attribute.t

val list_exception : (type_exception, unit) Attribute.t

val list_poly : (row_field, unit) Attribute.t

val allow_extra_fields_td : (type_declaration, unit) Attribute.t

val allow_extra_fields_cd : (constructor_declaration, unit) Attribute.t

val invalid_attribute : loc:Location.t -> (_, _) Attribute.t -> string -> 'a

val fail_if_allow_extra_field_cd : loc:Location.t -> constructor_declaration -> unit

val fail_if_allow_extra_field_td : loc:Location.t -> type_declaration -> unit

module Record_field_handler : sig

  type common =
    [ `omit_nil
    | `sexp_array of core_type
    | `sexp_bool
    | `sexp_list of core_type
    | `sexp_option of core_type
    ]

  module Of_sexp : sig
    type t =
      [ common
      | `default of expression
      ]

    val create : loc:Location.t -> label_declaration -> t option
  end

  module Sexp_of : sig
    type t =
      [ common
      | `drop_default of [ `no_arg | `compare | `equal | `sexp | `func of expression ]
      | `drop_if of expression
      | `keep
      ]

    val create : loc:Location.t -> label_declaration -> t
  end
end
