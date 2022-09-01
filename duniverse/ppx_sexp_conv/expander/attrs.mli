open! Base
open! Ppxlib

(** [default], [drop_default], and [drop_if] attributes are annotated with expressions
    that should be lifted out of the scope of ppx-generated temporary variables. See the
    [Lifted] module. *)
module To_lift : sig
  type 'a t = { to_lift : 'a } [@@unboxed]
end

val default : (label_declaration, expression To_lift.t) Attribute.t
val drop_default : (label_declaration, expression To_lift.t option) Attribute.t
val drop_if : (label_declaration, expression To_lift.t) Attribute.t
val drop_default_equal : (label_declaration, unit) Attribute.t
val drop_default_compare : (label_declaration, unit) Attribute.t
val drop_default_sexp : (label_declaration, unit) Attribute.t
val omit_nil : (label_declaration, unit) Attribute.t
val option : (label_declaration, unit) Attribute.t
val list : (label_declaration, unit) Attribute.t
val array : (label_declaration, unit) Attribute.t
val bool : (label_declaration, unit) Attribute.t
val opaque : (core_type, unit) Attribute.t
val list_variant : (constructor_declaration, unit) Attribute.t
val list_exception : (type_exception, unit) Attribute.t
val list_poly : (row_field, unit) Attribute.t
val allow_extra_fields_td : (type_declaration, unit) Attribute.t
val allow_extra_fields_cd : (constructor_declaration, unit) Attribute.t
val invalid_attribute : loc:Location.t -> (_, _) Attribute.t -> string -> 'a
val fail_if_allow_extra_field_cd : loc:Location.t -> constructor_declaration -> unit
val fail_if_allow_extra_field_td : loc:Location.t -> type_declaration -> unit
val tag_type : (core_type, (expression * expression) list) Attribute.t
val tag_ld : (label_declaration, (expression * expression) list) Attribute.t
val tag_cd : (constructor_declaration, (expression * expression) list) Attribute.t
val tag_poly : (row_field, (expression * expression) list) Attribute.t
