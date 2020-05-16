open Ppxlib

module type Attrs = sig
  val ignore_label_declaration : (label_declaration, unit) Attribute.t
  val ignore_core_type : (core_type, unit) Attribute.t
end

module type S = sig
  (** [type_ ty] is [ty -> ty -> result_type] where [result_type] is [int] for [compare]
      and [bool] for [equal]. *)
  val type_ : loc:Location.t -> core_type -> core_type

  (** [core_type ty] is an expression of type [ty -> ty -> result_type] *)
  val core_type : core_type -> expression

  val str_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> structure

  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature

  module Attrs : Attrs

  val str_attributes : Attribute.packed list
end
