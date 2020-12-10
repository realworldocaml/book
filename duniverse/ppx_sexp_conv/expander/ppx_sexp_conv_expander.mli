open Ppxlib

module Attrs : sig
  val default      : (label_declaration, expression) Attribute.t
  val drop_default : (label_declaration, expression option) Attribute.t
  val drop_if      : (label_declaration, expression) Attribute.t
end

module Sexp_of : sig
  val type_extension : core_type -> core_type

  val core_type : core_type -> expression

  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature

  val sig_exception
    :  loc:Location.t
    -> path:string
    -> type_exception
    -> signature

  val str_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> structure

  val str_exception
    :  loc:Location.t
    -> path:string
    -> type_exception
    -> structure
end

module Of_sexp : sig
  val type_extension : core_type -> core_type

  val core_type : path:string -> core_type -> expression

  val sig_type_decl
    :  poly:bool
    -> loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature

  val str_type_decl
    :  loc:Location.t
    -> poly:bool
    -> path:string
    -> rec_flag * type_declaration list
    -> structure
end

module Sig_sexp : sig
  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature
end
