open Ppxlib
module Attrs = Attrs
module Record_field_attrs = Record_field_attrs

module Sexp_of : sig
  val type_extension : core_type -> core_type
  val core_type : core_type -> expression

  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature

  val sig_exception : loc:Location.t -> path:string -> type_exception -> signature

  val str_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> structure

  val str_exception : loc:Location.t -> path:string -> type_exception -> structure
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
    -> poly:bool (** the type is annotated with sexp_poly instead of sexp *)
    -> path:string (** the module path within the file *)
    -> rec_flag * type_declaration list
    -> structure
end

module Sexp_grammar : sig
  val type_extension : ctxt:Expansion_context.Extension.t -> core_type -> core_type

  val core_type
    :  tags_of_doc_comments:bool
    -> ctxt:Expansion_context.Extension.t
    -> core_type
    -> expression

  val sig_type_decl
    :  ctxt:Expansion_context.Deriver.t
    -> rec_flag * type_declaration list
    -> signature

  val str_type_decl
    :  ctxt:Expansion_context.Deriver.t
    -> rec_flag * type_declaration list
    -> bool (** [true] means capture doc comments as tags *)
    -> structure
end

module Sig_sexp : sig
  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature
end
