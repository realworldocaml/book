open! Base
open! Ppxlib

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
