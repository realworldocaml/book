open Ppxlib

val hash_fold_type : loc:Location.t -> core_type -> core_type
val hash_fold_core_type : core_type -> expression

(** [hash_core_type ty] is an expression of type [Hash.state -> ty -> Hash.state] *)
val hash_type : loc:Location.t -> core_type -> core_type
val hash_core_type : core_type -> expression

val str_attributes : Attribute.packed list

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
