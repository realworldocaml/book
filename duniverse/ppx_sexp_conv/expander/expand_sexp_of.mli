open! Base
open! Ppxlib

module Sig_generate_sexp_of : sig
  (** Given a type, produce the type of its [sexp_of] conversion. *)
  val type_of_sexp_of : loc:location -> core_type -> core_type

  (** Derive a [sexp_of] interface for a list of type declarations. *)
  val mk_sig
    :  loc:location
    -> path:string
    -> rec_flag * type_declaration list
    -> signature_item list

  (** Derive a [sexp_of] interface for an exception declaration. *)
  val mk_sig_exn : loc:location -> path:string -> type_exception -> signature_item list
end

module Str_generate_sexp_of : sig
  (** Given a type, produce its [sexp_of] conversion. *)
  val sexp_of_core_type : core_type -> expression

  (** Derive a [sexp_of] implementation for a list of type declarations. *)
  val sexp_of_tds
    :  loc:location
    -> path:string
    -> rec_flag * type_declaration list
    -> structure_item list

  (** Derive a [sexp_of] implementation for an exception declaration. *)
  val sexp_of_exn : loc:location -> path:string -> type_exception -> structure_item list
end
