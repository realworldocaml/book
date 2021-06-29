(** Module for the type [unit]. *)

open! Import

type t = unit [@@deriving_inline enumerate, sexp, sexp_grammar]

val all : t list

include Ppx_sexp_conv_lib.Sexpable.S with type t := t

val t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t

[@@@end]

include Identifiable.S with type t := t
include Invariant.S with type t := t
