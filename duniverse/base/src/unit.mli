(** Module for the type [unit]. *)

open! Import

type t = unit [@@deriving_inline enumerate, sexp, sexp_grammar]

include Ppx_enumerate_lib.Enumerable.S with type t := t
include Sexplib0.Sexpable.S with type t := t

val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

[@@@end]

include Identifiable.S with type t := t
include Invariant.S with type t := t
