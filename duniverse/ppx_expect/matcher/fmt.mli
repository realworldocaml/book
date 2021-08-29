(** Representation of parsed [%expect] lines *)

open! Base
open Import

type t =
  | Regexp of string
  | Glob of string
  | Literal of string
[@@deriving_inline sexp_of, compare, equal]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end
[@@ocaml.doc "@inline"]

[@@@end]
