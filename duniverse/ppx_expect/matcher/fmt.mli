(** Representation of parsed [%expect] lines *)

open! Base
open Base.Exported_for_specific_uses (* for [Ppx_compare_lib] *)

type t =
  | Regexp of string
  | Glob of string
  | Literal of string
[@@deriving_inline sexp_of, compare, equal]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_t : t -> Sexplib0.Sexp.t

  include Ppx_compare_lib.Comparable.S with type t := t
  include Ppx_compare_lib.Equal.S with type t := t
end
[@@ocaml.doc "@inline"]

[@@@end]
