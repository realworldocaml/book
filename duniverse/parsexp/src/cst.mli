(** Concrete syntax tree of s-expressions *)

(** This module exposes a type that describe the full contents of a source file containing
    s-expressions.

    One can use this type to do low-level rewriting of s-expression files.
*)

open! Import

type t =
  | Atom of
      { loc : Positions.range
      ; atom : string
      (** Source syntax of atom. The parser only fills this for atoms that are quoted in
          the source, but it makes sense for unquoted atoms too (to ensure they get
          printed unquoted). *)
      ; unescaped : string option
      }
  | List of
      { loc : Positions.range
      ; elements : t_or_comment list
      }

and t_or_comment =
  | Sexp of t
  | Comment of comment

and comment =
  | Plain_comment of
      { loc : Positions.range
      ; comment : string
      } (** Line or block comment *)
  | Sexp_comment of
      { hash_semi_pos : Positions.pos
      ; comments : comment list
      ; sexp : t
      }
[@@deriving_inline compare, sexp_of]

include sig
  [@@@ocaml.warning "-32"]

  val compare : t -> t -> int
  val compare_t_or_comment : t_or_comment -> t_or_comment -> int
  val compare_comment : comment -> comment -> int
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val sexp_of_t_or_comment : t_or_comment -> Ppx_sexp_conv_lib.Sexp.t
  val sexp_of_comment : comment -> Ppx_sexp_conv_lib.Sexp.t
end
[@@ocaml.doc "@inline"]

[@@@end]

module Forget : sig
  val t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_or_comment : t_or_comment -> Ppx_sexp_conv_lib.Sexp.t option
  val t_or_comments : t_or_comment list -> Ppx_sexp_conv_lib.Sexp.t list
end
