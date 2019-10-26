(** S-expressions annotated with relative source positions and comments *)

module Make (Pos : sig type t val sexp_of_t : t -> Type.t end) : sig
  module type S = sig
    (** S-expressions annotated with relative source positions and comments *)
    type t =
      | Atom of Pos.t * string * string option (* optional quoted representation *)
      | List of Pos.t * t_or_comment list * Pos.t (* left & right paren positions *)
    and t_or_comment =
      | Sexp of t
      | Comment of comment
    and comment =
      | Plain_comment of Pos.t * string (* line or block comment *)
      | Sexp_comment of Pos.t * comment list * t (* #; position *)

    val sexp_of_t : t -> Type.t
    val sexp_of_comment : comment -> Type.t
    val sexp_of_t_or_comment : t_or_comment -> Type.t
  end
end

module Parsed : Make(Src_pos.Absolute).S (** parsing produces absolute positions *)

include Make(Src_pos.Relative).S (* the exposed type contains relative positions *)

val relativize : Parsed.t_or_comment -> t_or_comment

