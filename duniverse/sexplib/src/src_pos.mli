(** source positions, both relative and absolute *)

module Relative : sig
  type t = { row : int; col : int }
  val sexp_of_t : t -> Type.t

  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
end

module Absolute : sig
  type t = { row : int; col : int }
  val sexp_of_t : t -> Type.t

  val origin : t (* first row, first column *)

  val of_lexing : Lexing.position -> t

  val diff : t -> t -> Relative.t

  val add : t -> Relative.t -> t
  val sub : t -> Relative.t -> t

  (*val compare : t -> t -> int*)
  val geq     : t -> t -> bool
end

