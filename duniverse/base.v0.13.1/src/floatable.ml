(** Module type with float conversion functions. *)

open! Import

module type S = sig
  type t

  val of_float : float -> t
  val to_float : t -> float
end
