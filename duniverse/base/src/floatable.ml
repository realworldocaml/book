(** Functor that adds float conversion functions to a module. *)

open! Import

module type S = sig
  type t

  val of_float : float -> t
  val to_float : t -> float
end
