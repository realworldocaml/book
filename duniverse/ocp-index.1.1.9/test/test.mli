type trec = { f1: int; f2: string }
type tsum = T1 | T2 of string
(** tsum *)
(** tsum2 *)
type tsum2 = T1 of unit | T2
(** tsum2 suite *)

module M : sig
  type t
  val v : int -> string
  exception Toto of string
end

module type Sig = sig type tsig end

module MS : Sig

module F : functor (M:Sig) -> Sig

module S2 : sig include module type of String end

module StringMap : Map.S
