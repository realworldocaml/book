(** runtime witnes of type equality
    this is a reduced version of [Core.Type_equal]. *)
type ('a, 'b) t = ('a,'b) Base.Type_equal.t = T : ('a, 'a) t
type ('a, 'b) equal = ('a, 'b) t

(** type-safe conversion between equal types *)
val conv : ('a, 'b) t -> 'a -> 'b

(** type equality is reflexive *)
val refl : ('a, 'a) t

(** needed in some cases even though t is exported and is a gadt *)
module Lift (X: sig
  type 'a t
end) : sig
  val lift : ('a, 'b) t -> ('a X.t, 'b X.t) t
end
