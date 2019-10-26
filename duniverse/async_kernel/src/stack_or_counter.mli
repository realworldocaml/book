(** An ['a Stack_or_counter.t] is like an ['a Stack.t], except for ['a = unit],
    it can be implemented with a counter. *)

open! Base

type 'a t [@@deriving sexp_of]

val clear : _ t -> unit
val create_counter : length:int -> unit t
val iter : 'a t -> f:('a -> unit) -> unit
val length : _ t -> int
val of_list : 'a list -> 'a t
val pop_exn : 'a t -> 'a
val push : 'a t -> 'a -> unit
