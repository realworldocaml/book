(** A module internal to [Core_bench]. Please look at {!Bench}.

    Arrays in which only some initial segment is meaningful. *)
open! Core

type 'a t

val create : values:'a array -> len:int -> 'a t
val map_to_array : 'a t -> f:('a -> 'b) -> 'b array
