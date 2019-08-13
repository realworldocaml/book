(** A buffer of items which can grow in size by appending to the end. *)
type 'a t

val create : ?capacity:int -> unit -> 'a t
val append : 'a t -> 'a -> unit
val nth_exn : 'a t -> int -> 'a
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val to_array : 'a t -> 'a array
val length : 'a t -> int

(** Clear the buffer so it is empty again.

    This is lax in that clearing the buffer does
    not necessarily allow things that were in the
    buffer to be garbage collected.
*)
val lax_clear : 'a t -> unit
