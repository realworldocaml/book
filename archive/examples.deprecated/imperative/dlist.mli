open Core.Std

type 'a t
type 'a element

(** Basic list operations  *)
val create   : unit -> 'a t
val is_empty : 'a t -> bool

(** navigating using [element]s *)
val first : 'a t -> 'a element option
val next  : 'a element -> 'a element option
val prev  : 'a element -> 'a element option
val value : 'a element -> 'a

(** Simple iteration functions *)
val iter    : 'a t -> f:('a -> unit) -> unit
val find_el : 'a t -> f:('a -> bool) -> 'a element option

(** mutators *)
val insert_first : 'a t -> 'a -> 'a element
val insert_after : 'a element -> 'a -> 'a element
val remove : 'a t -> 'a element -> unit
val set : 'a t -> 'a -> unit
