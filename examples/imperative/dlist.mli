open Core.Std

type 'a t
type 'a element

val create   : unit -> 'a t
val is_empty : 'a t -> bool

val first        : 'a t       -> 'a element option
val insert_first : 'a t       -> 'a -> 'a element
val insert_after : 'a element -> 'a -> 'a element
val value        : 'a element -> 'a
val remove       : 'a t       -> 'a element -> unit
val next         : 'a element -> 'a element option
val previous     : 'a element -> 'a element option

val find     : 'a t   -> 'a -> 'a element option
val iter : 'a t -> f:('a -> unit) -> unit

type 'a iterator =
  < has_value : bool;
    value : 'a;
    next : unit;
    remove : unit;
    insert_after : 'a -> unit >

val iterator : 'a t -> 'a iterator
val find_it : 'a t -> data:'a -> 'a iterator
