open Core.Std

type 'a dlist
type 'a element

val create : unit -> 'a dlist
val is_empty : 'a dlist -> bool

val value : 'a element -> 'a

val find     : 'a dlist   -> 'a -> 'a element option
val first    : 'a dlist   -> 'a element option
val next     : 'a element -> 'a element option
val previous : 'a element -> 'a element option

val insert_first : 'a dlist -> 'a -> 'a element
val insert_after : 'a element -> 'a -> 'a element
val remove : 'a dlist -> 'a element -> unit

val iter : 'a dlist -> f:('a -> unit) -> unit

type 'a iterator =
  < has_value : bool;
  value : 'a;
  next : unit;
  remove : unit;
  insert_after : 'a -> unit
                 >

val iterator : 'a dlist -> 'a iterator
val find_it : 'a dlist -> data:'a -> 'a iterator
