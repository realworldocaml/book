(** A module containing the ad-hoc polymorphic comparison functions.  Useful when
    you want to use polymorphic compare in some small scope of a file within which
    polymorphic compare has been hidden *)

external compare : 'a -> 'a -> int = "%compare"

(** [ascending] is identical to [compare].  [descending x y = ascending y x].  These are
    intended to be mnemonic when used like [List.sort ~compare:ascending] and [List.sort
    ~compare:descending], since they cause the list to be sorted in ascending or
    descending order, respectively. *)
val ascending : 'a -> 'a -> int

val descending : 'a -> 'a -> int
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( = ) : 'a -> 'a -> bool = "%equal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external equal : 'a -> 'a -> bool = "%equal"
val min : 'a -> 'a -> 'a
val max : 'a -> 'a -> 'a
