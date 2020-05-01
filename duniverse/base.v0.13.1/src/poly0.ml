(** Primitives for polymorphic compare. *)

(*_ Polymorphic compiler primitives can't be aliases as this doesn't play well with
  inlining. (If aliased without a type annotation, the compiler would implement them
  using the generic code doing a C call, and it's this code that would be inlined.) As a
  result we have to copy the [external ...] declaration here. *)
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( = ) : 'a -> 'a -> bool = "%equal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external ascending : 'a -> 'a -> int = "%compare"
external compare : 'a -> 'a -> int = "%compare"
external equal : 'a -> 'a -> bool = "%equal"

let descending x y = compare y x
let max = Caml.max
let min = Caml.min
