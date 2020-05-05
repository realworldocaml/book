(** Runtime library for OCaml validators. *)

val validate_list
  : (([> `Index of int ] as 'a) list -> 'b -> 'c option)
  -> 'a list
  -> 'b list
  -> 'c option

val validate_array
  : (([> `Index of int ] as 'a) list -> 'b -> 'c option)
  -> 'a list
  -> 'b array
  -> 'c option

val validate_option : ('a -> 'b -> 'c option) -> 'a -> 'b option -> 'c option
