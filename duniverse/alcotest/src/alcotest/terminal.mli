type dimensions = { rows : int; columns : int }
(** Get the dimensions of the terminal *)

val get_dimensions : unit -> dimensions option
