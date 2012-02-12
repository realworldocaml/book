open Core.Std

type t

val empty : t
val touch : t -> string -> t
val to_list : t -> (string * int) list

