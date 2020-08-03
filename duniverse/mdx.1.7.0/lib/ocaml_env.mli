(** Block environments. *)

type t = Default | User_defined of string

val name : t -> string

val mk : string option -> t

module Set : Set.S with type elt = t
