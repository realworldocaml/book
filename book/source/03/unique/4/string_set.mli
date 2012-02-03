open Core.Std

type t
val empty : t
val add : t -> string -> t
val mem : t -> string -> bool
