open Core

type t
val t_of_sexp : Sexp.t -> t
val sexp_of_t : t -> Sexp.t

val is_empty : t -> bool
val create : int -> int -> t
val contains : t -> int -> bool
