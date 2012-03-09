(* file: int_interval.mli *)
(* Module for representing closed integer intervals *)

type t with sexp

val empty : t
val create : int -> int -> t
val contains : t -> int -> bool
