(** Overrides the Longident module of OCaml *)

open! Import

type t = longident = Lident of string | Ldot of t * string | Lapply of t * t

val compare : t -> t -> int
val sexp_of_t : t -> Sexp.t
val flatten_exn : t -> string list
val last_exn : t -> string

val parse : string -> t
(** Parses the given string as a longident, properly handling infix operators
    which may contain '.'. Note that it does not parse [Lapply _] longidents and
    will raise [Invalid_argument _] if passed values such as ["A(B)"]. *)

val name : t -> string

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
