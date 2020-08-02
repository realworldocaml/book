
open Sexplib

val sexp   : tag:string -> Sexp.t Lazy.t -> unit
val sexps  : tag:string -> Sexp.t Lazy.t list -> unit

val sexpf  : tag:string -> f:('a -> Sexp.t) -> 'a -> unit
val sexpfs : tag:string -> f:('a -> Sexp.t) -> 'a list -> unit

val cs     : tag:string -> Cstruct.t -> unit
val css    : tag:string -> Cstruct.t list -> unit
