type t [@@deriving sexp]

val is_empty : t -> bool
val create : int -> int -> t
val contains : t -> int -> bool
