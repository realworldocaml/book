type t [@@deriving sexp]

(** [create lo hi] creates an interval from [lo] to [hi] inclusive,
   and is empty if [lo > hi]. *)
val create : int -> int -> t

val is_empty : t -> bool
val contains : t -> int -> bool
