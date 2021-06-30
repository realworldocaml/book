(** [Lazy_group_id] is a cheap way to allocate unique integer identifiers for sexp
    grammars. See [sexp_intf.ml] for details. *)

type t

val compare : t -> t -> int

val create : unit -> t
val force : t -> int
