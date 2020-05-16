(** [state] is defined as a subtype of [int] using the [private] keyword. This makes it an
    opaque type for most purposes, and tells the compiler that the type is immediate. *)
type state = private int
type seed = int
type hash_value = int

external create_seeded  : seed            -> state = "%identity"                   [@@noalloc]
external fold_int64     : state -> int64  -> state = "Base_internalhash_fold_int64"     [@@noalloc]
external fold_int       : state -> int    -> state = "Base_internalhash_fold_int"       [@@noalloc]
external fold_float     : state -> float  -> state = "Base_internalhash_fold_float"     [@@noalloc]
external fold_string    : state -> string -> state = "Base_internalhash_fold_string"    [@@noalloc]
external get_hash_value : state -> hash_value      = "Base_internalhash_get_hash_value" [@@noalloc]
