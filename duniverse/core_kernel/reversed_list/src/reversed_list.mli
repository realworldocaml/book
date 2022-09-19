(** [Reversed_list] is constructed the same way as a list, but it needs to be reversed
    before it can be used. This is helpful when building up a list in reverse order to
    force reversal before use. *)
type 'a t =
  | []
  | ( :: ) of 'a * 'a t
[@@deriving sexp_of]

(** [of_list_rev] reverses the input list. *)
val of_list_rev : 'a list -> 'a t

val rev : 'a t -> 'a list
val rev_append : 'a t -> 'a list -> 'a list
val rev_map : 'a t -> f:('a -> 'b) -> 'b list
val rev_filter_map : 'a t -> f:('a -> 'b option) -> 'b list
val is_empty : 'a t -> bool
val length : 'a t -> int
val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
