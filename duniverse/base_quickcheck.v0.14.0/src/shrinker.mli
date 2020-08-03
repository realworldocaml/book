(** Shrinkers produce small values from large values. When a random test case fails, a
    shrinker finds the simplest version of the problem. *)

open! Base

type 'a t

(** {2 Basic Shrinkers} *)

(** This shrinker treats a type as atomic, never attempting to produce smaller values. *)
val atomic : _ t

include With_basic_types.S with type 'a t := 'a t (** @inline *)

val map_t : 'key t -> 'data t -> ('key, 'data, 'cmp) Map.t t
val set_t : 'elt t -> ('elt, 'cmp) Set.t t

val map_tree_using_comparator
  :  comparator:('key, 'cmp) Comparator.t
  -> 'key t
  -> 'data t
  -> ('key, 'data, 'cmp) Map.Using_comparator.Tree.t t

val set_tree_using_comparator
  :  comparator:('elt, 'cmp) Comparator.t
  -> 'elt t
  -> ('elt, 'cmp) Set.Using_comparator.Tree.t t

(** {2 Modifying Shrinkers} *)

val map : 'a t -> f:('a -> 'b) -> f_inverse:('b -> 'a) -> 'b t
val filter : 'a t -> f:('a -> bool) -> 'a t

(** Filters and maps according to [f], and provides input to [t] via [f_inverse]. Only the
    [f] direction produces options, intentionally. *)
val filter_map : 'a t -> f:('a -> 'b option) -> f_inverse:('b -> 'a) -> 'b t

(** {2 Shrinkers for Recursive Types} *)

(** Ties the recursive knot to shrink recursive types.

    For example, here is an shrinker for binary trees:

    {[
      let tree_shrinker leaf_shrinker =
        fixed_point (fun self ->
          either leaf_shrinker (both self self)
          |> map
               ~f:(function
                 | First leaf -> `Leaf leaf
                 | Second (l, r) -> `Node (l, r))
               ~f_inverse:(function
                 | `Leaf leaf -> First leaf
                 | `Node (l, r) -> Second (l, r)))
    ]}
*)
val fixed_point : ('a t -> 'a t) -> 'a t

(** Creates a [t] that forces the lazy argument as necessary. Can be used to tie
    (mutually) recursive knots. *)
val of_lazy : 'a t Lazy.t -> 'a t

(** {2 Low-level functions}

    Most users will not need to call these.
*)

val create : ('a -> 'a Sequence.t) -> 'a t
val shrink : 'a t -> 'a -> 'a Sequence.t
