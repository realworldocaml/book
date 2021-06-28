(** Functional heaps (implemented as
    {{: http://en.wikipedia.org/wiki/Pairing_heap} pairing heaps}). *)

open! Core_kernel

(** [t_of_sexp] is not supported, because of the difficulty involved in recreating the
    comparison function. *)
type 'a t [@@deriving sexp_of]

(** Even though [min_elt], [max_elt], and [to_list] are in [Container.S1], they are
    documented separately to make sure there is no confusion. *)
include
  Container.S1 with type 'a t := 'a t

(** The comparison functions in [min_elt] and [max_elt] are independent of the one used to
    order the heap. Since the provided [compare] may be different from the one used to
    create the heap, it is necessary for these functions to traverse the entire heap. If
    you want to access the smallest element of the heap according to the heap's comparison
    function, you should use [top]. *)
val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option

val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option

(** The elements of [to_list t] are not in any particular order.  You need to sort the
    list afterwards if you want to get a sorted list. *)
val to_list : 'a t -> 'a list

(** [create ~cmp] returns a new min-heap that uses ordering function [cmp].

    The top of the heap is the smallest element as determined by the provided comparison
    function. *)
val create : cmp:('a -> 'a -> int) -> 'a t

val of_array : 'a array -> cmp:('a -> 'a -> int) -> 'a t
val of_list : 'a list -> cmp:('a -> 'a -> int) -> 'a t

(** [add t v] returns the new heap after addition.  Complexity O(1). *)
val add : 'a t -> 'a -> 'a t

(** This returns the top (i.e., smallest) element of the heap.  Complexity O(1). *)
val top : 'a t -> 'a option

val top_exn : 'a t -> 'a

(** [remove_top t] returns the new heap after a remove.  It does nothing if [t]
    is empty.

    The amortized time per [remove_top t] (or [pop t], [pop_exn t], [pop_if t]) is O(lg
    n).  The complexity of the worst case is O(n). *)
val remove_top : 'a t -> 'a t option

(** This removes and returns the top (i.e., least) element and the modified heap. *)
val pop : 'a t -> ('a * 'a t) option

val pop_exn : 'a t -> 'a * 'a t

(** [pop_if t cond] returns [Some (top_element, rest_of_heap)] if [t] is not empty and its
    top element satisfies condition [cond], or [None] in any other case. *)
val pop_if : 'a t -> ('a -> bool) -> ('a * 'a t) option

(** [to_sequence t] is a sequence of the elements of [t] in ascending order. *)
val to_sequence : 'a t -> 'a Base.Sequence.t
