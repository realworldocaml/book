(** Heap implementation based on a pairing-heap.

    This heap implementations supports an arbitrary element type via a comparison
    function. *)

open! Core_kernel

(** of_sexp and bin_io functions aren't supplied for heaps due to the difficulties in
    reconstructing the correct comparison function when de-serializing. *)
type 'a t [@@deriving sexp_of]

(** Mutation of the heap during iteration is not supported, but there is no check to
    prevent it.  The behavior of a heap that is mutated during iteration is
    undefined. *)
include
  Container.S1 with type 'a t := 'a t

include Invariant.S1 with type 'a t := 'a t

(** Even though these two functions [min_elt] and [max_elt] are part of Container.S1, they
    are documented separately to make sure there is no confusion. They are independent of
    the comparison function used to order the heap. Instead, a traversal of the entire
    structure is done using the provided [cmp] function to find a min or max.

    If you want to access the smallest element of the heap according to the heap's
    comparison function in constant time, you should use [top]. *)

val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option

(** [create ?min_size ~cmp] returns a new min-heap that can store [min_size] elements
    without reallocations, using ordering function [cmp].

    The top of the heap is the smallest element as determined by the provided comparison
    function.  In particular, if [cmp x y < 0] then [x] will be "on top of" [y] in the
    heap.

    Memory use can be surprising in that the underlying pool never shrinks, so current
    memory use will at least be proportional to the largest number of elements that the
    heap has ever held.
*)
val create : ?min_size:int -> cmp:('a -> 'a -> int) -> unit -> 'a t

(** [min_size] (see [create]) will be set to the size of the input array or list. *)
val of_array : 'a array -> cmp:('a -> 'a -> int) -> 'a t

val of_list : 'a list -> cmp:('a -> 'a -> int) -> 'a t

(** Returns the top (i.e., smallest) element of the heap. *)
val top : 'a t -> 'a option

val top_exn : 'a t -> 'a
val add : 'a t -> 'a -> unit

(** [remove_top t] does nothing if [t] is empty. *)
val remove_top : _ t -> unit


(** [pop] removes and returns the top (i.e. least) element. *)
val pop : 'a t -> 'a option

val pop_exn : 'a t -> 'a

(** [pop_if t cond] returns [Some top_element] of [t] if it satisfies condition
    [cond], removing it, or [None] in any other case. *)
val pop_if : 'a t -> ('a -> bool) -> 'a option

(** [copy t] returns a shallow copy. *)
val copy : 'a t -> 'a t

module Elt : sig
  type 'a t [@@deriving sexp_of]

  (** [value_exn t] returns the value in the heap controlled by this token if the
      value is still in the heap, and raises otherwise. *)
  val value_exn : 'a t -> 'a
end

(** [add_removable t v] adds [v] to [t], returning a token that can be used to delete
    [v] from [t] in lg(n) amortized time.

    Note that while [add] doesn't allocate unless the underlying pool needs to be resized,
    [add_removable] always allocates. The [Unsafe] module has a non-allocating
    alternative.
*)
val add_removable : 'a t -> 'a -> 'a Elt.t

(** If [t] and [token] are mismatched then behavior is undefined. Trying to
    [remove] an already removed token (by an earlier call to [remove] or [pop] for
    instance) is a no-op, but keeping [token] around after it has been removed may lead
    to memory leaks since it has a reference to the heap. *)
val remove : 'a t -> 'a Elt.t -> unit


(** [update t token v] is shorthand for [remove t token; add_removable t v]. *)
val update : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t

(** [find_elt t ~f].  If [f] is true for some element in [t], return an [Elt.t] for
    that element.  This operation is O(n). *)
val find_elt : 'a t -> f:('a -> bool) -> 'a Elt.t option

module Unsafe : sig
  (** [Unsafe] functions provide faster alternatives to regular functions with the same
      name. They don't allocate but the behavior is unspecified and could be memory unsafe
      in certain cases where regular functions would fail with informative exceptions. *)

  module Elt : sig
    type 'a heap = 'a t
    type 'a t

    (** [value t heap] returns the value in the [heap] controlled by this token if the
        value is still in the [heap] and [heap] and [t] match. Otherwise the behavior is
        unspecified and could lead to segfaults. *)
    val value : 'a t -> 'a heap -> 'a
  end

  (** [add_removable t v] returns a token that can be later used to remove [v]. Unlike
      the regular function, this unsafe version doesn't allocate. *)
  val add_removable : 'a t -> 'a -> 'a Elt.t

  (** [remove t elt] removes [elt] from [t]. Behavior is undefined and could lead to
      segfaults if [t] and [elt] don't match, if [elt] was already removed, or if the
      underlying value has already been removed (e.g., via [pop]). *)
  val remove : 'a t -> 'a Elt.t -> unit

  (** [update t token v] is shorthand for [remove t token; add_removable t v]. *)
  val update : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t
end
