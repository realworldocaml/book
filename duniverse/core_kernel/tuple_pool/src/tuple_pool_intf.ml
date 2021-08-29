(** A manual memory manager for a set of mutable tuples.  The point of [Tuple_pool] is to
    allocate a single long-lived block of memory (the pool) that lives in the OCaml major
    heap, and then to reuse the block, rather than continually allocating blocks on the
    minor heap.

    A pool stores a bounded-size set of tuples, where client code is responsible for
    explicitly controlling when the pool allocates and frees tuples.  One [create]s a
    pool of a certain capacity, which returns an empty pool that can hold that many
    tuples.  One then uses [new] to allocate a tuple, which returns a [Pointer.t] to the
    tuple.  One then uses [get] and [set] along with the pointer to get and set slots of
    the tuple.  Finally, one [free]'s a pointer to the pool's memory for a tuple, making
    the memory available for subsequent reuse.

    In typical usage, one wraps up a pool with an abstract interface, giving nice names
    to the tuple slots, and only exposing mutation where desired.

    All the usual problems with manual memory allocation are present with pools:

    - one can mistakenly use a pointer after it is freed
    - one can mistakenly free a pointer multiple times
    - one can forget to free a pointer

    There is a debugging functor, [Tuple_pool.Error_check], that is useful for building
    pools to help debug incorrect pointer usage. *)

open! Core_kernel
open! Import

(** [S] is the module type for a pool. *)
module type S = sig
  module Slots : Tuple_type.Slots
  module Slot : Tuple_type.Slot

  module Pointer : sig
    (** A pointer to a tuple in a pool.  ['slots] will look like [('a1, ..., 'an)
        Slots.tn], and the tuples have type ['a1 * ... * 'an]. *)
    type 'slots t [@@deriving sexp_of, typerep]

    (** The [null] pointer is a distinct pointer that does not correspond to a tuple in
        the pool.  It is a function to prevent problems due to the value restriction. *)
    val null : unit -> _ t

    val is_null : _ t -> bool
    val phys_compare : 'a t -> 'a t -> int
    val phys_equal : 'a t -> 'a t -> bool

    module Id : sig
      type t [@@deriving bin_io, sexp]

      val to_int63 : t -> Int63.t
      val of_int63 : Int63.t -> t
    end
  end

  (** A pool.  ['slots] will look like [('a1, ..., 'an) Slots.tn], and the pool holds
      tuples of type ['a1 * ... * 'an]. *)
  type 'slots t [@@deriving sexp_of]

  include Invariant.S1 with type 'a t := 'a t

  (** [pointer_is_valid t pointer] returns [true] iff [pointer] points to a live tuple in
      [t], i.e. [pointer] is not null, not free, and is in the range of [t].

      A pointer might not be in the range of a pool if it comes from another pool for
      example.  In this case unsafe_get/set functions would cause a segfault. *)
  val pointer_is_valid : 'slots t -> 'slots Pointer.t -> bool

  (** [id_of_pointer t pointer] returns an id that is unique for the lifetime of
      [pointer]'s tuple.  When the tuple is freed, the id is no longer valid, and
      [pointer_of_id_exn] will fail on it.  [Pointer.null ()] has a distinct id from all
      non-null pointers. *)
  val id_of_pointer : 'slots t -> 'slots Pointer.t -> Pointer.Id.t

  (** [pointer_of_id_exn t id] returns the pointer corresponding to [id].  It fails if the
      tuple corresponding to [id] was already [free]d. *)
  val pointer_of_id_exn : 'slots t -> Pointer.Id.t -> 'slots Pointer.t

  (** [create slots ~capacity ~dummy] creates an empty pool that can hold up to [capacity]
      N-tuples.  The slots of [dummy] are stored in free tuples.  [create] raises if
      [capacity < 0 || capacity > max_capacity ~slots_per_tuple]. *)
  val create
    :  (('tuple, _) Slots.t as 'slots)
    -> capacity:int
    -> dummy:'tuple
    -> 'slots t

  (** [max_capacity] returns the maximum capacity allowed when creating a pool. *)
  val max_capacity : slots_per_tuple:int -> int

  (** [capacity] returns the maximum number of tuples that the pool can hold. *)
  val capacity : _ t -> int

  (** [length] returns the number of tuples currently in the pool.

      {[
        0 <= length t <= capacity t
      ]}
  *)
  val length : _ t -> int

  (** [grow t ~capacity] returns a new pool [t'] with the supplied capacity.  The new pool
      is to be used as a replacement for [t].  All live tuples in [t] are now live in
      [t'], and valid pointers to tuples in [t] are now valid pointers to the identical
      tuple in [t'].  It is an error to use [t] after calling [grow t].

      [grow] raises if the supplied capacity isn't larger than [capacity t]. *)
  val grow : ?capacity:int (** default is [2 * capacity t] *) -> 'a t -> 'a t

  (** [is_full t] returns [true] if no more tuples can be allocated in [t]. *)
  val is_full : _ t -> bool

  (** [free t pointer] frees the tuple pointed to by [pointer] from [t]. *)
  val free : 'slots t -> 'slots Pointer.t -> unit

  (** [unsafe_free t pointer] frees the tuple pointed to by [pointer] without checking
      [pointer_is_valid] *)
  val unsafe_free : 'slots t -> 'slots Pointer.t -> unit

  (** [new<N> t a0 ... a<N-1>] returns a new tuple from the pool, with the tuple's
      slots initialized to [a0] ... [a<N-1>].  [new] raises if [is_full t]. *)
  val new1 : ('a0 Slots.t1 as 'slots) t -> 'a0 -> 'slots Pointer.t

  val new2 : (('a0, 'a1) Slots.t2 as 'slots) t -> 'a0 -> 'a1 -> 'slots Pointer.t

  val new3
    :  (('a0, 'a1, 'a2) Slots.t3 as 'slots) t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'slots Pointer.t

  val new4
    :  (('a0, 'a1, 'a2, 'a3) Slots.t4 as 'slots) t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'slots Pointer.t

  val new5
    :  (('a0, 'a1, 'a2, 'a3, 'a4) Slots.t5 as 'slots) t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'a4
    -> 'slots Pointer.t

  val new6
    :  (('a0, 'a1, 'a2, 'a3, 'a4, 'a5) Slots.t6 as 'slots) t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'a4
    -> 'a5
    -> 'slots Pointer.t

  val new7
    :  (('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6) Slots.t7 as 'slots) t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'a4
    -> 'a5
    -> 'a6
    -> 'slots Pointer.t

  val new8
    :  (('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) Slots.t8 as 'slots) t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'a4
    -> 'a5
    -> 'a6
    -> 'a7
    -> 'slots Pointer.t

  val new9
    :  (('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) Slots.t9 as 'slots) t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'a4
    -> 'a5
    -> 'a6
    -> 'a7
    -> 'a8
    -> 'slots Pointer.t

  val new10
    :  (('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9) Slots.t10 as 'slots) t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'a4
    -> 'a5
    -> 'a6
    -> 'a7
    -> 'a8
    -> 'a9
    -> 'slots Pointer.t

  val new11
    :  (('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10) Slots.t11 as 'slots) t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'a4
    -> 'a5
    -> 'a6
    -> 'a7
    -> 'a8
    -> 'a9
    -> 'a10
    -> 'slots Pointer.t

  val new12
    :  (('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11) Slots.t12 as 'slots)
         t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'a4
    -> 'a5
    -> 'a6
    -> 'a7
    -> 'a8
    -> 'a9
    -> 'a10
    -> 'a11
    -> 'slots Pointer.t

  val new13
    :  (('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11, 'a12) Slots.t13
        as
        'slots)
         t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'a4
    -> 'a5
    -> 'a6
    -> 'a7
    -> 'a8
    -> 'a9
    -> 'a10
    -> 'a11
    -> 'a12
    -> 'slots Pointer.t

  val new14
    :  (( 'a0
        , 'a1
        , 'a2
        , 'a3
        , 'a4
        , 'a5
        , 'a6
        , 'a7
        , 'a8
        , 'a9
        , 'a10
        , 'a11
        , 'a12
        , 'a13 )
          Slots.t14
        as
        'slots)
         t
    -> 'a0
    -> 'a1
    -> 'a2
    -> 'a3
    -> 'a4
    -> 'a5
    -> 'a6
    -> 'a7
    -> 'a8
    -> 'a9
    -> 'a10
    -> 'a11
    -> 'a12
    -> 'a13
    -> 'slots Pointer.t

  (** [get_tuple t pointer] allocates an OCaml tuple isomorphic to the pool [t]'s tuple
      pointed to by [pointer]. The tuple gets copied, but its slots do not. *)
  val get_tuple : (('tuple, _) Slots.t as 'slots) t -> 'slots Pointer.t -> 'tuple

  (** [get t pointer slot] gets [slot] of the tuple pointed to by [pointer] in
      pool [t].

      [set t pointer slot a] sets to [a] the [slot] of the tuple pointed to by [pointer]
      in pool [t].

      In [get] and [set], it is an error to refer to a pointer that has been [free]d.  It
      is also an error to use a pointer with any pool other than the one the pointer was
      [new]'d from or [grow]n to.  These errors will lead to undefined behavior, but will
      not segfault.

      [unsafe_get] is comparable in speed to [get] for immediate values, and 5%-10% faster
      for pointers.

      [unsafe_get] and [unsafe_set] skip bounds checking, and can thus segfault. *)
  val get
    :  ((_, 'variant) Slots.t as 'slots) t
    -> 'slots Pointer.t
    -> ('variant, 'slot) Slot.t
    -> 'slot

  val unsafe_get
    :  ((_, 'variant) Slots.t as 'slots) t
    -> 'slots Pointer.t
    -> ('variant, 'slot) Slot.t
    -> 'slot

  val set
    :  ((_, 'variant) Slots.t as 'slots) t
    -> 'slots Pointer.t
    -> ('variant, 'slot) Slot.t
    -> 'slot
    -> unit

  val unsafe_set
    :  ((_, 'variant) Slots.t as 'slots) t
    -> 'slots Pointer.t
    -> ('variant, 'slot) Slot.t
    -> 'slot
    -> unit
end

module type Tuple_pool = sig
  module Tuple_type = Tuple_type

  module type S = S


  (** This uses a [Uniform_array.t] to implement the pool.  We expose that [Pointer.t] is
      an [int] so that OCaml can avoid the write barrier, due to knowing that [Pointer.t]
      isn't an OCaml pointer. *)
  include
    S with type 'a Pointer.t = private int
  (** @inline *)

  (** An [Unsafe] pool is like an ordinary pool, except that the [create] function does
      not require an initial element.  The pool stores a dummy value for each slot.
      Such a pool is only safe if one never accesses a slot from a [free]d tuple.

      It makes sense to use [Unsafe] if one has a small constrained chunk of code where
      one can prove that one never accesses a [free]d tuple, and one needs a pool where
      it is difficult to construct a dummy value.

      Some [Unsafe] functions are faster than the corresponding safe version because they
      do not have to maintain values with the correct represention in the [Uniform_array]
      backing the pool: [free], [create], [grow].
  *)
  module Unsafe : sig
    include S with type 'a Pointer.t = private int

    (** [create slots ~capacity] creates an empty pool that can hold up to [capacity]
        N-tuples.  The elements of a [free] tuple may contain stale and/or invalid values
        for their types, and as such any access to a [free] tuple from this pool is
        unsafe. *)
    val create : ((_, _) Slots.t as 'slots) -> capacity:int -> 'slots t
  end

  (** [Debug] builds a pool in which every function can run [invariant] on its pool
      argument(s) and/or print a debug message to stderr, as determined by
      [!check_invariant] and [!show_messages], which are initially both [true].

      The performance of the pool resulting from [Debug] is much worse than that of the
      input [Tuple_pool], even with all the controls set to [false]. *)
  module Debug (Tuple_pool : S) : sig
    include
      S
      with type 'a Pointer.t = 'a Tuple_pool.Pointer.t
      with type Pointer.Id.t = Tuple_pool.Pointer.Id.t
      with type 'a t = 'a Tuple_pool.t

    val check_invariant : bool ref
    val show_messages : bool ref
  end

  (** [Error_check] builds a pool that has additional error checking for pointers, in
      particular to detect using a [free]d pointer or multiply [free]ing a pointer.

      [Error_check] has a significant performance cost, but less than that of [Debug].

      One can compose [Debug] and [Error_check], e.g:

      {[
        module M = Debug (Error_check (Tuple_pool))
      ]}
  *)
  module Error_check (Tuple_pool : S) : S
end
