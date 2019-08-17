(** A splittable pseudo-random number generator (SPRNG) functions like a PRNG in that it
    can be used as a stream of random values; it can also be "split" to produce a second,
    independent stream of random values.

    This module implements a splittable pseudo-random number generator that sacrifices
    cryptographic-quality randomness in favor of performance.

    The primary difference between [Splittable_random] and {!Random} is the [State.split]
    operation for generating new pseudo-random states.  While it is easy to simulate
    [State.split] using [Random], the result has undesirable statistical properties; the
    new state does not behave independently of the original.  It is better to switch to
    [Splittable_random] if you need an operation like [State.split], as this module has
    been implemented with the statistical properties of splitting in mind.  For most other
    purposes, [Random] is likely a better choice, as its implementation passes all Diehard
    tests, while [Splittable_random] fails some Diehard tests.
*)

open! Base

module State : sig
  type t

  (** Create a new [t] seeded from the given random state. This allows nondeterministic
      initialization, for example in the case that the input state was created using
      [Random.State.make_self_init].

      Constructors like [create] and [of_int] should be called once at the start of a
      randomized computation and the resulting state should be threaded through.
      Repeatedly creating splittable random states from seeds in the middle of computation
      can defeat the SPRNG's splittable properties. *)
  val create : Random.State.t -> t

  (** Create a new [t] that will return identical results to any other [t] created with
      that integer. *)
  val of_int : int -> t

  (** [perturb t salt] adds the entropy of [salt] to [t]. *)
  val perturb : t -> int -> unit

  (** Create a copy of [t] that will return the same random samples as [t]. *)
  val copy  : t -> t

  (** [split t] produces a new state that behaves deterministically (i.e. only depending
      on the state of [t]), but pseudo-independently from [t]. This operation mutates
      [t], i.e., [t] will return different values than if this hadn't been called. *)
  val split : t -> t
end

(** Produces a random, fair boolean. *)
val bool : State.t -> bool

(** Produce a random number uniformly distributed in the given inclusive range.  (In the
    case of [float], [hi] may or may not be attainable, depending on rounding.)  *)
val int       : State.t -> lo:int       -> hi:int       -> int
val int32     : State.t -> lo:int32     -> hi:int32     -> int32
val int63     : State.t -> lo:Int63.t   -> hi:Int63.t   -> Int63.t
val int64     : State.t -> lo:int64     -> hi:int64     -> int64
val nativeint : State.t -> lo:nativeint -> hi:nativeint -> nativeint
val float     : State.t -> lo:float     -> hi:float     -> float

(** [unit_float state = float state ~lo:0. ~hi:1.], but slightly more efficient (and
    right endpoint is exclusive). *)
val unit_float : State.t -> float

module Log_uniform : sig
  (** Produce a random number in the given inclusive range, where the number of bits in
      the representation is chosen uniformly based on the given range, and then the value
      is chosen uniformly within the range restricted to the chosen bit width. Raises if
      [lo < 0 || hi < lo].

      These functions are useful for choosing numbers that are weighted low within a given
      range. *)
  val int       : State.t -> lo:int       -> hi:int       -> int
  val int32     : State.t -> lo:int32     -> hi:int32     -> int32
  val int63     : State.t -> lo:Int63.t   -> hi:Int63.t   -> Int63.t
  val int64     : State.t -> lo:int64     -> hi:int64     -> int64
  val nativeint : State.t -> lo:nativeint -> hi:nativeint -> nativeint
end
