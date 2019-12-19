(** Pseudo-random number generation.

    This is a wrapper of the standard library's [Random] library, though it does not share
    state with that library.
*)

(*_
  (***********************************************************************)
  (*                                                                     *)
  (*                           Objective Caml                            *)
  (*                                                                     *)
  (*              Damien Doligez, projet Para, INRIA Rocquencourt        *)
  (*                                                                     *)
  (*  Copyright 1996 Institut National de Recherche en Informatique et   *)
  (*  en Automatique.  All rights reserved.  This file is distributed    *)
  (*  under the terms of the Apache 2.0 license. See ../THIRD-PARTY.txt  *)
  (*  for details.                                                       *)
  (*                                                                     *)
  (***********************************************************************) *)

open! Import

(** {6 Basic functions} *)

(** Note that all of these "basic" functions mutate a global random state. *)

(** Initialize the generator, using the argument as a seed.  The same seed will always
    yield the same sequence of numbers. *)
val init : int -> unit

(** Same as {!Random.init} but takes more data as seed. *)
val full_init : int array -> unit

(** Initialize the generator with a more-or-less random seed chosen in a system-dependent
    way.  By default, [self_init] is disallowed in inline tests, as it's often used for no
    good reason and it just creates nondeterministic failures for everyone.  Passing
    [~allow_in_tests:true] removes this restriction in case you legitimately want
    nondeterministic values, like in [Filename.temp_dir]. *)
val self_init : ?allow_in_tests:bool -> unit -> unit

(** Return 30 random bits in a nonnegative integer.  @before 3.12.0 used a different
    algorithm (affects all the following functions) *)
val bits : unit -> int

(** [Random.int bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val int : int -> int

(** [Random.int32 bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val int32 : int32 -> int32

(** [Random.nativeint bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val nativeint : nativeint -> nativeint

(** [Random.int64 bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val int64 : int64 -> int64

(** [Random.float bound] returns a random floating-point number between 0 (inclusive) and
    [bound] (exclusive).  If [bound] is negative, the result is negative or zero.  If
    [bound] is 0, the result is 0. *)
val float : float -> float

(** Produces a random value between the given inclusive bounds.  Raises if bounds are
    given in decreasing order. *)
val int_incl : int -> int -> int

val int32_incl : int32 -> int32 -> int32
val nativeint_incl : nativeint -> nativeint -> nativeint
val int64_incl : int64 -> int64 -> int64

(** Produces a value between the given bounds (inclusive and exclusive, respectively).
    Raises if bounds are given in decreasing order. *)
val float_range : float -> float -> float

(** [Random.bool ()] returns [true] or [false] with probability 0.5 each. *)
val bool : unit -> bool

(** Return a uniformly-chosen {!char}. *)
val char : unit -> char

(** Return a uniformly-chosen {!char} in the ASCII range. *)
val ascii : unit -> char

(** {6 Advanced functions} *)

(** The functions from module [State] manipulate the current state of the random generator
    explicitly.  This allows using one or several deterministic PRNGs, even in a
    multi-threaded program, without interference from other parts of the program.

    Note that [Random.get_state] from the standard library is not exposed, because it
    misleadingly makes a copy of random state, which is not typically the desired outcome
    for accessing the shared state.

    Obtaining multiple generators with good independence properties is nontrivial; see
    the [Splittable_random] library for that. *)
module State : sig
  type t

  (** This gives access to the default random state, allowing user code to share (and
      thereby mutate) the random state used by the main functions in [Random]. *)
  val default : t

  (** Creates a new state and initializes it with the given seed. *)
  val make : int array -> t

  (** Creates a new state and initializes it with a system-dependent low-entropy seed. *)
  val make_self_init : ?allow_in_tests:bool -> unit -> t

  val copy : t -> t

  (** These functions are the same as the basic functions, except that they use (and
      update) the given PRNG state instead of the default one.  *)

  val bits : t -> int
  val int : t -> int -> int
  val int32 : t -> int32 -> int32
  val nativeint : t -> nativeint -> nativeint
  val int64 : t -> int64 -> int64
  val float : t -> float -> float
  val int_incl : t -> int -> int -> int
  val int32_incl : t -> int32 -> int32 -> int32
  val nativeint_incl : t -> nativeint -> nativeint -> nativeint
  val int64_incl : t -> int64 -> int64 -> int64
  val float_range : t -> float -> float -> float
  val bool : t -> bool
  val char : t -> char
  val ascii : t -> char
end

(** Sets the state of the generator used by the basic functions. *)
val set_state : State.t -> unit
