(** 63-bit integers.

    The size of Int63 is always 63 bits.  On a 64-bit platform it is just an int
    (63-bits), and on a 32-bit platform it is an int64 wrapped to respect the
    semantics of 63-bit integers.

    Because [Int63] has different representations on 32-bit and 64-bit platforms,
    marshalling [Int63] will not work between 32-bit and 64-bit platforms -- [unmarshal]
    will segfault. *)

open! Import


(** The [@@immediate64] attribute is to indicate that [t] is implemented by a type that is
    immediate only on 64 bit platforms.  It is currently ignored by the compiler, however
    we are hoping that one day it will be taken into account so that the compiler can omit
    [caml_modify] when dealing with mutable data structures holding [Int63.t] values. *)
type t [@@immediate64]

include Int_intf.S with type t := t

(** {2 Arithmetic with overflow}

    Unlike the usual operations, these never overflow, preferring instead to raise. *)

module Overflow_exn : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val abs : t -> t
  val neg : t -> t
end

(** {2 Conversion functions} *)

val of_int : int -> t
val to_int : t -> int option
val of_int32 : Int32.t -> t
val to_int32 : t -> Int32.t option
val of_int64 : Int64.t -> t option
val of_nativeint : nativeint -> t option
val to_nativeint : t -> nativeint option

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

val to_int_trunc : t -> int
val to_int32_trunc : t -> Int32.t
val of_int64_trunc : Int64.t -> t
val of_nativeint_trunc : nativeint -> t
val to_nativeint_trunc : t -> nativeint

(** {2 Byteswap functions}

    See {{!modtype:Int.Int_without_module_types}[Int]'s byte swap section} for
    a description of Base's approach to exposing byte swap primitives.
*)

val bswap16 : t -> t
val bswap32 : t -> t
val bswap48 : t -> t

(** {2 Random generation} *)

(** [random ~state bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive). [bound] must be greater than 0.

    The default [~state] is [Random.State.default]. *)
val random : ?state:Random.State.t -> t -> t

(** [random_incl ~state lo hi] returns a random integer between [lo] (inclusive) and [hi]
    (inclusive).  Raises if [lo > hi].

    The default [~state] is [Random.State.default]. *)
val random_incl : ?state:Random.State.t -> t -> t -> t

(** [floor_log2 x] returns the floor of log-base-2 of [x], and raises if [x <= 0]. *)
val floor_log2 : t -> int

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  (** [val repr] states how [Int63.t] is represented, i.e., as an [int] or an [int64], and
      can be used for building [Int63] operations that behave differently depending on the
      representation (e.g., see core_int63.ml). *)
  module Repr : sig
    type ('underlying_type, 'intermediate_type) t =
      | Int : (int, int) t
      | Int64 : (int64, Int63_emul.t) t
  end

  val repr : (t, t) Repr.t

  module Emul = Int63_emul
end
