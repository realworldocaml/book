(** An int of exactly 32 bits, regardless of the machine.

    Side note: There's not much reason to want an int of at least 32 bits (i.e., 32 on
    32-bit machines and 63 on 64-bit machines) because [Int63] is basically just as
    efficient.

    Overflow issues are {i not} generally considered and explicitly handled.  This may be
    more of an issue for 32-bit ints than 64-bit ints.

    [Int32.t] is boxed on both 32-bit and 64-bit machines. *)

open! Import

include Int_intf.S with type t = int32

(** {2 Conversion functions} *)

val of_int : int -> t option
val to_int : t -> int option

val of_int32 : int32 -> t
val to_int32 : t -> int32

val of_nativeint : nativeint -> t option
val to_nativeint : t -> nativeint

val of_int64 : int64 -> t option

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

val of_int_trunc : int -> t
val to_int_trunc : t -> int
val of_nativeint_trunc : nativeint -> t
val of_int64_trunc : int64 -> t

(** {3 Low-level float conversions} *)

(** Rounds a regular 64-bit OCaml float to a 32-bit IEEE-754 "single" float, and returns
    its bit representation.  We make no promises about the exact rounding behavior, or
    what happens in case of over- or underflow. *)
val bits_of_float : float -> t

(** Creates a 32-bit IEEE-754 "single" float from the given bits, and converts it to a
    regular 64-bit OCaml float. *)
val float_of_bits : t -> float
