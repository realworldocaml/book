(** 64-bit integers. *)

open! Import
include Int_intf.S with type t = int64

(** {2 Conversion functions} *)

val of_int : int -> t
val to_int : t -> int option
val of_int32 : int32 -> t
val to_int32 : t -> int32 option
val of_nativeint : nativeint -> t
val to_nativeint : t -> nativeint option
val of_int64 : t -> t

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

val to_int_trunc : t -> int
val to_int32_trunc : t -> int32
val to_nativeint_trunc : t -> nativeint

(** {3 Low-level float conversions} *)

val bits_of_float : float -> t
val float_of_bits : t -> float

(** {2 Byte swap operations}

    See {{!modtype:Int.Int_without_module_types}[Int]'s byte swap section} for
    a description of Base's approach to exposing byte swap primitives.

    As of writing, these operations do not sign extend unnecessarily on 64 bit machines,
    unlike their int32 counterparts, and hence, are more performant. See the {!Int32}
    module for more details of the overhead entailed by the int32 byteswap functions.
*)

val bswap16 : t -> t
val bswap32 : t -> t
val bswap48 : t -> t
val bswap64 : t -> t
