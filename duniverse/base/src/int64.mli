(** 64-bit integers. *)

open! Import
include Int_intf.S with type t = int64

module O : sig
  (*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
    compiling without cross library inlining. *)
  external ( + ) : t -> t -> t = "%int64_add"
  external ( - ) : t -> t -> t = "%int64_sub"
  external ( * ) : t -> t -> t = "%int64_mul"
  external ( / ) : t -> t -> t = "%int64_div"
  external ( ~- ) : t -> t = "%int64_neg"
  val ( ** ) : t -> t -> t
  external ( = ) : t -> t -> bool = "%equal"
  external ( <> ) : t -> t -> bool = "%notequal"
  external ( < ) : t -> t -> bool = "%lessthan"
  external ( > ) : t -> t -> bool = "%greaterthan"
  external ( <= ) : t -> t -> bool = "%lessequal"
  external ( >= ) : t -> t -> bool = "%greaterequal"
  external ( land ) : t -> t -> t = "%int64_and"
  external ( lor ) : t -> t -> t = "%int64_or"
  external ( lxor ) : t -> t -> t = "%int64_xor"
  val lnot : t -> t
  val abs : t -> t
  external neg : t -> t = "%int64_neg"
  val zero : t
  val ( % ) : t -> t -> t
  val ( /% ) : t -> t -> t
  val ( // ) : t -> t -> float
  external ( lsl ) : t -> int -> t = "%int64_lsl"
  external ( asr ) : t -> int -> t = "%int64_asr"
  external ( lsr ) : t -> int -> t = "%int64_lsr"
end

include module type of O

(** {2 Conversion functions} *)

(*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
  compiling without cross library inlining. *)
external of_int : int -> t = "%int64_of_int"
external of_int32 : int32 -> t = "%int64_of_int32"
external of_int64 : t -> t = "%identity"
val to_int : t -> int option
val to_int32 : t -> int32 option
val of_nativeint : nativeint -> t
val to_nativeint : t -> nativeint option

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

(*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
  compiling without cross library inlining. *)
external to_int_trunc : t -> int = "%int64_to_int"
external to_int32_trunc : int64 -> int32 = "%int64_to_int32"
external to_nativeint_trunc : int64 -> nativeint = "%int64_to_nativeint"

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

(*_ Declared as an external so that the compiler skips the caml_apply_X wrapping even when
  compiling without cross library inlining. *)
external bswap64 : t -> t = "%bswap_int64"
