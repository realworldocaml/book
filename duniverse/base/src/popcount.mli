(** This module exposes popcount functions (which count the number of ones in a bitstring)
    for the various integer types.

    Functions are exposed in their respective modules. *)

open! Import

val int_popcount       : int       -> int
val int32_popcount     : int32     -> int
val int64_popcount     : int64     -> int
val nativeint_popcount : nativeint -> int
