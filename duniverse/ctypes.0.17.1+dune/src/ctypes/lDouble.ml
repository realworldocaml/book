(*
 * Copyright (c) 2016 Andy Ray.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

[@@@warning "-3"]

external init : unit -> unit = "ldouble_init"
let () = init ()

type t

external to_float : t -> float = "ctypes_ldouble_to_float"
external of_float : float -> t = "ctypes_ldouble_of_float"
external to_int : t -> int = "ctypes_ldouble_to_int"
external of_int : int -> t = "ctypes_ldouble_of_int"

external format : int -> int -> t -> string = "ctypes_ldouble_format"
let to_string ?(width=0) ?(prec=6) d = format width prec d
external of_string : string -> t = "ctypes_ldouble_of_string"

(* debug *)
(*external to_hex_string : t -> string = "ctypes_ldouble_to_hex"*)

external add : t -> t -> t = "ctypes_ldouble_add"
external sub : t -> t -> t = "ctypes_ldouble_sub"
external mul : t -> t -> t = "ctypes_ldouble_mul"
external div : t -> t -> t = "ctypes_ldouble_div"
external neg : t -> t = "ctypes_ldouble_neg"
external pow : t -> t -> t = "ctypes_ldouble_powl"
external sqrt : t -> t = "ctypes_ldouble_sqrtl"
external exp : t -> t = "ctypes_ldouble_expl"
external log : t -> t = "ctypes_ldouble_logl"
external log10 : t -> t = "ctypes_ldouble_log10l"
external expm1 : t -> t = "ctypes_ldouble_expm1l"
external log1p : t -> t = "ctypes_ldouble_log1pl"
external cos : t -> t = "ctypes_ldouble_cosl"
external sin : t -> t = "ctypes_ldouble_sinl"
external tan : t -> t = "ctypes_ldouble_tanl"
external acos : t -> t = "ctypes_ldouble_acosl"
external asin : t -> t = "ctypes_ldouble_asinl"
external atan : t -> t = "ctypes_ldouble_atanl"
external atan2 : t -> t -> t = "ctypes_ldouble_atan2l"
external hypot : t -> t -> t = "ctypes_ldouble_hypotl"
external cosh : t -> t = "ctypes_ldouble_coshl"
external sinh : t -> t = "ctypes_ldouble_sinhl"
external tanh : t -> t = "ctypes_ldouble_tanhl"
external acosh : t -> t = "ctypes_ldouble_acoshl"
external asinh : t -> t = "ctypes_ldouble_asinhl"
external atanh : t -> t = "ctypes_ldouble_atanhl"
external ceil : t -> t = "ctypes_ldouble_ceill"
external floor : t -> t = "ctypes_ldouble_floorl"
external abs : t -> t = "ctypes_ldouble_fabsl"
external rem : t -> t -> t = "ctypes_ldouble_remainderl"
external copysign : t -> t -> t = "ctypes_ldouble_copysignl"
external frexp : t -> t * int = "ctypes_ldouble_frexp"
external ldexp : t -> int -> t = "ctypes_ldouble_ldexp"
external modf : t -> t * t = "ctypes_ldouble_modf"
external classify : t -> fpclass = "ctypes_ldouble_classify"

external min_ : unit -> t = "ctypes_ldouble_min"
let min_float = min_ ()
external max_ : unit -> t = "ctypes_ldouble_max"
let max_float = max_ ()
external epsilon_ : unit -> t = "ctypes_ldouble_epsilon"
let epsilon = epsilon_ ()
external nan_ : unit -> t = "ctypes_ldouble_nan"
let nan = nan_ ()
external inf_ : unit -> t = "ctypes_ldouble_inf"
let infinity = inf_ ()
external ninf_ : unit -> t = "ctypes_ldouble_ninf"
let neg_infinity = ninf_ ()

let zero = of_int 0
let one = of_int 1

external size_ : unit -> (int * int) = "ctypes_ldouble_size"
let byte_sizes = size_ ()

external mant_dig_ : unit -> int = "ctypes_ldouble_mant_dig" [@@noalloc]
let mant_dig = mant_dig_ ()
