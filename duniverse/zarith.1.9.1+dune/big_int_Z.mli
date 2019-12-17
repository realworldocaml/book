(**
   [Big_int] interface for Z module.
   
   This modules provides an interface compatible with [Big_int], but using
   [Z] functions internally.


   This file is part of the Zarith library 
   http://forge.ocamlcore.org/projects/zarith .
   It is distributed under LGPL 2 licensing, with static linking exception.
   See the LICENSE file included in the distribution.
   
   Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
   Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
   a joint laboratory by:
   CNRS (Centre national de la recherche scientifique, France),
   ENS (École normale supérieure, Paris, France),
   INRIA Rocquencourt (Institut national de recherche en informatique, France).

 *)

(* note: generated with ocamlc -i *)

type big_int = Z.t

val zero_big_int : Z.t
val unit_big_int : Z.t
val minus_big_int : Z.t -> Z.t
val abs_big_int : Z.t -> Z.t
val add_big_int : Z.t -> Z.t -> Z.t
val succ_big_int : Z.t -> Z.t
val add_int_big_int : int -> Z.t -> Z.t
val sub_big_int : Z.t -> Z.t -> Z.t
val pred_big_int : Z.t -> Z.t
val mult_big_int : Z.t -> Z.t -> Z.t
val mult_int_big_int : int -> Z.t -> Z.t
val square_big_int : Z.t -> Z.t
val sqrt_big_int : Z.t -> Z.t
val quomod_big_int : Z.t -> Z.t -> Z.t * Z.t
val div_big_int : Z.t -> Z.t -> Z.t
val mod_big_int : Z.t -> Z.t -> Z.t
val gcd_big_int : Z.t -> Z.t -> Z.t
val power : Z.t -> int -> Z.t
val power_big : Z.t -> Z.t -> Z.t
val power_int_positive_int : int -> int -> Z.t
val power_big_int_positive_int : Z.t -> int -> Z.t
val power_int_positive_big_int : int -> Z.t -> Z.t
val power_big_int_positive_big_int : Z.t -> Z.t -> Z.t
val sign_big_int : Z.t -> int
val compare_big_int : Z.t -> Z.t -> int
val eq_big_int : Z.t -> Z.t -> bool
val le_big_int : Z.t -> Z.t -> bool
val ge_big_int : Z.t -> Z.t -> bool
val lt_big_int : Z.t -> Z.t -> bool
val gt_big_int : Z.t -> Z.t -> bool
val max_big_int : Z.t -> Z.t -> Z.t
val min_big_int : Z.t -> Z.t -> Z.t
val num_digits_big_int : Z.t -> int
val string_of_big_int : Z.t -> string
val big_int_of_string : string -> Z.t
val big_int_of_int : int -> Z.t
val is_int_big_int : Z.t -> bool
val int_of_big_int : Z.t -> int
val big_int_of_int32 : int32 -> Z.t
val big_int_of_nativeint : nativeint -> Z.t
val big_int_of_int64 : int64 -> Z.t
val int32_of_big_int : Z.t -> int32
val nativeint_of_big_int : Z.t -> nativeint
val int64_of_big_int : Z.t -> int64
val float_of_big_int : Z.t -> float
val and_big_int : Z.t -> Z.t -> Z.t
val or_big_int : Z.t -> Z.t -> Z.t
val xor_big_int : Z.t -> Z.t -> Z.t
val shift_left_big_int : Z.t -> int -> Z.t
val shift_right_big_int : Z.t -> int -> Z.t
val shift_right_towards_zero_big_int : Z.t -> int -> Z.t
val extract_big_int : Z.t -> int -> int -> Z.t
