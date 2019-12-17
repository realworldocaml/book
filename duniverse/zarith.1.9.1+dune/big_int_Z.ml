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

type big_int = Z.t

let zero_big_int = Z.zero

let unit_big_int = Z.one

let minus_big_int = Z.neg

let abs_big_int = Z.abs

let add_big_int = Z.add

let succ_big_int = Z.succ

let add_int_big_int x y = Z.add (Z.of_int x) y

let sub_big_int = Z.sub

let pred_big_int = Z.pred

let mult_big_int  = Z.mul

let mult_int_big_int x y = Z.mul (Z.of_int x) y

let square_big_int x = Z.mul x x

let sqrt_big_int = Z.sqrt

let quomod_big_int = Z.ediv_rem

let div_big_int = Z.ediv

let mod_big_int = Z.erem

let gcd_big_int = Z.gcd

let power = Z.pow

let power_big a b = 
  Z.pow a (Z.to_int b)

let power_int_positive_int a b =
  if b < 0 then raise (Invalid_argument "power_int_positive_int");
  power (Z.of_int a) b

let power_big_int_positive_int a b =
  if b < 0 then raise (Invalid_argument "power_big_int_positive_int");
  power a b

let power_int_positive_big_int a b =
  if Z.sign b < 0 then raise (Invalid_argument "power_int_positive_big_int");
  power_big (Z.of_int a) b

let power_big_int_positive_big_int a b =
  if Z.sign b < 0 then raise (Invalid_argument "power_big_int_positive_big_int");
  power_big a b

let sign_big_int = Z.sign

let compare_big_int = Z.compare

let eq_big_int = Z.equal

let le_big_int a b = Z.compare a b <= 0

let ge_big_int a b = Z.compare a b >= 0

let lt_big_int a b = Z.compare a b < 0

let gt_big_int a b = Z.compare a b > 0

let max_big_int = Z.max

let min_big_int = Z.min

let num_digits_big_int = Z.size

let string_of_big_int = Z.to_string

let big_int_of_string = Z.of_string

let big_int_of_int = Z.of_int

let is_int_big_int = Z.fits_int

let int_of_big_int x = 
   try Z.to_int x with Z.Overflow -> failwith "int_of_big_int"

let big_int_of_int32 = Z.of_int32

let big_int_of_nativeint = Z.of_nativeint

let big_int_of_int64 = Z.of_int64

let int32_of_big_int x =
   try Z.to_int32 x with Z.Overflow -> failwith "int32_of_big_int"

let nativeint_of_big_int x =
   try Z.to_nativeint x with Z.Overflow -> failwith "nativeint_of_big_int"

let int64_of_big_int x =
   try Z.to_int64 x with Z.Overflow -> failwith "int64_of_big_int"

let float_of_big_int = Z.to_float

let and_big_int = Z.logand

let or_big_int = Z.logor

let xor_big_int = Z.logxor

let shift_left_big_int = Z.shift_left

let shift_right_big_int = Z.shift_right

let shift_right_towards_zero_big_int = Z.shift_right_trunc

let extract_big_int = Z.extract



