(*
 * Copyright (c) 2016 Andy Ray.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type t
(** The type of long doubles. *)

val to_float : t -> float 
(** Convert a long double to a float.  The result is unspecified if the argument is
    either too large or too small to be represented as a [float].*)

val of_float : float -> t 
(** Create a long double from a float *)

val to_int : t -> int 
(** Convert a long double to an int.  The result is unspecified if the argument is NAN
    or falls outside the range of representable integers. *)

val of_int : int -> t 
(** Create a long double from an int *)

val to_string : ?width:int -> ?prec: int -> t -> string
(** Convert a long double to a string.
 
 [width] specifies the minimum number of digits to format the string
 with.  A negative value left aligns.  The default is 0.

 [prec] specifies the number of digits after the decimal point.  
 The default is 6. *)
 
val of_string : string -> t
(** Create a long double from a string *)

val add : t -> t -> t 
(** Addition *)

val sub : t -> t -> t 
(** Subtraction *)

val mul : t -> t -> t 
(** Multiplication *)

val div : t -> t -> t 
(** Division *)

val neg : t -> t
(** Negation *)

val pow : t -> t -> t 
(** Exponentiation *)

val sqrt : t -> t 
(** Square root *)

val exp : t -> t 
(** Exponential *)

val log : t -> t 
(** Natural logarithm *)

val log10 : t -> t 
(** Base 10 logarithm *)

val expm1 : t -> t 
(** [expm1 x] computes [exp x -. 1.0], giving numerically-accurate results
    even if [x] is close to [0.0].
*)

val log1p : t -> t 
(** [log1p x] computes [log(1.0 +. x)] (natural logarithm),
    giving numerically-accurate results even if [x] is close to [0.0].
*)

val cos : t -> t 
(** Cosine.  Argument is in radians. *)

val sin : t -> t 
(** Sine.  Argument is in radians. *)

val tan : t -> t 
(** Tangent.  Argument is in radians. *)

val acos : t -> t 
(** Arc cosine *)

val asin : t -> t 
(** Arc sine *)

val atan : t -> t 
(** Arc tangent *)

val atan2 : t -> t -> t 
(** [atan2 y x] returns the arc tangent of [y /. x]. *)

val hypot : t -> t -> t 

val cosh : t -> t 
(** Hyperbolic cosine *)

val sinh : t -> t 
(** Hyperbolic sine *)

val tanh : t -> t 
(** Hyperbolic tangent *)

val acosh : t -> t 
(** Inverse hyperbolic cosine *)

val asinh : t -> t 
(** Inverse hyperbolic sine *)

val atanh : t -> t 
(** Inverse hyperbolic tangent *)

val ceil : t -> t 
(** Round above to an integer value. *)

val floor : t -> t 
(** Round below to an integer value. *)

val abs : t -> t 
(** [abs f] returns absolute value of [f] *)

val rem : t -> t -> t
(** [rem x y] is the remainder of dividing x by y *)

val copysign : t -> t -> t  
(** [copysign x y] returns a float whose absolute value is that of [x]
  and whose sign is that of [y]. *)

val frexp : t -> t * int
(** [frexp f] returns the pair of the significant and the exponent of [f]. *)

val ldexp : t -> int -> t
(** [ldexp x n] returns [x *. 2 ** n]. *)

val modf : t -> t * t
(** return [(fractional,integer)] parts of number.
 
    Known fatal bug on mingw32; see https://sourceforge.net/p/mingw-w64/bugs/478 *)

val classify : t -> fpclass
(** Return the class of the given floating-point number:
   normal, subnormal, zero, infinite, or not a number. *)

val min_float : t
(** The smallest positive, non-zero, non-denormalized value *)

val max_float : t
(** The largest positive finite value *)

val epsilon : t
(** The difference between [1.0] and the smallest exactly representable
    floating-point number greater than [1.0]. *)

val nan : t
(** A special floating-point value denoting the result of an
   undefined operation such as [0.0 /. 0.0].  Stands for
   'not a number'. *)

val infinity : t
(** Positive infinity *)

val neg_infinity : t
(** Negative infinity *)

val zero : t
(** 0.0 *)

val one : t
(** 1.0 *)

val byte_sizes : int * int
(** size, in bytes, used for storing long doubles, 
    and the actual number of bytes used by the value.
    (unused bytes may contain undefined values) *)

val mant_dig : int
(** size of mantissa *)
