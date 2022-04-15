(**
   Integers.

   This modules provides arbitrary-precision integers.
   Small integers internally use a regular OCaml [int].
   When numbers grow too large, we switch transparently to GMP numbers
   ([mpn] numbers fully allocated on the OCaml heap).

   This interface is rather similar to that of [Int32] and [Int64],
   with some additional functions provided natively by GMP
   (GCD, square root, pop-count, etc.).


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


(** {1 Toplevel} *)

(** For an optimal experience with the [ocaml] interactive toplevel,
    the magic commands are:

    {[
    #load "zarith.cma";;
    #install_printer Z.pp_print;;
    ]}

    Alternatively, using the new [Zarith_top] toplevel module, simply:
    {[
    #require "zarith.top";;
    ]}
*)



(** {1 Types} *)

type t
(** Type of integers of arbitrary length. *)

exception Overflow
(** Raised by conversion functions when the value cannot be represented in
    the destination type.
 *)

(** {1 Construction} *)

val zero: t
(** The number 0. *)

val one: t
(** The number 1. *)

val minus_one: t
(** The number -1. *)

external of_int: int -> t = "%identity"
(** Converts from a base integer. *)

external of_int32: int32 -> t = "ml_z_of_int32"
(** Converts from a 32-bit integer. *)

external of_int64: int64 -> t = "ml_z_of_int64"
(** Converts from a 64-bit integer. *)

external of_nativeint: nativeint -> t = "ml_z_of_nativeint"
(** Converts from a native integer. *)

external of_float: float -> t = "ml_z_of_float"
(** Converts from a floating-point value.
    The value is truncated (rounded towards zero).
    Raises [Overflow] on infinity and NaN arguments.
 *)

val of_string: string -> t
(** Converts a string to an integer.
    An optional [-] prefix indicates a negative number, while a [+]
    prefix is ignored.
    An optional prefix [0x], [0o], or [0b] (following the optional [-]
    or [+] prefix) indicates that the number is,
    represented, in hexadecimal, octal, or binary, respectively.
    Otherwise, base 10 is assumed.
    (Unlike C, a lone [0] prefix does not denote octal.)
    Raises an [Invalid_argument] exception if the string is not a
    syntactically correct representation of an integer.
 *)

val of_substring : string -> pos:int -> len:int -> t
(** [of_substring s ~pos ~len] is the same as [of_string (String.sub s
    pos len)]
 *)

val of_string_base: int -> string -> t
(** Parses a number represented as a string in the specified base,
    with optional [-] or [+] prefix.
    The base must be between 2 and 16.
 *)

external of_substring_base
  : int -> string -> pos:int -> len:int -> t
  = "ml_z_of_substring_base"
(** [of_substring_base base s ~pos ~len] is the same as [of_string_base
    base (String.sub s pos len)]
*)


(** {1 Basic arithmetic operations} *)

val succ: t -> t
(** Returns its argument plus one. *)

val pred: t -> t
(** Returns its argument minus one. *)

val abs: t -> t
(** Absolute value. *)

val neg: t -> t
(** Unary negation. *)

val add: t -> t -> t
(** Addition. *)

val sub: t -> t -> t
(** Subtraction. *)

val mul: t -> t -> t
(** Multiplication. *)

val div: t -> t -> t
(** Integer division. The result is truncated towards zero
    and obeys the rule of signs.
    Raises [Division_by_zero] if the divisor (second argument) is 0.
 *)

val rem: t -> t -> t
(** Integer remainder. Can raise a [Division_by_zero].
    The result of [rem a b] has the sign of [a], and its absolute value is
    strictly smaller than the absolute value of [b].
    The result satisfies the equality [a = b * div a b + rem a b].
 *)

external div_rem: t -> t -> (t * t) = "ml_z_div_rem"
(** Computes both the integer quotient and the remainder.
    [div_rem a b] is equal to [(div a b, rem a b)].
    Raises [Division_by_zero] if [b = 0].
 *)

external cdiv: t -> t -> t = "ml_z_cdiv"
(** Integer division with rounding towards +oo (ceiling).
    Can raise a [Division_by_zero].
 *)

external fdiv: t -> t -> t = "ml_z_fdiv"
(** Integer division with rounding towards -oo (floor).
    Can raise a [Division_by_zero].
 *)

val ediv_rem: t -> t -> (t * t)
(** Euclidean division and remainder.  [ediv_rem a b] returns a pair [(q, r)]
    such that [a = b * q + r] and [0 <= r < |b|].
    Raises [Division_by_zero] if [b = 0].
 *)

val ediv: t -> t -> t
(** Euclidean division. [ediv a b] is equal to [fst (ediv_rem a b)].
    The result satisfies [0 <= a - b * ediv a b < |b|].
    Raises [Division_by_zero] if [b = 0].
 *)

val erem: t -> t -> t
(** Euclidean remainder.  [erem a b] is equal to [snd (ediv_rem a b)].
    The result satisfies [0 <= erem a b < |b|] and
    [a = b * ediv a b + erem a b].  Raises [Division_by_zero] if [b = 0].
 *)

val divexact: t -> t -> t
(** [divexact a b] divides [a] by [b], only producing correct result when the
    division is exact, i.e., when [b] evenly divides [a].
    It should be faster than general division.
    Can raise a [Division_by_zero].
*)

external divisible: t -> t -> bool = "ml_z_divisible"
(** [divisible a b] returns [true] if [a] is exactly divisible by [b].
    Unlike the other division functions, [b = 0] is accepted 
    (only 0 is considered divisible by 0).
*)

external congruent: t -> t -> t -> bool = "ml_z_congruent"
(** [congruent a b c] returns [true] if [a] is congruent to [b] modulo [c].
    Unlike the other division functions, [c = 0] is accepted 
    (only equal numbers are considered equal congruent 0).
*)




(** {1 Bit-level operations} *)

(** For all bit-level operations, negative numbers are considered in 2's
    complement representation, starting with a virtual infinite number of
    1s.
 *)

val logand: t -> t -> t
(** Bitwise logical and. *)
    
val logor: t -> t -> t
(** Bitwise logical or. *)
    
val logxor: t -> t -> t
(** Bitwise logical exclusive or. *)

val lognot: t -> t
(** Bitwise logical negation. 
    The identity [lognot a]=[-a-1] always hold.
 *)

val shift_left: t -> int -> t
(** Shifts to the left. 
    Equivalent to a multiplication by a power of 2.
    The second argument must be nonnegative.
 *)

val shift_right: t -> int -> t
(** Shifts to the right. 
    This is an arithmetic shift, 
    equivalent to a division by a power of 2 with rounding towards -oo.
    The second argument must be nonnegative.
 *)

val shift_right_trunc: t -> int -> t
(** Shifts to the right, rounding towards 0.
    This is equivalent to a division by a power of 2, with truncation.
    The second argument must be nonnegative.
 *)

external numbits: t -> int = "ml_z_numbits" [@@noalloc]
(** Returns the number of significant bits in the given number.
    If [x] is zero, [numbits x] returns 0.  Otherwise,
    [numbits x] returns a positive integer [n] such that
    [2^{n-1} <= |x| < 2^n].  Note that [numbits] is defined
    for negative arguments, and that [numbits (-x) = numbits x]. *)

external trailing_zeros: t -> int = "ml_z_trailing_zeros" [@@noalloc]
(** Returns the number of trailing 0 bits in the given number.
    If [x] is zero, [trailing_zeros x] returns [max_int].
    Otherwise, [trailing_zeros x] returns a nonnegative integer [n]
    which is the largest [n] such that [2^n] divides [x] evenly.
    Note that [trailing_zeros] is defined for negative arguments,
    and that [trailing_zeros (-x) = trailing_zeros x]. *)

val testbit: t -> int -> bool
(** [testbit x n] return the value of bit number [n] in [x]:
    [true] if the bit is 1, [false] if the bit is 0.
    Bits are numbered from 0.  Raise [Invalid_argument] if [n]
    is negative. *)

external popcount: t -> int = "ml_z_popcount"
(** Counts the number of bits set.
    Raises [Overflow] for negative arguments, as those have an infinite
    number of bits set.
 *)

external hamdist: t -> t -> int = "ml_z_hamdist"
(** Counts the number of different bits.
    Raises [Overflow] if the arguments have different signs
    (in which case the distance is infinite).
 *)

(** {1 Conversions} *)

(** Note that, when converting to an integer type that cannot represent the
    converted value, an [Overflow] exception is raised.
 *)

val to_int: t -> int
(** Converts to a base integer. May raise an [Overflow]. *)

external to_int32: t -> int32 = "ml_z_to_int32"
(** Converts to a 32-bit integer. May raise [Overflow]. *)

external to_int64: t -> int64 = "ml_z_to_int64"
(** Converts to a 64-bit integer. May raise [Overflow]. *)

external to_nativeint: t -> nativeint = "ml_z_to_nativeint"
(** Converts to a native integer. May raise [Overflow]. *)

val to_float: t -> float
(** Converts to a floating-point value.
    This function rounds the given integer according to the current
    rounding mode of the processor.  In default mode, it returns
    the floating-point number nearest to the given integer,
    breaking ties by rounding to even. *)

val to_string: t -> string
(** Gives a human-readable, decimal string representation of the argument. *)

external format: string -> t -> string = "ml_z_format"
(** Gives a string representation of the argument in the specified
    printf-like format.
    The general specification has the following form:

    [% \[flags\] \[width\] type]

    Where the type actually indicates the base:

    - [i], [d], [u]: decimal
    - [b]: binary
    - [o]: octal
    - [x]: lowercase hexadecimal
    - [X]: uppercase hexadecimal

    Supported flags are:

    - [+]: prefix positive numbers with a [+] sign
    - space: prefix positive numbers with a space
    - [-]: left-justify (default is right justification)
    - [0]: pad with zeroes (instead of spaces)
    - [#]: alternate formatting (actually, simply output a literal-like prefix: [0x], [0b], [0o])

    Unlike the classic [printf], all numbers are signed (even hexadecimal ones),
    there is no precision field, and characters that are not part of the format
    are simply ignored (and not copied in the output).
 *)

external fits_int: t -> bool = "ml_z_fits_int" [@@noalloc]
(** Whether the argument fits in a regular [int]. *)

external fits_int32: t -> bool = "ml_z_fits_int32" [@@noalloc]
(** Whether the argument fits in an [int32]. *)

external fits_int64: t -> bool = "ml_z_fits_int64" [@@noalloc]
(** Whether the argument fits in an [int64]. *)

external fits_nativeint: t -> bool = "ml_z_fits_nativeint" [@@noalloc]
(** Whether the argument fits in a [nativeint]. *)


(** {1 Printing} *)

val print: t -> unit
(** Prints the argument on the standard output. *)

val output: out_channel -> t -> unit
(** Prints the argument on the specified channel.
    Also intended to be used as [%a] format printer in [Printf.printf].
 *)

val sprint: unit -> t -> string
(** To be used as [%a] format printer in [Printf.sprintf]. *)

val bprint: Buffer.t -> t -> unit
(** To be used as [%a] format printer in [Printf.bprintf]. *)

val pp_print: Format.formatter -> t -> unit
(** Prints the argument on the specified formatter.
    Can be used as [%a] format printer in [Format.printf] and as
    argument to [#install_printer] in the top-level.
 *)


(** {1 Ordering} *)

external compare: t -> t -> int = "ml_z_compare" [@@noalloc]
(** Comparison.  [compare x y] returns 0 if [x] equals [y],
    -1 if [x] is smaller than [y], and 1 if [x] is greater than [y].

    Note that Pervasive.compare can be used to compare reliably two integers
    only on OCaml 3.12.1 and later versions.
 *)

external equal: t -> t -> bool = "ml_z_equal" [@@noalloc]
(** Equality test. *)

val leq: t -> t -> bool
(** Less than or equal. *)

val geq: t -> t -> bool
(** Greater than or equal. *)

val lt: t -> t -> bool
(** Less than (and not equal). *)

val gt: t -> t -> bool
(** Greater than (and not equal). *)

external sign: t -> int = "ml_z_sign" [@@noalloc]
(** Returns -1, 0, or 1 when the argument is respectively negative, null, or
    positive.
 *)

val min: t -> t -> t
(** Returns the minimum of its arguments. *)

val max: t -> t -> t
(** Returns the maximum of its arguments. *)

val is_even: t -> bool
(** Returns true if the argument is even (divisible by 2), false if odd. *)

val is_odd: t -> bool
(** Returns true if the argument is odd, false if even. *)

external hash: t -> int = "ml_z_hash" [@@noalloc]
(** Hashes a number, producing a small integer.
    The result is consistent with equality: if [a] = [b], then [hash a] =
    [hash b].
    OCaml's generic hash function, [Hashtbl.hash], works correctly with
    numbers, but {!Z.hash} is slightly faster.
*)

(** {1 Elementary number theory} *)

external gcd: t -> t -> t = "ml_z_gcd"
(** Greatest common divisor.
    The result is always nonnegative.
    We have [gcd(a,0) = gcd(0,a) = abs(a)], including [gcd(0,0) = 0].
*)

val gcdext: t -> t -> (t * t * t)
(** [gcdext u v] returns [(g,s,t)] where [g] is the greatest common divisor
    and [g=us+vt].
    [g] is always nonnegative.

    Note: the function is based on the GMP [mpn_gcdext] function. The exact choice of [s] and [t] such that [g=us+vt] is not specified, as it may vary from a version of GMP to another (it has changed notably in GMP 4.3.0 and 4.3.1).
 *)

val lcm: t -> t -> t
(**
    Least common multiple.
    The result is always nonnegative.
    We have [lcm(a,0) = lcm(0,a) = 0].
 *)

external powm: t -> t -> t -> t = "ml_z_powm"
(** [powm base exp mod] computes [base]^[exp] modulo [mod].
    Negative [exp] are supported, in which case ([base]^-1)^(-[exp]) modulo
    [mod] is computed.
    However, if [exp] is negative but [base] has no inverse modulo [mod], then
    a [Division_by_zero] is raised.
 *)

external powm_sec: t -> t -> t -> t = "ml_z_powm_sec"
(** [powm_sec base exp mod] computes [base]^[exp] modulo [mod].
    Unlike [Z.powm], this function is designed to take the same time
    and have the same cache access patterns for any two same-size
    arguments.  Used in cryptographic applications, it provides better
    resistance to side-channel attacks than [Z.powm].
    The exponent [exp] must be positive, and the modulus [mod]
    must be odd.  Otherwise, [Invalid_arg] is raised. *)

external invert: t -> t -> t = "ml_z_invert"
(** [invert base mod] returns the inverse of [base] modulo [mod].
    Raises a [Division_by_zero] if [base] is not invertible modulo [mod].
 *)

external probab_prime: t -> int -> int = "ml_z_probab_prime"
(** [probab_prime x r] returns 0 if [x] is definitely composite,
    1 if [x] is probably prime, and 2 if [x] is definitely prime.
    The [r] argument controls how many Miller-Rabin probabilistic
    primality tests are performed (5 to 10 is a reasonable value).
 *)

external nextprime: t -> t = "ml_z_nextprime"
(** Returns the next prime greater than the argument.
    The result is only prime with very high probability.
 *)

external jacobi: t -> t -> int = "ml_z_jacobi"
(** [jacobi a b] returns the Jacobi symbol [(a/b)]. *)

external legendre: t -> t -> int = "ml_z_legendre"
(** [legendre a b] returns the Legendre symbol [(a/b)]. *)

external kronecker: t -> t -> int = "ml_z_kronecker"
(** [kronecker a b] returns the Kronecker symbol [(a/b)]. *)

external remove: t -> t -> t * int = "ml_z_remove"
(** [remove a b] returns [a] after removing all the occurences of the
    factor [b]. 
    Also returns how many occurrences were removed.
 *)

external fac: int -> t = "ml_z_fac"
(** [fac n] returns the factorial of [n] ([n!]).
    Raises an [Invaid_argument] if [n] is non-positive.
*)
  
external fac2: int -> t = "ml_z_fac2"
(** [fac2 n] returns the double factorial of [n] ([n!!]).
    Raises an [Invaid_argument] if [n] is non-positive.
*)
  
 external facM: int -> int -> t = "ml_z_facM"
(** [facM n m] returns the [m]-th factorial of [n].
    Raises an [Invaid_argument] if [n] or [m] is non-positive.
*)

external primorial: int -> t = "ml_z_primorial"
(** [primorial n] returns the product of all positive prime numbers less
    than or equal to [n].
    Raises an [Invaid_argument] if [n] is non-positive.
*)

external bin: t -> int -> t = "ml_z_bin"
(** [bin n k] returns the binomial coefficient [n] over [k].
    Raises an [Invaid_argument] if [k] is non-positive.
*)

external fib: int -> t = "ml_z_fib"
(** [fib n] returns the [n]-th Fibonacci number.
    Raises an [Invaid_argument] if [n] is non-positive.
*)

external lucnum: int -> t = "ml_z_lucnum"
(** [lucnum n] returns the [n]-th Lucas number.
    Raises an [Invaid_argument] if [n] is non-positive.
*)


(** {1 Powers} *)

external pow: t -> int -> t = "ml_z_pow"
(** [pow base exp] raises [base] to the [exp] power.
    [exp] must be nonnegative.
    Note that only exponents fitting in a machine integer are supported, as
    larger exponents would surely make the result's size overflow the
    address space.
 *)

external sqrt: t -> t = "ml_z_sqrt"
(** Returns the square root. The result is truncated (rounded down
    to an integer).
    Raises an [Invalid_argument] on negative arguments.
 *)

external sqrt_rem: t -> (t * t) = "ml_z_sqrt_rem"
(** Returns the square root truncated, and the remainder.
    Raises an [Invalid_argument] on negative arguments.
 *)

external root: t -> int -> t = "ml_z_root"
(** [root x n] computes the [n]-th root of [x].
    [n] must be positive and, if [n] is even, then [x] must be nonnegative.
    Otherwise, an [Invalid_argument] is raised.
 *)

external rootrem: t -> int -> t * t = "ml_z_rootrem"
(** [rootrem x n] computes the [n]-th root of [x] and the remainder
    [x-root**n].
    [n] must be positive and, if [n] is even, then [x] must be nonnegative.
    Otherwise, an [Invalid_argument] is raised.
 *)

external perfect_power: t -> bool = "ml_z_perfect_power"
(** True if the argument has the form [a^b], with [b>1] *)

external perfect_square: t -> bool = "ml_z_perfect_square"
(** True if the argument has the form [a^2]. *)

val log2: t -> int
(** Returns the base-2 logarithm of its argument, rounded down to
    an integer.  If [x] is positive, [log2 x] returns the largest [n]
    such that [2^n <= x].  If [x] is negative or zero, [log2 x] raise
    the [Invalid_argument] exception. *)

val log2up: t -> int
(** Returns the base-2 logarithm of its argument, rounded up to
    an integer.  If [x] is positive, [log2up x] returns the smallest [n]
    such that [x <= 2^n].  If [x] is negative or zero, [log2up x] raise
    the [Invalid_argument] exception. *)

(** {1 Representation} *)

external size: t -> int = "ml_z_size" [@@noalloc]
(** Returns the number of machine words used to represent the number. *)

external extract: t -> int -> int -> t = "ml_z_extract"
(** [extract a off len] returns a nonnegative number corresponding to bits
    [off] to [off]+[len]-1 of [b].
    Negative [a] are considered in infinite-length 2's complement
    representation.
 *)

val signed_extract: t -> int -> int -> t
(** [signed_extract a off len] extracts bits [off] to [off]+[len]-1 of [b],
    as [extract] does, then sign-extends bit [len-1] of the result
    (that is, bit [off + len - 1] of [a]).  The result is between
    [- 2{^[len]-1}] (included) and [2{^[len]-1}] (excluded),
    and equal to [extract a off len] modulo [2{^len}].
 *)

external to_bits: t -> string = "ml_z_to_bits"
(** Returns a binary representation of the argument.
    The string result should be interpreted as a sequence of bytes,
    corresponding to the binary representation of the absolute value of
    the argument in little endian ordering.
    The sign is not stored in the string.
 *)

external of_bits: string -> t = "ml_z_of_bits"
(** Constructs a number from a binary string representation.
    The string is interpreted as a sequence of bytes in little endian order,
    and the result is always positive.
    We have the identity: [of_bits (to_bits x) = abs x].
    However, we can have [to_bits (of_bits s) <> s] due to the presence of
    trailing zeros in s.
 *)


(** {1 Prefix and infix operators} *)

(**
   Classic (and less classic) prefix and infix [int] operators are
   redefined on [t].

   This makes it easy to typeset expressions.
   Using OCaml 3.12's local open, you can simply write
   [Z.(~$2 + ~$5 * ~$10)].
 *)

val (~-): t -> t
(** Negation [neg]. *)

val (~+): t -> t
(** Identity. *)

val (+): t -> t -> t
(** Addition [add]. *)

val (-): t -> t -> t
(** Subtraction [sub]. *)

val ( * ): t -> t -> t
(** Multiplication [mul]. *)

val (/): t -> t -> t
(** Truncated division [div]. *)

external (/>): t -> t -> t = "ml_z_cdiv"
(** Ceiling division [cdiv]. *)

external (/<): t -> t -> t = "ml_z_fdiv"
(** Flooring division [fdiv]. *)

val (/|): t -> t -> t
(** Exact division [divexact]. *)

val (mod): t -> t -> t
(** Remainder [rem]. *)

val (land): t -> t -> t
(** Bit-wise logical and [logand]. *)

val (lor): t -> t -> t
(** Bit-wise logical inclusive or [logor]. *)

val (lxor): t -> t -> t
(** Bit-wise logical exclusive or [logxor]. *)

val (~!): t -> t
(** Bit-wise logical negation [lognot]. *)

val (lsl): t -> int -> t
(** Bit-wise shift to the left [shift_left]. *)

val (asr): t -> int -> t
(** Bit-wise shift to the right [shift_right]. *)

external (~$): int -> t = "%identity"

(** Conversion from [int] [of_int]. *)

external ( ** ): t -> int -> t = "ml_z_pow"
(** Power [pow]. *)

module Compare : sig

    val (=): t -> t -> bool
    (** Same as [equal]. *)

    val (<): t -> t -> bool
    (** Same as [lt]. *)

    val (>): t -> t -> bool
    (** Same as [gt]. *)

    val (<=): t -> t -> bool
    (** Same as [leq]. *)

    val (>=): t -> t -> bool
    (** Same as [geq]. *)

    val (<>): t -> t -> bool
    (** [a <> b] is equivalent to [not (equal a b)]. *)

end

(** {1 Miscellaneous} *)

val version: string
(** Library version. *)

(**/**)

(** For internal use in module [Q]. *)
val round_to_float: t -> bool -> float
