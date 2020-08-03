(** Floating-point representation and utilities.

    If using 32-bit OCaml, you cannot quite assume operations act as you'd expect for IEEE
    64-bit floats.  E.g., one can have [let x = ~-. (2. ** 62.) in x = x -. 1.] evaluate
    to [false] while [let x = ~-. (2. ** 62.) in let y = x -. 1 in x = y] evaluates to
    [true].  This is related to 80-bit registers being used for calculations; you can
    force representation as a 64-bit value by let-binding. *)

open! Import

type t = float [@@deriving_inline sexp_grammar]

val t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t

[@@@end]

include Floatable.S with type t := t

(** [max] and [min] will return nan if either argument is nan.

    The [validate_*] functions always fail if class is [Nan] or [Infinite]. *)
include
  Identifiable.S with type t := t

include Comparable.With_zero with type t := t
include Invariant.S with type t := t

(** [validate_ordinary] fails if class is [Nan] or [Infinite]. *)
val validate_ordinary : t Validate.check

val nan : t
val infinity : t
val neg_infinity : t

(** Equal to [infinity]. *)
val max_value : t

(** Equal to [neg_infinity]. *)
val min_value : t

val zero : t
val one : t
val minus_one : t

(** The constant pi. *)
val pi : t

(** The constant sqrt(pi). *)
val sqrt_pi : t

(** The constant sqrt(2 * pi). *)
val sqrt_2pi : t

(** Euler-Mascheroni constant (γ). *)
val euler : t

(** The difference between 1.0 and the smallest exactly representable floating-point
    number greater than 1.0.  That is:

    [epsilon_float = (one_ulp `Up 1.0) -. 1.0]

    This gives the relative accuracy of type [t], in the sense that for numbers on the
    order of [x], the roundoff error is on the order of [x *. float_epsilon].

    See also: {{:http://en.wikipedia.org/wiki/Machine_epsilon} Machine epsilon}.
*)
val epsilon_float : t

val max_finite_value : t

(**
   - [min_positive_subnormal_value = 2 ** -1074]
   - [min_positive_normal_value    = 2 ** -1022] *)

val min_positive_subnormal_value : t
val min_positive_normal_value : t

(** An order-preserving bijection between all floats except for nans, and all int64s with
    absolute value smaller than or equal to [2**63 - 2**52].  Note both 0. and -0. map to
    0L. *)
val to_int64_preserve_order : t -> int64 option

val to_int64_preserve_order_exn : t -> int64

(** Returns [nan] if the absolute value of the argument is too large. *)
val of_int64_preserve_order : int64 -> t

(** The next or previous representable float.  ULP stands for "unit of least precision",
    and is the spacing between floating point numbers.  Both [one_ulp `Up infinity] and
    [one_ulp `Down neg_infinity] return a nan. *)
val one_ulp : [ `Up | `Down ] -> t -> t

(** Note that this doesn't round trip in either direction.  For example, [Float.to_int
    (Float.of_int max_int) <> max_int]. *)
val of_int : int -> t

val to_int : t -> int
val of_int63 : Int63.t -> t
val of_int64 : int64 -> t
val to_int64 : t -> int64

(** [round] rounds a float to an integer float.  [iround{,_exn}] rounds a float to an
    int.  Both round according to a direction [dir], with default [dir] being [`Nearest].

    {v
      | `Down    | rounds toward Float.neg_infinity                             |
      | `Up      | rounds toward Float.infinity                                 |
      | `Nearest | rounds to the nearest int ("round half-integers up")         |
      | `Zero    | rounds toward zero                                           |
    v}

    [iround_exn] raises when trying to handle nan or trying to handle a float outside the
    range \[float min_int, float max_int).


    Here are some examples for [round] for each direction:

    {v
      | `Down    | [-2.,-1.)   to -2. | [-1.,0.)   to -1. | [0.,1.) to 0., [1.,2.) to 1. |
      | `Up      | (-2.,-1.]   to -1. | (-1.,0.]   to -0. | (0.,1.] to 1., (1.,2.] to 2. |
      | `Zero    | (-2.,-1.]   to -1. | (-1.,1.)   to 0.  | [1.,2.) to 1.                |
      | `Nearest | [-1.5,-0.5) to -1. | [-0.5,0.5) to 0.  | [0.5,1.5) to 1.              |
    v}

    For convenience, versions of these functions with the [dir] argument hard-coded are
    provided.  If you are writing performance-critical code you should use the
    versions with the hard-coded arguments (e.g. [iround_down_exn]).  The [_exn] ones
    are the fastest.

    The following properties hold:

    - [of_int (iround_*_exn i) = i] for any float [i] that is an integer with
      [min_int <= i <= max_int].

    - [round_* i = i] for any float [i] that is an integer.

    - [iround_*_exn (of_int i) = i] for any int [i] with [-2**52 <= i <= 2**52]. *)
val round : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> t -> t

val iround : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> t -> int option
val iround_exn : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> t -> int
val round_towards_zero : t -> t
val round_down : t -> t
val round_up : t -> t

(** Rounds half integers up. *)
val round_nearest : t -> t

(** Rounds half integers to the even integer. *)
val round_nearest_half_to_even : t -> t

val iround_towards_zero : t -> int option
val iround_down : t -> int option
val iround_up : t -> int option
val iround_nearest : t -> int option
val iround_towards_zero_exn : t -> int
val iround_down_exn : t -> int
val iround_up_exn : t -> int
val iround_nearest_exn : t -> int
val int63_round_down_exn : t -> Int63.t
val int63_round_up_exn : t -> Int63.t
val int63_round_nearest_exn : t -> Int63.t

(** If [f <= iround_lbound || f >= iround_ubound], then [iround*] functions will refuse
    to round [f], returning [None] or raising as appropriate. *)
val iround_lbound : t

val iround_ubound : t

(** [round_significant x ~significant_digits:n] rounds to the nearest number with [n]
    significant digits.  More precisely: it returns the representable float closest to [x
    rounded to n significant digits].  It is meant to be equivalent to [sprintf "%.*g" n x
    |> Float.of_string] but faster (10x-15x).  Exact ties are resolved as round-to-even.

    However, it might in rare cases break the contract above.


    It might in some cases appear as if it violates the round-to-even rule:

    {[
      let x = 4.36083208835;;
      let z = 4.3608320883;;
      assert (z = fast_approx_round_significant x ~sf:11)
    ]}

    But in this case so does sprintf, since [x] as a float is slightly
    under-represented:

    {[
      sprintf "%.11g" x = "4.3608320883";;
      sprintf "%.30g" x = "4.36083208834999958014577714493"
    ]}

    More importantly, [round_significant] might sometimes give a different
    result than [sprintf ... |> Float.of_string] because it round-trips through an
    integer.  For example, the decimal fraction 0.009375 is slightly under-represented as
    a float:

    {[ sprintf "%.17g" 0.009375 = "0.0093749999999999997" ]}

    But:

    {[ 0.009375 *. 1e5 = 937.5 ]}

    Therefore:

    {[ round_significant 0.009375 ~significant_digits:3 = 0.00938 ]}

    whereas:

    {[ sprintf "%.3g" 0.009375 = "0.00937" ]}


    In general we believe (and have tested on numerous examples) that the following
    holds for all x:

    {[
      let s = sprintf "%.*g" significant_digits x |> Float.of_string in
      s = round_significant ~significant_digits x
      || s = round_significant ~significant_digits (one_ulp `Up x)
      || s = round_significant ~significant_digits (one_ulp `Down x)
    ]}

    Also, for float representations of decimal fractions (like 0.009375),
    [round_significant] is more likely to give the "desired" result than [sprintf ... |>
    of_string] (that is, the result of rounding the decimal fraction, rather than its
    float representation).  But it's not guaranteed either--see the [4.36083208835]
    example above.

*)
val round_significant : float -> significant_digits:int -> float

(** [round_decimal x ~decimal_digits:n] rounds [x] to the nearest [10**(-n)]. For positive
    [n] it is meant to be equivalent to [sprintf "%.*f" n x |> Float.of_string], but
    faster.

    All the considerations mentioned in [round_significant] apply (both functions use the
    same code path).
*)
val round_decimal : float -> decimal_digits:int -> float


val is_nan : t -> bool

(** Includes positive and negative [Float.infinity]. *)
val is_inf : t -> bool

(** [min_inan] and [max_inan] return, respectively, the min and max of the two given
    values, except when one of the values is a [nan], in which case the other is
    returned. (Returns [nan] if both arguments are [nan].) *)

val min_inan : t -> t -> t
val max_inan : t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( / ) : t -> t -> t
val ( * ) : t -> t -> t
val ( ** ) : t -> t -> t
val ( ~- ) : t -> t

(** Returns the fractional part and the whole (i.e., integer) part. For example, [modf
    (-3.14)] returns [{ fractional = -0.14; integral = -3.; }]! *)
module Parts : sig
  type outer
  type t

  val fractional : t -> outer
  val integral : t -> outer
end
with type outer := t

val modf : t -> Parts.t

(** [mod_float x y] returns a result with the same sign as [x].  It returns [nan] if [y]
    is [0].  It is basically

    {[ let mod_float x y = x -. float(truncate(x/.y)) *. y]}

    not

    {[ let mod_float x y = x -. floor(x/.y) *. y ]}

    and therefore resembles [mod] on integers more than [%]. *)
val mod_float : t -> t -> t

(** {6 Ordinary functions for arithmetic operations}

    These are for modules that inherit from [t], since the infix operators are more
    convenient. *)
val add : t -> t -> t

val sub : t -> t -> t
val neg : t -> t
val scale : t -> t -> t
val abs : t -> t


(** A sub-module designed to be opened to make working with floats more convenient.  *)
module O : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( ~- ) : t -> t

  include Comparisons.Infix with type t := t

  val abs : t -> t
  val neg : t -> t
  val zero : t
  val of_int : int -> t
  val of_float : float -> t
end

(** Similar to [O], except that operators are suffixed with a dot, allowing one to have
    both int and float operators in scope simultaneously. *)
module O_dot : sig
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t
  val ( **. ) : t -> t -> t
  val ( ~-. ) : t -> t
end

(** [to_string x] builds a string [s] representing the float [x] that guarantees the round
    trip, that is such that [Float.equal x (Float.of_string s)].

    It usually yields as few significant digits as possible.  That is, it won't print
    [3.14] as [3.1400000000000001243].  The only exception is that occasionally it will
    output 17 significant digits when the number can be represented with just 16 (but not
    15 or less) of them. *)
val to_string : t -> string

(** Pretty print float, for example [to_string_hum ~decimals:3 1234.1999 = "1_234.200"]
    [to_string_hum ~decimals:3 ~strip_zero:true 1234.1999 = "1_234.2" ].  No delimiters
    are inserted to the right of the decimal. *)
val to_string_hum
  :  ?delimiter:char (** defaults to ['_'] *)
  -> ?decimals:int (** defaults to [3] *)
  -> ?strip_zero:bool (** defaults to [false] *)
  -> t
  -> string

(** Produce a lossy compact string representation of the float.  The float is scaled by
    an appropriate power of 1000 and rendered with one digit after the decimal point,
    except that the decimal point is written as '.', 'k', 'm', 'g', 't', or 'p' to
    indicate the scale factor.  (However, if the digit after the "decimal" point is 0,
    it is suppressed.)

    The smallest scale factor that allows the number to be rendered with at most 3 digits
    to the left of the decimal is used.  If the number is too large for this format (i.e.,
    the absolute value is at least 999.95e15), scientific notation is used instead. E.g.:

    - [to_padded_compact_string     (-0.01) =  "-0  "]
    - [to_padded_compact_string       1.89  =   "1.9"]
    - [to_padded_compact_string 999_949.99  = "999k9"]
    - [to_padded_compact_string 999_950.    =   "1m "]

    In the case where the digit after the "decimal", or the "decimal" itself is omitted,
    the numbers are padded on the right with spaces to ensure the last two columns of the
    string always correspond to the decimal and the digit afterward (except in the case of
    scientific notation, where the exponent is the right-most element in the string and
    could take up to four characters).

    - [to_padded_compact_string    1. =    "1  "]
    - [to_padded_compact_string  1.e6 =    "1m "]
    - [to_padded_compact_string 1.e16 = "1.e+16"]
    - [to_padded_compact_string max_finite_value = "1.8e+308"]

    Numbers in the range -.05 < x < .05 are rendered as "0  " or "-0  ".

    Other cases:

    - [to_padded_compact_string nan          =  "nan  "]
    - [to_padded_compact_string infinity     =  "inf  "]
    - [to_padded_compact_string neg_infinity = "-inf  "]

    Exact ties are resolved to even in the decimal:

    - [to_padded_compact_string      3.25 =  "3.2"]
    - [to_padded_compact_string      3.75 =  "3.8"]
    - [to_padded_compact_string 33_250.   = "33k2"]
    - [to_padded_compact_string 33_350.   = "33k4"]

    [to_padded_compact_string] is defined in terms of [to_padded_compact_string_custom]
    below as
    {[
      let to_padded_compact_string t =
        to_padded_compact_string_custom t ?prefix:None
          ~kilo:"k" ~mega:"m" ~giga:"g" ~tera:"t" ~peta:"p"
          ()
    ]}
*)
val to_padded_compact_string : t -> string

(** Similar to [to_padded_compact_string] but allows the user to provide different
    abbreviations. This can be useful to display currency values, e.g. $1mm3, where
    prefix="$", mega="mm".
*)
val to_padded_compact_string_custom
  :  t
  -> ?prefix:string
  -> kilo:string
  -> mega:string
  -> giga:string
  -> tera:string
  -> ?peta:string
  -> unit
  -> string

(** [int_pow x n] computes [x ** float n] via repeated squaring.  It is generally much
    faster than [**].

    Note that [int_pow x 0] always returns [1.], even if [x = nan].  This
    coincides with [x ** 0.] and is intentional.

    For [n >= 0] the result is identical to an n-fold product of [x] with itself under
    [*.], with a certain placement of parentheses.  For [n < 0] the result is identical
    to [int_pow (1. /. x) (-n)].

    The error will be on the order of [|n|] ulps, essentially the same as if you
    perturbed [x] by up to a ulp and then exponentiated exactly.

    Benchmarks show a factor of 5-10 speedup (relative to [**]) for exponents up to about
    1000 (approximately 10ns vs. 70ns).  For larger exponents the advantage is smaller but
    persists into the trillions.  For a recent or more detailed comparison, run the
    benchmarks.

    Depending on context, calling this function might or might not allocate 2 minor words.
    Even if called in a way that causes allocation, it still appears to be faster than
    [**]. *)
val int_pow : t -> int -> t

(** [square x] returns [x *. x]. *)
val square : t -> t

(** [ldexp x n] returns [x *. 2 ** n] *)
val ldexp : t -> int -> t

(** [frexp f] returns the pair of the significant and the exponent of [f]. When [f] is
    zero, the significant [x] and the exponent [n] of [f] are equal to zero. When [f] is
    non-zero, they are defined by [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)
val frexp : t -> t * int

(** Base 10 logarithm. *)
external log10 : t -> t = "caml_log10_float" "log10"
[@@unboxed] [@@noalloc]

(** [expm1 x] computes [exp x -. 1.0], giving numerically-accurate results even if [x] is
    close to [0.0]. *)
external expm1 : t -> t = "caml_expm1_float" "caml_expm1"
[@@unboxed] [@@noalloc]

(** [log1p x] computes [log(1.0 +. x)] (natural logarithm), giving numerically-accurate
    results even if [x] is close to [0.0]. *)
external log1p : t -> t = "caml_log1p_float" "caml_log1p"
[@@unboxed] [@@noalloc]

(** [copysign x y] returns a float whose absolute value is that of [x] and whose sign is
    that of [y].  If [x] is [nan], returns [nan].  If [y] is [nan], returns either [x] or
    [-. x], but it is not specified which. *)
external copysign : t -> t -> t = "caml_copysign_float" "caml_copysign"
[@@unboxed] [@@noalloc]

(** Cosine.  Argument is in radians. *)
external cos : t -> t = "caml_cos_float" "cos"
[@@unboxed] [@@noalloc]

(** Sine.  Argument is in radians. *)
external sin : t -> t = "caml_sin_float" "sin"
[@@unboxed] [@@noalloc]

(** Tangent.  Argument is in radians. *)
external tan : t -> t = "caml_tan_float" "tan"
[@@unboxed] [@@noalloc]

(** Arc cosine.  The argument must fall within the range [[-1.0, 1.0]].  Result is in
    radians and is between [0.0] and [pi]. *)
external acos : t -> t = "caml_acos_float" "acos"
[@@unboxed] [@@noalloc]

(** Arc sine.  The argument must fall within the range [[-1.0, 1.0]].  Result is in
    radians and is between [-pi/2] and [pi/2]. *)
external asin : t -> t = "caml_asin_float" "asin"
[@@unboxed] [@@noalloc]

(** Arc tangent.  Result is in radians and is between [-pi/2] and [pi/2]. *)
external atan : t -> t = "caml_atan_float" "atan"
[@@unboxed] [@@noalloc]

(** [atan2 y x] returns the arc tangent of [y /. x].  The signs of [x] and [y] are used to
    determine the quadrant of the result.  Result is in radians and is between [-pi] and
    [pi]. *)
external atan2 : t -> t -> t = "caml_atan2_float" "atan2"
[@@unboxed] [@@noalloc]

(** [hypot x y] returns [sqrt(x *. x + y *. y)], that is, the length of the hypotenuse of
    a right-angled triangle with sides of length [x] and [y], or, equivalently, the
    distance of the point [(x,y)] to origin. *)
external hypot : t -> t -> t = "caml_hypot_float" "caml_hypot"
[@@unboxed] [@@noalloc]

(** Hyperbolic cosine.  Argument is in radians. *)
external cosh : t -> t = "caml_cosh_float" "cosh"
[@@unboxed] [@@noalloc]

(** Hyperbolic sine.  Argument is in radians. *)
external sinh : t -> t = "caml_sinh_float" "sinh"
[@@unboxed] [@@noalloc]

(** Hyperbolic tangent.  Argument is in radians. *)
external tanh : t -> t = "caml_tanh_float" "tanh"
[@@unboxed] [@@noalloc]

(** Square root. *)
external sqrt : t -> t = "caml_sqrt_float" "sqrt"
[@@unboxed] [@@noalloc]

(** Exponential. *)
external exp : t -> t = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]

(** Natural logarithm. *)
external log : t -> t = "caml_log_float" "log"
[@@unboxed] [@@noalloc]

(** Excluding nan the floating-point "number line" looks like:
    {v
             t                Class.t    example
           ^ neg_infinity     Infinite   neg_infinity
           | neg normals      Normal     -3.14
           | neg subnormals   Subnormal  -.2. ** -1023.
           | (-/+) zero       Zero       0.
           | pos subnormals   Subnormal  2. ** -1023.
           | pos normals      Normal     3.14
           v infinity         Infinite   infinity
    v} *)
module Class : sig
  type t =
    | Infinite
    | Nan
    | Normal
    | Subnormal
    | Zero
  [@@deriving_inline compare, enumerate, sexp]

  val compare : t -> t -> int
  val all : t list

  include Ppx_sexp_conv_lib.Sexpable.S with type t := t

  [@@@end]

  include Stringable.S with type t := t
end

val classify : t -> Class.t

(** [is_finite t] returns [true] iff [classify t] is in [Normal; Subnormal; Zero;]. *)
val is_finite : t -> bool

(*_ Caution: If we remove this sig item, [sign] will still be present from
  [Comparable.With_zero]. *)

val sign : t -> Sign.t
[@@deprecated "[since 2016-01] Replace [sign] with [robust_sign] or [sign_exn]"]

(** The sign of a float.  Both [-0.] and [0.] map to [Zero].  Raises on nan.  All other
    values map to [Neg] or [Pos]. *)
val sign_exn : t -> Sign.t

(** The sign of a float, with support for NaN. Both [-0.] and [0.] map to [Zero].  All NaN
    values map to [Nan]. All other values map to [Neg] or [Pos]. *)
val sign_or_nan : t -> Sign_or_nan.t

(** These functions construct and destruct 64-bit floating point numbers based on their
    IEEE representation with a sign bit, an 11-bit non-negative (biased) exponent, and a
    52-bit non-negative mantissa (or significand).  See
    {{:http://en.wikipedia.org/wiki/Double-precision_floating-point_format} Wikipedia} for
    details of the encoding.

    In particular, if 1 <= exponent <= 2046, then:

    {[
      create_ieee_exn ~negative:false ~exponent ~mantissa
      = 2 ** (exponent - 1023) * (1 + (2 ** -52) * mantissa)
    ]} *)
val create_ieee : negative:bool -> exponent:int -> mantissa:Int63.t -> t Or_error.t

val create_ieee_exn : negative:bool -> exponent:int -> mantissa:Int63.t -> t
val ieee_negative : t -> bool
val ieee_exponent : t -> int
val ieee_mantissa : t -> Int63.t

(** S-expressions contain at most 8 significant digits. *)
module Terse : sig
  type nonrec t = t [@@deriving_inline sexp]

  include Ppx_sexp_conv_lib.Sexpable.S with type t := t

  [@@@end]

  include Stringable.S with type t := t
end

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val box : t -> t
  val clamp_unchecked : t -> min:t -> max:t -> t
  val lower_bound_for_int : int -> t
  val upper_bound_for_int : int -> t
  val specialized_hash : t -> int
  val one_ulp_less_than_half : t
  val int63_round_nearest_portable_alloc_exn : t -> Int63.t
  val int63_round_nearest_arch64_noalloc_exn : t -> Int63.t
  val iround_nearest_exn_64 : t -> int
end
