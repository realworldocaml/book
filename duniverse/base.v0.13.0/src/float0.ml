open! Import

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Float_replace_polymorphic_compare

let ceil = Caml.ceil
let floor = Caml.floor
let mod_float = Caml.mod_float
let modf = Caml.modf
let float_of_string = Caml.float_of_string
let nan = Caml.nan
let infinity = Caml.infinity
let neg_infinity = Caml.neg_infinity
let max_finite_value = Caml.max_float
let epsilon_float = Caml.epsilon_float
let classify_float = Caml.classify_float
let abs_float = Caml.abs_float
let ( ** ) = Caml.( ** )

(* The bits of INRIA's [Pervasives] that we just want to expose in [Float]. Most are
   already deprecated in [Pervasives], and eventually all of them should be. *)
include (
  Caml :
  sig
    external frexp : float -> float * int = "caml_frexp_float"

    external ldexp
      :  (float[@unboxed])
      -> (int[@untagged])
      -> (float[@unboxed])
      = "caml_ldexp_float" "caml_ldexp_float_unboxed"
    [@@noalloc]

    external log10 : float -> float = "caml_log10_float" "log10"
    [@@unboxed] [@@noalloc]

    external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
    [@@unboxed] [@@noalloc]

    external log1p : float -> float = "caml_log1p_float" "caml_log1p"
    [@@unboxed] [@@noalloc]

    external copysign : float -> float -> float = "caml_copysign_float" "caml_copysign"
    [@@unboxed] [@@noalloc]

    external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
    external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
    external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
    external acos : float -> float = "caml_acos_float" "acos" [@@unboxed] [@@noalloc]
    external asin : float -> float = "caml_asin_float" "asin" [@@unboxed] [@@noalloc]
    external atan : float -> float = "caml_atan_float" "atan" [@@unboxed] [@@noalloc]

    external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
    [@@unboxed] [@@noalloc]

    external hypot : float -> float -> float = "caml_hypot_float" "caml_hypot"
    [@@unboxed] [@@noalloc]

    external cosh : float -> float = "caml_cosh_float" "cosh" [@@unboxed] [@@noalloc]
    external sinh : float -> float = "caml_sinh_float" "sinh" [@@unboxed] [@@noalloc]
    external tanh : float -> float = "caml_tanh_float" "tanh" [@@unboxed] [@@noalloc]
    external sqrt : float -> float = "caml_sqrt_float" "sqrt" [@@unboxed] [@@noalloc]
    external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
    external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
  end)

(* We need this indirection because these are exposed as "val" instead of "external" *)
let frexp = frexp
let ldexp = ldexp
let is_nan x = (x : float) <> x

(* An order-preserving bijection between all floats except for NaNs, and 99.95% of
   int64s.

   Note we don't distinguish 0. and -0. as separate values here, they both map to 0L, which
   maps back to 0.

   This should work both on little-endian and high-endian CPUs.  Wikipedia says: "on
   modern standard computers (i.e., implementing IEEE 754), one may in practice safely
   assume that the endianness is the same for floating point numbers as for integers"
   (http://en.wikipedia.org/wiki/Endianness#Floating-point_and_endianness).
*)
let to_int64_preserve_order t =
  if is_nan t
  then None
  else if t = 0.
  then (* also includes -0. *)
    Some 0L
  else if t > 0.
  then Some (Caml.Int64.bits_of_float t)
  else Some (Caml.Int64.neg (Caml.Int64.bits_of_float (-.t)))
;;

let to_int64_preserve_order_exn x = Option.value_exn (to_int64_preserve_order x)

let of_int64_preserve_order x =
  if Int64_replace_polymorphic_compare.( >= ) x 0L
  then Caml.Int64.float_of_bits x
  else ~-.(Caml.Int64.float_of_bits (Caml.Int64.neg x))
;;

let one_ulp dir t =
  match to_int64_preserve_order t with
  | None -> Caml.nan
  | Some x ->
    of_int64_preserve_order
      (Caml.Int64.add
         x
         (match dir with
          | `Up -> 1L
          | `Down -> -1L))
;;

(* [upper_bound_for_int] and [lower_bound_for_int] are for calculating the max/min float
   that fits in a given-size integer when rounded towards 0 (using [int_of_float]).

   max_int/min_int depend on [num_bits], e.g. +/- 2^30, +/- 2^62 if 31-bit, 63-bit
   (respectively) while float is IEEE standard for double (52 significant bits).

   In all cases, we want to guarantee that
   [lower_bound_for_int <= x <= upper_bound_for_int]
   iff [int_of_float x] fits in an int with [num_bits] bits.

   [2 ** (num_bits - 1)] is the first float greater that max_int, we use the preceding
   float as upper bound.

   [- (2 ** (num_bits - 1))] is equal to min_int.
   For lower bound we look for the smallest float [f] satisfying [f > min_int - 1] so that
   [f] rounds toward zero to [min_int]

   So in particular we will have:
   [lower_bound_for_int x <= - (2 ** (1-x))]
   [upper_bound_for_int x  <    2 ** (1-x) ]
*)
let upper_bound_for_int num_bits =
  let exp = Caml.float_of_int (num_bits - 1) in
  one_ulp `Down (2. ** exp)
;;

let is_x_minus_one_exact x =
  (* [x = x -. 1.] does not work with x87 floating point arithmetic backend (which is used
     on 32-bit ocaml) because of 80-bit register precision of intermediate computations.

     An alternative way of computing this: [x -. one_ulp `Down x <= 1.] is also prone to
     the same precision issues: you need to make sure [x] is 64-bit.
  *)
  let open Int64_replace_polymorphic_compare in
  not (Caml.Int64.bits_of_float x = Caml.Int64.bits_of_float (x -. 1.))
;;

let lower_bound_for_int num_bits =
  let exp = Caml.float_of_int (num_bits - 1) in
  let min_int_as_float = ~-.(2. ** exp) in
  let open Int_replace_polymorphic_compare in
  if num_bits - 1 < 53 (* 53 = #bits in the float's mantissa with sign included *)
  then (
    (* The smallest float that rounds towards zero to [min_int] is
       [min_int - 1 + epsilon] *)
    assert (is_x_minus_one_exact min_int_as_float);
    one_ulp `Up (min_int_as_float -. 1.))
  else (
    (* [min_int_as_float] is already the smallest float [f] satisfying [f > min_int - 1]. *)
    assert (not (is_x_minus_one_exact min_int_as_float));
    min_int_as_float)
;;

(* Float clamping is structured slightly differently than clamping for other types, so
   that we get the behavior of [clamp_unchecked nan ~min ~max = nan] (for any [min] and
   [max]) for free.
*)
let clamp_unchecked (t : float) ~min ~max =
  if t < min then min else if max < t then max else t
;;

let box =
  (* Prevent potential constant folding of [+. 0.] in the near ocamlopt future. *)
  let x = Sys0.opaque_identity 0. in
  fun f -> f +. x
;;

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Float_replace_polymorphic_compare
