(**
   Integers.


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

type t

exception Overflow

external init: unit -> unit = "ml_z_init"
let _ = init ()

let _ = Callback.register_exception "ml_z_overflow" Overflow

external is_small_int: t -> bool = "%obj_is_int"
external unsafe_to_int: t -> int = "%identity"
external of_int: int -> t = "%identity"

external c_neg: t -> t = "ml_z_neg"

let neg x =
  if is_small_int x && unsafe_to_int x <> min_int
  then of_int (- unsafe_to_int x)
  else c_neg x

external c_add: t -> t -> t = "ml_z_add"

let add x y =
  if is_small_int x && is_small_int y then begin
    let z = unsafe_to_int x + unsafe_to_int y in
    (* Overflow check -- Hacker's Delight, section 2.12 *)
    if (z lxor unsafe_to_int x) land (z lxor unsafe_to_int y) >= 0 
    then of_int z
    else c_add x y
  end else
    c_add x y

external c_sub: t -> t -> t = "ml_z_sub"

let sub x y =
  if is_small_int x && is_small_int y then begin
    let z = unsafe_to_int x - unsafe_to_int y in
    (* Overflow check -- Hacker's Delight, section 2.12 *)
    if (unsafe_to_int x lxor unsafe_to_int y)
       land (z lxor unsafe_to_int x) >= 0 
    then of_int z
    else c_sub x y
  end else
    c_sub x y

external mul_overflows: int -> int -> bool = "ml_z_mul_overflows" [@@noalloc]
external c_mul: t -> t -> t = "ml_z_mul"

let mul x y =
  if is_small_int x && is_small_int y
  && not (mul_overflows (unsafe_to_int x) (unsafe_to_int y))
  then of_int (unsafe_to_int x * unsafe_to_int y)
  else c_mul x y

external c_div: t -> t -> t = "ml_z_div"

let div x y =
  if is_small_int y then
    if unsafe_to_int y = -1 then
      neg x
    else if is_small_int x then
      of_int (unsafe_to_int x / unsafe_to_int y)
    else
      c_div x y
  else
    c_div x y

external cdiv: t -> t -> t = "ml_z_cdiv"
external fdiv: t -> t -> t = "ml_z_fdiv"

external c_rem: t -> t -> t = "ml_z_rem"

let rem x y =
  if is_small_int y then
    if unsafe_to_int y = -1 then
      of_int 0
    else if is_small_int x then
      of_int (unsafe_to_int x mod unsafe_to_int y)
    else
      c_rem x y
  else
    c_rem x y

external div_rem: t -> t -> (t * t) = "ml_z_div_rem"

external c_divexact: t -> t -> t = "ml_z_divexact"

let divexact x y =
  if is_small_int y then
    if unsafe_to_int y = -1 then
      neg x
    else if is_small_int x then
      of_int (unsafe_to_int x / unsafe_to_int y)
    else
      c_divexact x y
  else
    c_divexact x y

external c_succ: t -> t = "ml_z_succ"

let succ x =
  if is_small_int x && unsafe_to_int x <> max_int
  then of_int (unsafe_to_int x + 1)
  else c_succ x

external c_pred: t -> t = "ml_z_pred"

let pred x =
  if is_small_int x && unsafe_to_int x <> min_int
  then of_int (unsafe_to_int x - 1)
  else c_pred x

external c_abs: t -> t = "ml_z_abs"

let abs x =
  if is_small_int x then
    if unsafe_to_int x >= 0 then x
    else if unsafe_to_int x <> min_int then
      of_int (- unsafe_to_int x)
    else
      c_abs x
  else
    c_abs x

external c_logand: t -> t -> t = "ml_z_logand"

let logand x y =
  if is_small_int x && is_small_int y
  then of_int (unsafe_to_int x land unsafe_to_int y)
  else c_logand x y

external c_logor: t -> t -> t = "ml_z_logor"

let logor x y =
  if is_small_int x && is_small_int y
  then of_int (unsafe_to_int x lor unsafe_to_int y)
  else c_logor x y

external c_logxor: t -> t -> t = "ml_z_logxor"

let logxor x y =
  if is_small_int x && is_small_int y
  then of_int (unsafe_to_int x lxor unsafe_to_int y)
  else c_logxor x y

external c_lognot: t -> t = "ml_z_lognot"

let lognot x =
  if is_small_int x
  then of_int (unsafe_to_int x lxor (-1))
  else c_lognot x

external c_shift_left: t -> int -> t = "ml_z_shift_left"

let shift_left x y =
  if is_small_int x && y >= 0 && y < Sys.word_size then begin
    let z = unsafe_to_int x lsl y in
    if z asr y = unsafe_to_int x
    then of_int z
    else c_shift_left x y
  end else
    c_shift_left x y

external c_shift_right: t -> int -> t = "ml_z_shift_right"

let shift_right x y =
  if is_small_int x && y >= 0 then
    of_int
      (unsafe_to_int x asr (if y < Sys.word_size then y else Sys.word_size - 1))
  else
    c_shift_right x y

external c_shift_right_trunc: t -> int -> t = "ml_z_shift_right_trunc"

let shift_right_trunc x y =
  if is_small_int x && y >= 0 then
    if y >= Sys.word_size then
      of_int 0
    else if unsafe_to_int x >= 0 then
      of_int (unsafe_to_int x lsr y)
    else
      of_int (- ((- unsafe_to_int x) lsr y))
  else
    c_shift_right_trunc x y

external of_int32: int32 -> t = "ml_z_of_int32"
external of_int64: int64 -> t = "ml_z_of_int64"
external of_nativeint: nativeint -> t = "ml_z_of_nativeint"
external of_float: float -> t = "ml_z_of_float"

external c_to_int: t -> int = "ml_z_to_int"

let to_int x =
  if is_small_int x then unsafe_to_int x else c_to_int x

external to_int32: t -> int32 = "ml_z_to_int32"
external to_int64: t -> int64 = "ml_z_to_int64"
external to_nativeint: t -> nativeint = "ml_z_to_nativeint"
external format: string -> t -> string = "ml_z_format"
external of_substring_base: int -> string -> pos:int -> len:int -> t = "ml_z_of_substring_base"
external compare: t -> t -> int = "ml_z_compare" [@@noalloc]
external equal: t -> t -> bool = "ml_z_equal" [@@noalloc]
external sign: t -> int = "ml_z_sign" [@@noalloc]
external gcd: t -> t -> t = "ml_z_gcd"
external gcdext_intern: t -> t -> (t * t * bool) = "ml_z_gcdext_intern"
external sqrt: t -> t = "ml_z_sqrt"
external sqrt_rem: t -> (t * t) = "ml_z_sqrt_rem"
external numbits: t -> int = "ml_z_numbits" [@@noalloc]
external trailing_zeros: t -> int = "ml_z_trailing_zeros" [@@noalloc]
external popcount: t -> int = "ml_z_popcount"
external hamdist: t -> t -> int = "ml_z_hamdist"
external size: t -> int = "ml_z_size" [@@noalloc]
external fits_int: t -> bool = "ml_z_fits_int" [@@noalloc]
external fits_int32: t -> bool = "ml_z_fits_int32" [@@noalloc]
external fits_int64: t -> bool = "ml_z_fits_int64" [@@noalloc]
external fits_nativeint: t -> bool = "ml_z_fits_nativeint" [@@noalloc]
external extract: t -> int -> int -> t = "ml_z_extract"
external powm: t -> t -> t -> t = "ml_z_powm"
external pow: t -> int -> t = "ml_z_pow"
external powm_sec: t -> t -> t -> t = "ml_z_powm_sec"
external root: t -> int -> t = "ml_z_root"
external rootrem: t -> int -> t * t = "ml_z_rootrem"
external invert: t -> t -> t = "ml_z_invert"
external perfect_power: t -> bool = "ml_z_perfect_power"
external perfect_square: t -> bool = "ml_z_perfect_square"
external probab_prime: t -> int -> int = "ml_z_probab_prime"
external nextprime: t -> t = "ml_z_nextprime"
external hash: t -> int = "ml_z_hash" [@@noalloc]
external to_bits: t -> string = "ml_z_to_bits"
external of_bits: string -> t = "ml_z_of_bits"
external divisible: t -> t -> bool = "ml_z_divisible"
external congruent: t -> t -> t -> bool = "ml_z_congruent"
external jacobi: t -> t -> int = "ml_z_jacobi"
external legendre: t -> t -> int = "ml_z_legendre"
external kronecker: t -> t -> int = "ml_z_kronecker"
external remove: t -> t -> t * int = "ml_z_remove"
external fac: int -> t = "ml_z_fac"
external fac2: int -> t = "ml_z_fac2"
external facM: int -> int -> t = "ml_z_facM"
external primorial: int -> t = "ml_z_primorial"
external bin: t -> int -> t = "ml_z_bin"
external fib: int -> t = "ml_z_fib"
external lucnum: int -> t = "ml_z_lucnum"

let zero = of_int 0
let one = of_int 1
let minus_one = of_int (-1)

let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b

let leq a b = compare a b <= 0
let geq a b = compare a b >= 0
let lt a b = compare a b < 0
let gt a b = compare a b > 0

let to_string = format "%d"

let of_string s = of_substring_base 0 s ~pos:0 ~len:(String.length s)
let of_substring = of_substring_base 0
let of_string_base base s = of_substring_base base s ~pos:0 ~len:(String.length s)

let ediv_rem a b =
  (* we have a = q * b + r, but [Big_int]'s remainder satisfies 0 <= r < |b|,
     while [Z]'s remainder satisfies -|b| < r < |b| and sign(r) = sign(a)
   *)
   let q,r = div_rem a b in
   if sign r >= 0 then (q,r) else
   if sign b >= 0 then (pred q, add r b)
   else (succ q, sub r b)

let ediv a b =
   if sign b >= 0 then fdiv a b else cdiv a b

let erem a b =
   let r = rem a b in
   if sign r >= 0 then r else add r (abs b)

let gcdext u v =
  match sign u, sign v with
  (* special cases: one argument is null *)
  |  0,  0 -> zero, zero, zero
  |  0,  1 -> v, zero, one
  |  0, -1 -> neg v, zero, minus_one
  |  1,  0 -> u, one, zero
  | -1,  0 -> neg u, minus_one, zero
  | _ ->
     (* general case *)
     let g,s,z = gcdext_intern u v in
     if z then g, s, div (sub g (mul u s)) v
     else g, div (sub g (mul v s)) u, s

let lcm u v =
  if u = zero || v = zero then zero
  else
    let g = gcd u v in
    abs (mul (divexact u g) v)

external testbit_internal: t -> int -> bool = "ml_z_testbit" [@@noalloc]
let testbit x n =
  if n >= 0 then testbit_internal x n else invalid_arg "Z.testbit"
(* The test [n >= 0] is done in Caml rather than in the C stub code
   so that the latter raises no exceptions and can be declared [@@noalloc]. *)

let is_odd x = testbit_internal x 0
let is_even x  = not (testbit_internal x 0)

let signed_extract x o l =
  if o < 0 then invalid_arg "Z.signed_extract: negative bit offset";
  if l < 1 then invalid_arg "Z.signed_extract: nonpositive bit length";
  if testbit x (o + l - 1)
  then lognot (extract (lognot x) o l)
  else extract x o l

let log2 x =
  if sign x > 0 then (numbits x) - 1 else invalid_arg "Z.log2"
let log2up x =
  if sign x > 0 then numbits (pred x) else invalid_arg "Z.log2up"

(* Consider a real number [r] such that
   - the integral part of [r] is the bigint [x]
   - 2^54 <= |x| < 2^63
   - the fractional part of [r] is 0 if [exact = true],
     nonzero if [exact = false].
   Then, the following function returns [r] correctly rounded
   according to the current rounding mode of the processor.
   This is an instance of the "round to odd" technique formalized in
   "When double rounding is odd" by S. Boldo and G. Melquiond.
   The claim above is lemma Fappli_IEEE_extra.round_odd_fix
   from the CompCert Coq development. *)

let round_to_float x exact =
  let m = to_int64 x in
  (* Unless the fractional part is exactly 0, round m to an odd integer *)
  let m = if exact then m else Int64.logor m 1L in
  (* Then convert m to float, with the current rounding mode. *)
  Int64.to_float m

let to_float x =
  if Obj.is_int (Obj.repr x) then
    (* Fast path *)
    float_of_int (Obj.magic x : int)
  else begin
    let n = numbits x in
    if n <= 63 then
      Int64.to_float (to_int64 x)
    else begin
      let n = n - 55 in
      (* Extract top 55 bits of x *)
      let top = shift_right x n in
      (* Check if the other bits are all zero *)
      let exact = equal x (shift_left top n) in
      (* Round to float and apply exponent *)
      ldexp (round_to_float top exact) n
    end
  end

let print x = print_string (to_string x)
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)

let (~-) = neg
let (~+) x = x
let (+) = add
let (-) = sub
let ( * ) = mul
let (/) = div
external (/>): t -> t -> t = "ml_z_cdiv"
external (/<): t -> t -> t = "ml_z_fdiv"
let (/|) = divexact
let (mod) = rem
let (land) = logand
let (lor) = logor
let (lxor) = logxor
let (~!) = lognot
let (lsl) = shift_left
let (asr) = shift_right
external (~$): int -> t = "%identity"
external ( ** ): t -> int -> t = "ml_z_pow"

module Compare = struct
  let (=) = equal
  let (<) = lt
  let (>) = gt
  let (<=) = leq
  let (>=) = geq
  let (<>) a b = not (equal a b)
end

let version = Zarith_version.version
