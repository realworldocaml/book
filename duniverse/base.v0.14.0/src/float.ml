open! Import
open! Printf
module Bytes = Bytes0
include Float0

let raise_s = Error.raise_s

module T = struct
  type t = float [@@deriving_inline hash, sexp, sexp_grammar]

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_float

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_float in
    fun x -> func x
  ;;

  let t_of_sexp = (float_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (sexp_of_float : t -> Ppx_sexp_conv_lib.Sexp.t)

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "float" ]
      ; ggid = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
      ; types = [ "t", Implicit_var 0 ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ float_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "float.ml.T"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]

  let compare = Float_replace_polymorphic_compare.compare
end

include T
include Comparator.Make (T)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open Float_replace_polymorphic_compare

let invariant (_ : t) = ()
let to_float x = x
let of_float x = x

let of_string s =
  try float_of_string s with
  | _ -> invalid_argf "Float.of_string %s" s ()
;;

external format_float : string -> float -> string = "caml_format_float"

(* Stolen from [pervasives.ml].  Adds a "." at the end if needed.  It is in
   [pervasives.mli], but it also says not to use it directly, so we copy and paste the
   code. It makes the assumption on the string passed in argument that it was returned by
   [format_float]. *)
let valid_float_lexem s =
  let l = String.length s in
  let rec loop i =
    if Int_replace_polymorphic_compare.( >= ) i l
    then s ^ "."
    else (
      match s.[i] with
      | '0' .. '9' | '-' -> loop (i + 1)
      | _ -> s)
  in
  loop 0
;;

(* Let [y] be a power of 2.  Then the next representable float is:
   [z = y * (1 + 2 ** -52)]
   and the previous one is
   [x = y * (1 - 2 ** -53)]

   In general, every two adjacent floats are within a factor of between [1 + 2**-53]
   and [1 + 2**-52] from each other, that is within [1 + 1.1e-16] and [1 + 2.3e-16].

   So if the decimal representation of a float starts with "1", then its adjacent floats
   will usually differ from it by 1, and sometimes by 2, at the 17th significant digit
   (counting from 1).

   On the other hand, if the decimal representation starts with "9", then the adjacent
   floats will be off by no more than 23 at the 16th and 17th significant digits.

   E.g.:

   {v
     # sprintf "%.17g" (1024. *. (1. -. 2.** (-53.)));;
                             11111111
                   1234 5678901234567
     - : string = "1023.9999999999999"
   v}
   Printing a couple of extra digits reveals that the difference indeed is roughly 11 at
   digits 17th and 18th (that is, 13th and 14th after "."):

   {v
     # sprintf "%.19g" (1024. *. (1. -. 2.** (-53.)));;
                             1111111111
                   1234 567890123456789
     - : string = "1023.999999999999886"
   v}

   The ulp (the difference between adjacent floats) is twice as big on the other side of
   1024.:

   {v
     # sprintf "%.19g" (1024. *. (1. +. 2.** (-52.)));;
                             1111111111
                   1234 567890123456789
     - : string = "1024.000000000000227"
   v}

   Now take a power of 2 which starts with 99:

   {v
     # 2.**93. ;;
                          1111111111
                 1 23456789012345678
     - : float = 9.9035203142830422e+27

     # 2.**93. *. (1. +. 2.** (-52.));;
     - : float = 9.9035203142830444e+27

     # 2.**93. *. (1. -. 2.** (-53.));;
     - : float = 9.9035203142830411e+27
   v}

   The difference between 2**93 and its two neighbors is slightly more than, respectively,
   1 and 2 at significant digit 16.

   Those examples show that:
   - 17 significant digits is always sufficient to represent a float without ambiguity
   - 15th significant digit can always be represented accurately
   - converting a decimal number with 16 significant digits to its nearest float and back
     can change the last decimal digit by no more than 1

   To make sure that floats obtained by conversion from decimal fractions (e.g. "3.14")
   are printed without trailing non-zero digits, one should choose the first among the
   '%.15g', '%.16g', and '%.17g' representations which does round-trip:

   {v
     # sprintf "%.15g" 3.14;;
     - : string = "3.14"                     (* pick this one *)
     # sprintf "%.16g" 3.14;;
     - : string = "3.14"
     # sprintf "%.17g" 3.14;;
     - : string = "3.1400000000000001"       (* do not pick this one *)

     # sprintf "%.15g" 8.000000000000002;;
     - : string = "8"                        (* do not pick this one--does not round-trip *)
     # sprintf "%.16g" 8.000000000000002;;
     - : string = "8.000000000000002"        (* prefer this one *)
     # sprintf "%.17g" 8.000000000000002;;
     - : string = "8.0000000000000018"       (* this one has one digit of junk at the end *)
   v}

   Skipping the '%.16g' in the above procedure saves us some time, but it means that, as
   seen in the second example above, occasionally numbers with exactly 16 significant
   digits will have an error introduced at the 17th digit.  That is probably OK for
   typical use, because a number with 16 significant digits is "ugly" already.  Adding one
   more doesn't make it much worse for a human reader.

   On the other hand, we cannot skip '%.15g' and only look at '%.16g' and '%.17g', since
   the inaccuracy at the 16th digit might introduce the noise we want to avoid:

   {v
     # sprintf "%.15g" 9.992;;
     - : string = "9.992"                    (* pick this one *)
     # sprintf "%.16g" 9.992;;
     - : string = "9.992000000000001"        (* do not pick this one--junk at the end *)
     # sprintf "%.17g" 9.992;;
     - : string = "9.9920000000000009"
   v}
*)
let to_string x =
  valid_float_lexem
    (let y = format_float "%.15g" x in
     if float_of_string y = x then y else format_float "%.17g" x)
;;

let max_value = infinity
let min_value = neg_infinity
let min_positive_subnormal_value = 2. ** -1074.
let min_positive_normal_value = 2. ** -1022.
let zero = 0.
let one = 1.
let minus_one = -1.
let pi = 0x3.243F6A8885A308D313198A2E037073
let sqrt_pi = 0x1.C5BF891B4EF6AA79C3B0520D5DB938
let sqrt_2pi = 0x2.81B263FEC4E0B2CAF9483F5CE459DC
let euler = 0x0.93C467E37DB0C7A4D1BE3F810152CB
let of_int = Int.to_float
let to_int = Int.of_float
let of_int63 i = Int63.to_float i
let of_int64 i = Caml.Int64.to_float i
let to_int64 = Caml.Int64.of_float
let iround_lbound = lower_bound_for_int Int.num_bits
let iround_ubound = upper_bound_for_int Int.num_bits

(* The performance of the "exn" rounding functions is important, so they are written
   out separately, and tuned individually.  (We could have the option versions call
   the "exn" versions, but that imposes arguably gratuitous overhead---especially
   in the case where the capture of backtraces is enabled upon "with"---and that seems
   not worth it when compared to the relatively small amount of code duplication.) *)

(* Error reporting below is very carefully arranged so that, e.g., [iround_nearest_exn]
   itself can be inlined into callers such that they don't need to allocate a box for the
   [float] argument.  This is done with a box [box] function carefully chosen to allow the
   compiler to create a separate box for the float only in error cases.  See, e.g.,
   [../../zero/test/price_test.ml] for a mechanical test of this property when building
   with [X_LIBRARY_INLINING=true]. *)

let iround_up t =
  if t > 0.0
  then (
    let t' = ceil t in
    if t' <= iround_ubound then Some (Int.of_float_unchecked t') else None)
  else if t >= iround_lbound
  then Some (Int.of_float_unchecked t)
  else None
;;

let[@ocaml.inline always] iround_up_exn t =
  if t > 0.0
  then (
    let t' = ceil t in
    if t' <= iround_ubound
    then Int.of_float_unchecked t'
    else invalid_argf "Float.iround_up_exn: argument (%f) is too large" (box t) ())
  else if t >= iround_lbound
  then Int.of_float_unchecked t
  else invalid_argf "Float.iround_up_exn: argument (%f) is too small or NaN" (box t) ()
;;

let iround_down t =
  if t >= 0.0
  then if t <= iround_ubound then Some (Int.of_float_unchecked t) else None
  else (
    let t' = floor t in
    if t' >= iround_lbound then Some (Int.of_float_unchecked t') else None)
;;

let[@ocaml.inline always] iround_down_exn t =
  if t >= 0.0
  then
    if t <= iround_ubound
    then Int.of_float_unchecked t
    else invalid_argf "Float.iround_down_exn: argument (%f) is too large" (box t) ()
  else (
    let t' = floor t in
    if t' >= iround_lbound
    then Int.of_float_unchecked t'
    else
      invalid_argf "Float.iround_down_exn: argument (%f) is too small or NaN" (box t) ())
;;

let iround_towards_zero t =
  if t >= iround_lbound && t <= iround_ubound
  then Some (Int.of_float_unchecked t)
  else None
;;

let[@ocaml.inline always] iround_towards_zero_exn t =
  if t >= iround_lbound && t <= iround_ubound
  then Int.of_float_unchecked t
  else
    invalid_argf
      "Float.iround_towards_zero_exn: argument (%f) is out of range or NaN"
      (box t)
      ()
;;

(* Outside of the range (round_nearest_lb..round_nearest_ub), all representable doubles
   are integers in the mathematical sense, and [round_nearest] should be identity.

   However, for odd numbers with the absolute value between 2**52 and 2**53, the formula
   [round_nearest x = floor (x + 0.5)] does not hold:

   {v
     # let naive_round_nearest x = floor (x +. 0.5);;
     # let x = 2. ** 52. +. 1.;;
     val x : float = 4503599627370497.
     # naive_round_nearest x;;
     - :     float = 4503599627370498.
   v}
*)

let round_nearest_lb = -.(2. ** 52.)
let round_nearest_ub = 2. ** 52.

(* For [x = one_ulp `Down 0.5], the formula [floor (x +. 0.5)] for rounding to nearest
   does not work, because the exact result is halfway between [one_ulp `Down 1.] and [1.],
   and it gets rounded up to [1.] due to the round-ties-to-even rule. *)
let one_ulp_less_than_half = one_ulp `Down 0.5

let add_half_for_round_nearest t =
  t
  +.
  if t = one_ulp_less_than_half
  then one_ulp_less_than_half (* since t < 0.5, make sure the result is < 1.0 *)
  else 0.5
;;

let iround_nearest_32 t =
  if t >= 0.
  then (
    let t' = add_half_for_round_nearest t in
    if t' <= iround_ubound then Some (Int.of_float_unchecked t') else None)
  else (
    let t' = floor (t +. 0.5) in
    if t' >= iround_lbound then Some (Int.of_float_unchecked t') else None)
;;

let iround_nearest_64 t =
  if t >= 0.
  then
    if t < round_nearest_ub
    then Some (Int.of_float_unchecked (add_half_for_round_nearest t))
    else if t <= iround_ubound
    then Some (Int.of_float_unchecked t)
    else None
  else if t > round_nearest_lb
  then Some (Int.of_float_unchecked (floor (t +. 0.5)))
  else if t >= iround_lbound
  then Some (Int.of_float_unchecked t)
  else None
;;

let iround_nearest =
  match Word_size.word_size with
  | W64 -> iround_nearest_64
  | W32 -> iround_nearest_32
;;

let iround_nearest_exn_32 t =
  if t >= 0.
  then (
    let t' = add_half_for_round_nearest t in
    if t' <= iround_ubound
    then Int.of_float_unchecked t'
    else invalid_argf "Float.iround_nearest_exn: argument (%f) is too large" (box t) ())
  else (
    let t' = floor (t +. 0.5) in
    if t' >= iround_lbound
    then Int.of_float_unchecked t'
    else invalid_argf "Float.iround_nearest_exn: argument (%f) is too small" (box t) ())
;;

let[@ocaml.inline always] iround_nearest_exn_64 t =
  if t >= 0.
  then
    if t < round_nearest_ub
    then Int.of_float_unchecked (add_half_for_round_nearest t)
    else if t <= iround_ubound
    then Int.of_float_unchecked t
    else invalid_argf "Float.iround_nearest_exn: argument (%f) is too large" (box t) ()
  else if t > round_nearest_lb
  then Int.of_float_unchecked (floor (t +. 0.5))
  else if t >= iround_lbound
  then Int.of_float_unchecked t
  else
    invalid_argf "Float.iround_nearest_exn: argument (%f) is too small or NaN" (box t) ()
;;

let iround_nearest_exn =
  match Word_size.word_size with
  | W64 -> iround_nearest_exn_64
  | W32 -> iround_nearest_exn_32
;;

(* The following [iround_exn] and [iround] functions are slower than the ones above.
   Their equivalence to those functions is tested in the unit tests below. *)

let[@inline] iround_exn ?(dir = `Nearest) t =
  match dir with
  | `Zero -> iround_towards_zero_exn t
  | `Nearest -> iround_nearest_exn t
  | `Up -> iround_up_exn t
  | `Down -> iround_down_exn t
;;

let iround ?(dir = `Nearest) t =
  try Some (iround_exn ~dir t) with
  | _ -> None
;;

let is_inf x =
  match classify_float x with
  | FP_infinite -> true
  | _ -> false
;;

let min_inan (x : t) y =
  if is_nan y then x else if is_nan x then y else if x < y then x else y
;;

let max_inan (x : t) y =
  if is_nan y then x else if is_nan x then y else if x > y then x else y
;;

let add = ( +. )
let sub = ( -. )
let neg = ( ~-. )
let abs = abs_float
let scale = ( *. )
let square x = x *. x

module Parts : sig
  type t

  val fractional : t -> float
  val integral : t -> float
  val modf : float -> t
end = struct
  type t = float * float

  let fractional t = fst t
  let integral t = snd t
  let modf = modf
end

let modf = Parts.modf
let round_down = floor
let round_up = ceil
let round_towards_zero t = if t >= 0. then round_down t else round_up t

(* see the comment above [round_nearest_lb] and [round_nearest_ub] for an explanation *)
let round_nearest t =
  if t > round_nearest_lb && t < round_nearest_ub
  then floor (add_half_for_round_nearest t)
  else t +. 0.
;;

let round_nearest_half_to_even t =
  if t <= round_nearest_lb || t >= round_nearest_ub
  then t +. 0.
  else (
    let floor = floor t in
    (* [ceil_or_succ = if t is an integer then t +. 1. else ceil t].  Faster than [ceil]. *)
    let ceil_or_succ = floor +. 1. in
    let diff_floor = t -. floor in
    let diff_ceil = ceil_or_succ -. t in
    if diff_floor < diff_ceil
    then floor
    else if diff_floor > diff_ceil
    then ceil_or_succ
    else if (* exact tie, pick the even *)
      mod_float floor 2. = 0.
    then floor
    else ceil_or_succ)
;;

let int63_round_lbound = lower_bound_for_int Int63.num_bits
let int63_round_ubound = upper_bound_for_int Int63.num_bits

let int63_round_up_exn t =
  if t > 0.0
  then (
    let t' = ceil t in
    if t' <= int63_round_ubound
    then Int63.of_float_unchecked t'
    else
      invalid_argf
        "Float.int63_round_up_exn: argument (%f) is too large"
        (Float0.box t)
        ())
  else if t >= int63_round_lbound
  then Int63.of_float_unchecked t
  else
    invalid_argf
      "Float.int63_round_up_exn: argument (%f) is too small or NaN"
      (Float0.box t)
      ()
;;

let int63_round_down_exn t =
  if t >= 0.0
  then
    if t <= int63_round_ubound
    then Int63.of_float_unchecked t
    else
      invalid_argf
        "Float.int63_round_down_exn: argument (%f) is too large"
        (Float0.box t)
        ()
  else (
    let t' = floor t in
    if t' >= int63_round_lbound
    then Int63.of_float_unchecked t'
    else
      invalid_argf
        "Float.int63_round_down_exn: argument (%f) is too small or NaN"
        (Float0.box t)
        ())
;;

let int63_round_nearest_portable_alloc_exn t0 =
  let t = round_nearest t0 in
  if t > 0.
  then
    if t <= int63_round_ubound
    then Int63.of_float_unchecked t
    else
      invalid_argf
        "Float.int63_round_nearest_portable_alloc_exn: argument (%f) is too large"
        (box t0)
        ()
  else if t >= int63_round_lbound
  then Int63.of_float_unchecked t
  else
    invalid_argf
      "Float.int63_round_nearest_portable_alloc_exn: argument (%f) is too small or NaN"
      (box t0)
      ()
;;

let int63_round_nearest_arch64_noalloc_exn f = Int63.of_int (iround_nearest_exn f)

let int63_round_nearest_exn =
  match Word_size.word_size with
  | W64 -> int63_round_nearest_arch64_noalloc_exn
  | W32 -> int63_round_nearest_portable_alloc_exn
;;

let round ?(dir = `Nearest) t =
  match dir with
  | `Nearest -> round_nearest t
  | `Down -> round_down t
  | `Up -> round_up t
  | `Zero -> round_towards_zero t
;;

module Class = struct
  type t =
    | Infinite
    | Nan
    | Normal
    | Subnormal
    | Zero
  [@@deriving_inline compare, enumerate, sexp]

  let compare = (Ppx_compare_lib.polymorphic_compare : t -> t -> int)
  let all = ([ Infinite; Nan; Normal; Subnormal; Zero ] : t list)

  let t_of_sexp =
    (let _tp_loc = "float.ml.Class.t" in
     function
     | Ppx_sexp_conv_lib.Sexp.Atom ("infinite" | "Infinite") -> Infinite
     | Ppx_sexp_conv_lib.Sexp.Atom ("nan" | "Nan") -> Nan
     | Ppx_sexp_conv_lib.Sexp.Atom ("normal" | "Normal") -> Normal
     | Ppx_sexp_conv_lib.Sexp.Atom ("subnormal" | "Subnormal") -> Subnormal
     | Ppx_sexp_conv_lib.Sexp.Atom ("zero" | "Zero") -> Zero
     | Ppx_sexp_conv_lib.Sexp.List
         (Ppx_sexp_conv_lib.Sexp.Atom ("infinite" | "Infinite") :: _) as sexp ->
       Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
     | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("nan" | "Nan") :: _) as
       sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
     | Ppx_sexp_conv_lib.Sexp.List
         (Ppx_sexp_conv_lib.Sexp.Atom ("normal" | "Normal") :: _) as sexp ->
       Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
     | Ppx_sexp_conv_lib.Sexp.List
         (Ppx_sexp_conv_lib.Sexp.Atom ("subnormal" | "Subnormal") :: _) as sexp ->
       Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
     | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.Atom ("zero" | "Zero") :: _)
       as sexp -> Ppx_sexp_conv_lib.Conv_error.stag_no_args _tp_loc sexp
     | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.List _ :: _) as sexp ->
       Ppx_sexp_conv_lib.Conv_error.nested_list_invalid_sum _tp_loc sexp
     | Ppx_sexp_conv_lib.Sexp.List [] as sexp ->
       Ppx_sexp_conv_lib.Conv_error.empty_list_invalid_sum _tp_loc sexp
     | sexp -> Ppx_sexp_conv_lib.Conv_error.unexpected_stag _tp_loc sexp
               : Ppx_sexp_conv_lib.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
      | Infinite -> Ppx_sexp_conv_lib.Sexp.Atom "Infinite"
      | Nan -> Ppx_sexp_conv_lib.Sexp.Atom "Nan"
      | Normal -> Ppx_sexp_conv_lib.Sexp.Atom "Normal"
      | Subnormal -> Ppx_sexp_conv_lib.Sexp.Atom "Subnormal"
      | Zero -> Ppx_sexp_conv_lib.Sexp.Atom "Zero"
                : t -> Ppx_sexp_conv_lib.Sexp.t)
  ;;

  [@@@end]

  let to_string t = string_of_sexp (sexp_of_t t)
  let of_string s = t_of_sexp (sexp_of_string s)
end

let classify t =
  let module C = Class in
  match classify_float t with
  | FP_normal -> C.Normal
  | FP_subnormal -> C.Subnormal
  | FP_zero -> C.Zero
  | FP_infinite -> C.Infinite
  | FP_nan -> C.Nan
;;

let is_finite t = not (t = infinity || t = neg_infinity || is_nan t)

let insert_underscores ?(delimiter = '_') ?(strip_zero = false) string =
  match String.lsplit2 string ~on:'.' with
  | None -> Int_conversions.insert_delimiter string ~delimiter
  | Some (left, right) ->
    let left = Int_conversions.insert_delimiter left ~delimiter in
    let right =
      if strip_zero then String.rstrip right ~drop:(fun c -> Char.( = ) c '0') else right
    in
    (match right with
     | "" -> left
     | _ -> left ^ "." ^ right)
;;

let to_string_hum ?delimiter ?(decimals = 3) ?strip_zero f =
  if Int_replace_polymorphic_compare.( < ) decimals 0
  then invalid_argf "to_string_hum: invalid argument ~decimals=%d" decimals ();
  match classify f with
  | Class.Infinite -> if f > 0. then "inf" else "-inf"
  | Class.Nan -> "nan"
  | Class.Normal | Class.Subnormal | Class.Zero ->
    insert_underscores (sprintf "%.*f" decimals f) ?delimiter ?strip_zero
;;

let sexp_of_t t =
  let sexp = sexp_of_t t in
  match !Sexp.of_float_style with
  | `No_underscores -> sexp
  | `Underscores ->
    (match sexp with
     | List _ ->
       raise_s
         (Sexp.message
            "[sexp_of_float] produced strange sexp"
            [ "sexp", Sexp.sexp_of_t sexp ])
     | Atom string ->
       if String.contains string 'E' then sexp else Atom (insert_underscores string))
;;

let to_padded_compact_string_custom t ?(prefix = "") ~kilo ~mega ~giga ~tera ?peta () =
  (* Round a ratio toward the nearest integer, resolving ties toward the nearest even
     number.  For sane inputs (in particular, when [denominator] is an integer and
     [abs numerator < 2e52]) this should be accurate.  Otherwise, the result might be a
     little bit off, but we don't really use that case. *)
  let iround_ratio_exn ~numerator ~denominator =
    let k = floor (numerator /. denominator) in
    (* if [abs k < 2e53], then both [k] and [k +. 1.] are accurately represented, and in
       particular [k +. 1. > k].  If [denominator] is also an integer, and
       [abs (denominator *. (k +. 1)) < 2e53] (and in some other cases, too), then [lower]
       and [higher] are actually both accurate.  Since (roughly)
       [numerator = denominator *. k] then for [abs numerator < 2e52] we should be
       fine. *)
    let lower = denominator *. k in
    let higher = denominator *. (k +. 1.) in
    (* Subtracting numbers within a factor of two from each other is accurate.
       So either the two subtractions below are accurate, or k = 0, or k = -1.
       In case of a tie, round to even. *)
    let diff_right = higher -. numerator in
    let diff_left = numerator -. lower in
    let k = iround_nearest_exn k in
    if diff_right < diff_left
    then k + 1
    else if diff_right > diff_left
    then k
    else if (* a tie *)
      Int_replace_polymorphic_compare.( = ) (k mod 2) 0
    then k
    else k + 1
  in
  match classify t with
  | Class.Infinite -> if t < 0.0 then "-inf  " else "inf  "
  | Class.Nan -> "nan  "
  | Class.Subnormal | Class.Normal | Class.Zero ->
    let go t =
      let conv_one t =
        assert (0. <= t && t < 999.95);
        let x = prefix ^ format_float "%.1f" t in
        (* Fix the ".0" suffix *)
        if String.is_suffix x ~suffix:".0"
        then (
          let x = Bytes.of_string x in
          let n = Bytes.length x in
          Bytes.set x (n - 1) ' ';
          Bytes.set x (n - 2) ' ';
          Bytes.unsafe_to_string ~no_mutation_while_string_reachable:x)
        else x
      in
      let conv mag t denominator =
        assert (
          (denominator = 100. && t >= 999.95)
          || (denominator >= 100_000. && t >= round_nearest (denominator *. 9.999_5)));
        assert (t < round_nearest (denominator *. 9_999.5));
        let i, d =
          let k = iround_ratio_exn ~numerator:t ~denominator in
          (* [mod] is okay here because we know i >= 0. *)
          k / 10, k mod 10
        in
        let open Int_replace_polymorphic_compare in
        assert (0 <= i && i < 1000);
        assert (0 <= d && d < 10);
        if d = 0
        then sprintf "%s%d%s " prefix i mag
        else sprintf "%s%d%s%d" prefix i mag d
      in
      (* While the standard metric prefixes (e.g. capital "M" rather than "m", [1]) are
         nominally more correct, this hinders readability in our case.  E.g., 10G6 and
         1066 look too similar.  That's an extreme example, but in general k,m,g,t,p
         probably stand out better than K,M,G,T,P when interspersed with digits.

         [1] http://en.wikipedia.org/wiki/Metric_prefix *)
      (* The trick here is that:
         - the first boundary (999.95) as a float is slightly over-represented (so it is
           better approximated as "1k" than as "999.9"),
         - the other boundaries are accurately represented, because they are integers.
           That's why the strict equalities below do exactly what we want. *)
      if t < 999.95E0
      then conv_one t
      else if t < 999.95E3
      then conv kilo t 100.
      else if t < 999.95E6
      then conv mega t 100_000.
      else if t < 999.95E9
      then conv giga t 100_000_000.
      else if t < 999.95E12
      then conv tera t 100_000_000_000.
      else (
        match peta with
        | None -> sprintf "%s%.1e" prefix t
        | Some peta ->
          if t < 999.95E15
          then conv peta t 100_000_000_000_000.
          else sprintf "%s%.1e" prefix t)
    in
    if t >= 0. then go t else "-" ^ go ~-.t
;;

let to_padded_compact_string t =
  to_padded_compact_string_custom t ~kilo:"k" ~mega:"m" ~giga:"g" ~tera:"t" ~peta:"p" ()
;;

(* Performance note: Initializing the accumulator to 1 results in one extra
   multiply; e.g., to compute x ** 4, we in principle only need 2 multiplies,
   but this function will have 3 multiplies.  However, attempts to avoid this
   (like decrementing n and initializing accum to be x, or handling small
   exponents as a special case) have not yielded anything that is a net
   improvement.
*)
let int_pow x n =
  let open Int_replace_polymorphic_compare in
  if n = 0
  then 1.
  else (
    (* Using [x +. (-0.)] on the following line convinces the compiler to avoid a certain
       boxing (that would result in allocation in each iteration).  Soon, the compiler
       shouldn't need this "hint" to avoid the boxing.  The reason we add -0 rather than 0
       is that [x +. (-0.)] is apparently always the same as [x], whereas [x +. 0.] is
       not, in that it sends [-0.] to [0.].  This makes a difference because we want
       [int_pow (-0.) (-1)] to return neg_infinity just like [-0. ** -1.] would.  *)
    let x = ref (x +. -0.) in
    let n = ref n in
    let accum = ref 1. in
    if !n < 0
    then (
      (* x ** n = (1/x) ** -n *)
      x := 1. /. !x;
      n := ~- (!n);
      if !n < 0
      then (
        (* n must have been min_int, so it is now so big that it has wrapped around.
           We decrement it so that it looks positive again, but accordingly have
           to put an extra factor of x in the accumulator.
        *)
        accum := !x;
        decr n));
    (* Letting [a] denote (the original value of) [x ** n], we maintain
       the invariant that [(x ** n) *. accum = a]. *)
    while !n > 1 do
      if !n land 1 <> 0 then accum := !x *. !accum;
      x := !x *. !x;
      n := !n lsr 1
    done;
    (* n is necessarily 1 at this point, so there is one additional
       multiplication by x. *)
    !x *. !accum)
;;

let round_gen x ~how =
  if x = 0.
  then 0.
  else if not (is_finite x)
  then x
  else (
    (* Significant digits and decimal digits. *)
    let sd, dd =
      match how with
      | `significant_digits sd ->
        let dd = sd - to_int (round_up (log10 (abs x))) in
        sd, dd
      | `decimal_digits dd ->
        let sd = dd + to_int (round_up (log10 (abs x))) in
        sd, dd
    in
    let open Int_replace_polymorphic_compare in
    if sd < 0
    then 0.
    else if sd >= 17
    then x
    else (
      (* Choose the order that is exactly representable as a float. Small positive
         integers are, but their inverses in most cases are not. *)
      let abs_dd = Int.abs dd in
      if abs_dd > 22 || sd >= 16
      (* 10**22 is exactly representable as a float, but 10**23 is not, so use the slow
         path.  Similarly, if we need 16 significant digits in the result, then the integer
         [round_nearest (x <op> order)] might not be exactly representable as a float, since
         for some ranges we only have 15 digits of precision guaranteed.

         That said, we are still rounding twice here:

         1) first time when rounding [x *. order] or [x /. order] to the nearest float
         (just the normal way floating-point multiplication or division works),

         2) second time when applying [round_nearest_half_to_even] to the result of the
         above operation

         So for arguments within an ulp from a tie we might still produce an off-by-one
         result. *)
      then of_string (sprintf "%.*g" sd x)
      else (
        let order = int_pow 10. abs_dd in
        if dd >= 0
        then round_nearest_half_to_even (x *. order) /. order
        else round_nearest_half_to_even (x /. order) *. order)))
;;

let round_significant x ~significant_digits =
  if Int_replace_polymorphic_compare.( <= ) significant_digits 0
  then
    invalid_argf
      "Float.round_significant: invalid argument significant_digits:%d"
      significant_digits
      ()
  else round_gen x ~how:(`significant_digits significant_digits)
;;

let round_decimal x ~decimal_digits = round_gen x ~how:(`decimal_digits decimal_digits)
let between t ~low ~high = low <= t && t <= high

let clamp_exn t ~min ~max =
  (* Also fails if [min] or [max] is nan *)
  assert (min <= max);
  (* clamp_unchecked is in float0.ml *)
  clamp_unchecked t ~min ~max
;;

let clamp t ~min ~max =
  (* Also fails if [min] or [max] is nan *)
  if min <= max
  then Ok (clamp_unchecked t ~min ~max)
  else
    Or_error.error_s
      (Sexp.message
         "clamp requires [min <= max]"
         [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
;;

let ( + ) = ( +. )
let ( - ) = ( -. )
let ( * ) = ( *. )
let ( ** ) = ( ** )
let ( / ) = ( /. )
let ( ~- ) = ( ~-. )

let sign_exn t : Sign.t =
  if t > 0.
  then Pos
  else if t < 0.
  then Neg
  else if t = 0.
  then Zero
  else Error.raise_s (Sexp.message "Float.sign_exn of NAN" [ "", sexp_of_t t ])
;;

let sign_or_nan t : Sign_or_nan.t =
  if t > 0. then Pos else if t < 0. then Neg else if t = 0. then Zero else Nan
;;

let ieee_negative t =
  let bits = Caml.Int64.bits_of_float t in
  Poly.(bits < Caml.Int64.zero)
;;

let exponent_bits = 11
let mantissa_bits = 52
let exponent_mask64 = Int64.(shift_left one exponent_bits - one)
let exponent_mask = Int64.to_int_exn exponent_mask64
let mantissa_mask = Int63.(shift_left one mantissa_bits - one)
let mantissa_mask64 = Int63.to_int64 mantissa_mask

let ieee_exponent t =
  let bits = Caml.Int64.bits_of_float t in
  Int64.(bit_and (shift_right_logical bits mantissa_bits) exponent_mask64)
  |> Caml.Int64.to_int
;;

let ieee_mantissa t =
  let bits = Caml.Int64.bits_of_float t in
  Int63.of_int64_exn Caml.Int64.(logand bits mantissa_mask64)
;;

let create_ieee_exn ~negative ~exponent ~mantissa =
  if Int.(bit_and exponent exponent_mask <> exponent)
  then failwithf "exponent %d out of range [0, %d]" exponent exponent_mask ()
  else if Int63.(bit_and mantissa mantissa_mask <> mantissa)
  then
    failwithf
      "mantissa %s out of range [0, %s]"
      (Int63.to_string mantissa)
      (Int63.to_string mantissa_mask)
      ()
  else (
    let sign_bits = if negative then Caml.Int64.min_int else Caml.Int64.zero in
    let expt_bits = Caml.Int64.shift_left (Caml.Int64.of_int exponent) mantissa_bits in
    let mant_bits = Int63.to_int64 mantissa in
    let bits = Caml.Int64.(logor sign_bits (logor expt_bits mant_bits)) in
    Caml.Int64.float_of_bits bits)
;;

let create_ieee ~negative ~exponent ~mantissa =
  Or_error.try_with (fun () -> create_ieee_exn ~negative ~exponent ~mantissa)
;;

module Terse = struct
  type nonrec t = t

  let t_of_sexp = t_of_sexp
  let to_string x = Printf.sprintf "%.8G" x
  let sexp_of_t x = Sexp.Atom (to_string x)
  let of_string x = of_string x
end

let validate_ordinary t =
  Validate.of_error_opt
    (let module C = Class in
     match classify t with
     | C.Normal | C.Subnormal | C.Zero -> None
     | C.Infinite -> Some "value is infinite"
     | C.Nan -> Some "value is NaN")
;;

module V = struct
  module ZZ = Comparable.Validate (T)

  let validate_bound ~min ~max t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_bound t ~min ~max)
  ;;

  let validate_lbound ~min t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_lbound t ~min)
  ;;

  let validate_ubound ~max t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_ubound t ~max)
  ;;
end

include V

include Comparable.With_zero (struct
    include T

    let zero = zero

    include V
  end)

(* These are partly here as a performance hack to avoid some boxing we're getting with
   the versions we get from [With_zero].  They also make [Float.is_negative nan] and
   [Float.is_non_positive nan] return [false]; the versions we get from [With_zero] return
   [true]. *)
let is_positive t = t > 0.
let is_non_negative t = t >= 0.
let is_negative t = t < 0.
let is_non_positive t = t <= 0.

include Pretty_printer.Register (struct
    include T

    let module_name = "Base.Float"
    let to_string = to_string
  end)

module O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  let ( ** ) = ( ** )

  include (Float_replace_polymorphic_compare : Comparisons.Infix with type t := t)

  let abs = abs
  let neg = neg
  let zero = zero
  let of_int = of_int
  let of_float x = x
end

module O_dot = struct
  let ( *. ) = ( * )
  let ( +. ) = ( + )
  let ( -. ) = ( - )
  let ( /. ) = ( / )
  let ( ~-. ) = ( ~- )
  let ( **. ) = ( ** )
end

module Private = struct
  let box = box
  let clamp_unchecked = clamp_unchecked
  let lower_bound_for_int = lower_bound_for_int
  let upper_bound_for_int = upper_bound_for_int
  let specialized_hash = hash_float
  let one_ulp_less_than_half = one_ulp_less_than_half
  let int63_round_nearest_portable_alloc_exn = int63_round_nearest_portable_alloc_exn
  let int63_round_nearest_arch64_noalloc_exn = int63_round_nearest_arch64_noalloc_exn
  let iround_nearest_exn_64 = iround_nearest_exn_64
end

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Float_replace_polymorphic_compare

(* These functions specifically replace defaults in replace_polymorphic_compare.

   The desired behavior here is to propagate a nan if either argument is nan. Because the
   first comparison will always return false if either argument is nan, it suffices to
   check if x is nan. Then, when x is nan or both x and y are nan, we return x = nan; and
   when y is nan but not x, we return y = nan.

   There are various ways to implement these functions.  The benchmark below shows a few
   different versions.  This benchmark was run over an array of random floats (none of
   which are nan).

   ┌────────────────────────────────────────────────┬──────────┐
   │ Name                                           │ Time/Run │
   ├────────────────────────────────────────────────┼──────────┤
   │ if is_nan x then x else if x < y then x else y │   2.42us │
   │ if is_nan x || x < y then x else y             │   2.02us │
   │ if x < y || is_nan x then x else y             │   1.88us │
   └────────────────────────────────────────────────┴──────────┘

   The benchmark below was run when x > y is always true (again, no nan values).

   ┌────────────────────────────────────────────────┬──────────┐
   │ Name                                           │ Time/Run │
   ├────────────────────────────────────────────────┼──────────┤
   │ if is_nan x then x else if x < y then x else y │   2.83us │
   │ if is_nan x || x < y then x else y             │   1.97us │
   │ if x < y || is_nan x then x else y             │   1.56us │
   └────────────────────────────────────────────────┴──────────┘
*)
let min (x : t) y = if x < y || is_nan x then x else y
let max (x : t) y = if x > y || is_nan x then x else y
