open! Import
include Int_intf
include Int0

module T = struct
  type t = int [@@deriving_inline hash, sexp, sexp_grammar]

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_int

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_int in
    fun x -> func x
  ;;

  let t_of_sexp = (int_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (sexp_of_int : t -> Ppx_sexp_conv_lib.Sexp.t)

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int" ]
      ; ggid = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
      ; types = [ "t", Implicit_var 0 ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "int.ml.T"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]

  let compare x y = Int_replace_polymorphic_compare.compare x y

  let of_string s =
    try of_string s with
    | _ -> Printf.failwithf "Int.of_string: %S" s ()
  ;;

  let to_string = to_string
end

let num_bits = Int_conversions.num_bits_int
let float_lower_bound = Float0.lower_bound_for_int num_bits
let float_upper_bound = Float0.upper_bound_for_int num_bits
let to_float = Caml.float_of_int
let of_float_unchecked = Caml.int_of_float

let of_float f =
  if Float_replace_polymorphic_compare.( >= ) f float_lower_bound
  && Float_replace_polymorphic_compare.( <= ) f float_upper_bound
  then Caml.int_of_float f
  else
    Printf.invalid_argf
      "Int.of_float: argument (%f) is out of range or NaN"
      (Float0.box f)
      ()
;;

let zero = 0
let one = 1
let minus_one = -1

include T
include Comparator.Make (T)

include Comparable.Validate_with_zero (struct
    include T

    let zero = zero
  end)

module Conv = Int_conversions
include Conv.Make (T)

include Conv.Make_hex (struct
    open Int_replace_polymorphic_compare

    type t = int [@@deriving_inline compare, hash]

    let compare = (compare_int : t -> t -> int)

    let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
      hash_fold_int

    and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
      let func = hash_int in
      fun x -> func x
    ;;

    [@@@end]

    let zero = zero
    let neg = ( ~- )
    let ( < ) = ( < )
    let to_string i = Printf.sprintf "%x" i
    let of_string s = Caml.Scanf.sscanf s "%x" Fn.id
    let module_name = "Base.Int.Hex"
  end)

include Pretty_printer.Register (struct
    type nonrec t = t

    let to_string = to_string
    let module_name = "Base.Int"
  end)

(* Open replace_polymorphic_compare after including functor instantiations so
   they do not shadow its definitions. This is here so that efficient versions
   of the comparison functions are available within this module. *)
open! Int_replace_polymorphic_compare

let invariant (_ : t) = ()
let between t ~low ~high = low <= t && t <= high
let clamp_unchecked t ~min ~max = if t < min then min else if t <= max then t else max

let clamp_exn t ~min ~max =
  assert (min <= max);
  clamp_unchecked t ~min ~max
;;

let clamp t ~min ~max =
  if min > max
  then
    Or_error.error_s
      (Sexp.message
         "clamp requires [min <= max]"
         [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
  else Ok (clamp_unchecked t ~min ~max)
;;

let pred i = i - 1
let succ i = i + 1
let to_int i = i
let to_int_exn = to_int
let of_int i = i
let of_int_exn = of_int
let max_value = Caml.max_int
let min_value = Caml.min_int
let max_value_30_bits = 0x3FFF_FFFF
let of_int32 = Conv.int32_to_int
let of_int32_exn = Conv.int32_to_int_exn
let of_int32_trunc = Conv.int32_to_int_trunc
let to_int32 = Conv.int_to_int32
let to_int32_exn = Conv.int_to_int32_exn
let to_int32_trunc = Conv.int_to_int32_trunc
let of_int64 = Conv.int64_to_int
let of_int64_exn = Conv.int64_to_int_exn
let of_int64_trunc = Conv.int64_to_int_trunc
let to_int64 = Conv.int_to_int64
let of_nativeint = Conv.nativeint_to_int
let of_nativeint_exn = Conv.nativeint_to_int_exn
let of_nativeint_trunc = Conv.nativeint_to_int_trunc
let to_nativeint = Conv.int_to_nativeint
let to_nativeint_exn = to_nativeint
let abs x = abs x
let ( + ) x y = x + y
let ( - ) x y = x - y
let ( * ) x y = x * y
let ( / ) x y = x / y
let neg x = -x
let ( ~- ) = neg

(* note that rem is not same as % *)
let rem a b = a mod b
let incr = Caml.incr
let decr = Caml.decr
let shift_right a b = a asr b
let shift_right_logical a b = a lsr b
let shift_left a b = a lsl b
let bit_not a = lnot a
let bit_or a b = a lor b
let bit_and a b = a land b
let bit_xor a b = a lxor b
let pow = Int_math.Private.int_pow
let ( ** ) b e = pow b e

module Pow2 = struct
  open! Import
  module Sys = Sys0

  let raise_s = Error.raise_s

  let non_positive_argument () =
    Printf.invalid_argf "argument must be strictly positive" ()
  ;;


  (** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
  let ceil_pow2 x =
    if x <= 0 then non_positive_argument ();
    let x = x - 1 in
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    (* The next line is superfluous on 32-bit architectures, but it's faster to do it
       anyway than to branch *)
    let x = x lor (x lsr 32) in
    x + 1
  ;;

  (** "floor power of 2" - Largest power of 2 less than or equal to x. *)
  let floor_pow2 x =
    if x <= 0 then non_positive_argument ();
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    (* The next line is superfluous on 32-bit architectures, but it's faster to do it
       anyway than to branch *)
    let x = x lor (x lsr 32) in
    x - (x lsr 1)
  ;;

  let is_pow2 x =
    if x <= 0 then non_positive_argument ();
    x land (x - 1) = 0
  ;;

  (* C stubs for int clz and ctz to use the CLZ/BSR/CTZ/BSF instruction where possible *)
  external clz
    :  (* Note that we pass the tagged int here. See int_math_stubs.c for details on why
          this is correct. *)
    int
    -> (int[@untagged])
    = "Base_int_math_int_clz" "Base_int_math_int_clz_untagged"
  [@@noalloc]

  external ctz
    :  (int[@untagged])
    -> (int[@untagged])
    = "Base_int_math_int_ctz" "Base_int_math_int_ctz_untagged"
  [@@noalloc]

  (** Hacker's Delight Second Edition p106 *)
  let floor_log2 i =
    if i <= 0
    then
      raise_s (Sexp.message "[Int.floor_log2] got invalid input" [ "", sexp_of_int i ]);
    num_bits - 1 - clz i
  ;;

  let ceil_log2 i =
    if i <= 0
    then raise_s (Sexp.message "[Int.ceil_log2] got invalid input" [ "", sexp_of_int i ]);
    if i = 1 then 0 else num_bits - clz (i - 1)
  ;;
end

include Pow2

(* This is already defined by Comparable.Validate_with_zero, but Sign.of_int is
   more direct. *)
let sign = Sign.of_int
let popcount = Popcount.int_popcount

module Pre_O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  let ( ** ) = ( ** )

  include (Int_replace_polymorphic_compare : Comparisons.Infix with type t := t)

  let abs = abs
  let neg = neg
  let zero = zero
  let of_int_exn = of_int_exn
end

module O = struct
  include Pre_O

  module F = Int_math.Make (struct
      type nonrec t = t

      include Pre_O

      let rem = rem
      let to_float = to_float
      let of_float = of_float
      let of_string = T.of_string
      let to_string = T.to_string
    end)

  include F

  external bswap16 : int -> int = "%bswap16"

  (* These inlined versions of (%), (/%), and (//) perform better than their functorized
     counterparts in [F] (see benchmarks below).

     The reason these functions are inlined in [Int] but not in any of the other integer
     modules is that they existed in [Int] and [Int] alone prior to the introduction of
     the [Int_math.Make] functor, and we didn't want to degrade their performance.

     We won't pre-emptively do the same for new functions, unless someone cares, on a case
     by case fashion.  *)

  let ( % ) x y =
    if y <= zero
    then
      Printf.invalid_argf
        "%s %% %s in core_int.ml: modulus should be positive"
        (to_string x)
        (to_string y)
        ();
    let rval = rem x y in
    if rval < zero then rval + y else rval
  ;;

  let ( /% ) x y =
    if y <= zero
    then
      Printf.invalid_argf
        "%s /%% %s in core_int.ml: divisor should be positive"
        (to_string x)
        (to_string y)
        ();
    if x < zero then ((x + one) / y) - one else x / y
  ;;

  let ( // ) x y = to_float x /. to_float y
  let ( land ) = ( land )
  let ( lor ) = ( lor )
  let ( lxor ) = ( lxor )
  let lnot = lnot
  let ( lsl ) = ( lsl )
  let ( asr ) = ( asr )
  let ( lsr ) = ( lsr )
end

include O

(* [Int] and [Int.O] agree value-wise *)

module Private = struct
  module O_F = O.F
end

(* Include type-specific [Replace_polymorphic_compare] at the end, after including functor
   application that could shadow its definitions. This is here so that efficient versions
   of the comparison functions are exported by this module. *)
include Int_replace_polymorphic_compare
