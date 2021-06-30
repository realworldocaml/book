open! Import
open! Caml.Int32

module T = struct
  type t = int32 [@@deriving_inline hash, sexp, sexp_grammar]

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_int32

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_int32 in
    fun x -> func x
  ;;

  let t_of_sexp = (int32_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (sexp_of_int32 : t -> Ppx_sexp_conv_lib.Sexp.t)

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "int32" ]
      ; ggid = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
      ; types = [ "t", Implicit_var 0 ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ int32_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "int32.ml.T"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]

  let compare (x : t) y = compare x y
  let to_string = to_string
  let of_string = of_string
end

include T
include Comparator.Make (T)

let num_bits = 32
let float_lower_bound = Float0.lower_bound_for_int num_bits
let float_upper_bound = Float0.upper_bound_for_int num_bits
let float_of_bits = float_of_bits
let bits_of_float = bits_of_float
let shift_right_logical = shift_right_logical
let shift_right = shift_right
let shift_left = shift_left
let bit_not = lognot
let bit_xor = logxor
let bit_or = logor
let bit_and = logand
let min_value = min_int
let max_value = max_int
let abs = abs
let pred = pred
let succ = succ
let rem = rem
let neg = neg
let minus_one = minus_one
let one = one
let zero = zero
let compare = compare
let to_float = to_float
let of_float_unchecked = of_float

let of_float f =
  if Float_replace_polymorphic_compare.( >= ) f float_lower_bound
  && Float_replace_polymorphic_compare.( <= ) f float_upper_bound
  then of_float f
  else
    Printf.invalid_argf
      "Int32.of_float: argument (%f) is out of range or NaN"
      (Float0.box f)
      ()
;;

include Comparable.Validate_with_zero (struct
    include T

    let zero = zero
  end)

module Infix_compare = struct
  open Poly

  let ( >= ) (x : t) y = x >= y
  let ( <= ) (x : t) y = x <= y
  let ( = ) (x : t) y = x = y
  let ( > ) (x : t) y = x > y
  let ( < ) (x : t) y = x < y
  let ( <> ) (x : t) y = x <> y
end

module Compare = struct
  include Infix_compare

  let compare = compare
  let ascending = compare
  let descending x y = compare y x
  let min (x : t) y = if x < y then x else y
  let max (x : t) y = if x > y then x else y
  let equal (x : t) y = x = y
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
end

include Compare

let invariant (_ : t) = ()
let ( / ) = div
let ( * ) = mul
let ( - ) = sub
let ( + ) = add
let ( ~- ) = neg
let incr r = r := !r + one
let decr r = r := !r - one
let of_int32 t = t
let of_int32_exn = of_int32
let to_int32 t = t
let to_int32_exn = to_int32
let popcount = Popcount.int32_popcount

module Conv = Int_conversions

let of_int = Conv.int_to_int32
let of_int_exn = Conv.int_to_int32_exn
let of_int_trunc = Conv.int_to_int32_trunc
let to_int = Conv.int32_to_int
let to_int_exn = Conv.int32_to_int_exn
let to_int_trunc = Conv.int32_to_int_trunc
let of_int64 = Conv.int64_to_int32
let of_int64_exn = Conv.int64_to_int32_exn
let of_int64_trunc = Conv.int64_to_int32_trunc
let to_int64 = Conv.int32_to_int64
let of_nativeint = Conv.nativeint_to_int32
let of_nativeint_exn = Conv.nativeint_to_int32_exn
let of_nativeint_trunc = Conv.nativeint_to_int32_trunc
let to_nativeint = Conv.int32_to_nativeint
let to_nativeint_exn = to_nativeint
let pow b e = of_int_exn (Int_math.Private.int_pow (to_int_exn b) (to_int_exn e))
let ( ** ) b e = pow b e

external bswap32 : t -> t = "%bswap_int32"

let bswap16 x = Caml.Int32.shift_right_logical (bswap32 x) 16

module Pow2 = struct
  open! Import
  open Int32_replace_polymorphic_compare
  module Sys = Sys0

  let raise_s = Error.raise_s

  let non_positive_argument () =
    Printf.invalid_argf "argument must be strictly positive" ()
  ;;

  let ( lor ) = Caml.Int32.logor
  let ( lsr ) = Caml.Int32.shift_right_logical
  let ( land ) = Caml.Int32.logand

  (** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
  let ceil_pow2 x =
    if x <= Caml.Int32.zero then non_positive_argument ();
    let x = Caml.Int32.pred x in
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    Caml.Int32.succ x
  ;;

  (** "floor power of 2" - Largest power of 2 less than or equal to x. *)
  let floor_pow2 x =
    if x <= Caml.Int32.zero then non_positive_argument ();
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    Caml.Int32.sub x (x lsr 1)
  ;;

  let is_pow2 x =
    if x <= Caml.Int32.zero then non_positive_argument ();
    x land Caml.Int32.pred x = Caml.Int32.zero
  ;;

  (* C stubs for int32 clz and ctz to use the CLZ/BSR/CTZ/BSF instruction where possible *)
  external clz
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "Base_int_math_int32_clz" "Base_int_math_int32_clz_unboxed"
  [@@noalloc]

  external ctz
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "Base_int_math_int32_ctz" "Base_int_math_int32_ctz_unboxed"
  [@@noalloc]

  (** Hacker's Delight Second Edition p106 *)
  let floor_log2 i =
    if i <= Caml.Int32.zero
    then
      raise_s
        (Sexp.message "[Int32.floor_log2] got invalid input" [ "", sexp_of_int32 i ]);
    num_bits - 1 - clz i
  ;;

  (** Hacker's Delight Second Edition p106 *)
  let ceil_log2 i =
    if i <= Caml.Int32.zero
    then
      raise_s
        (Sexp.message "[Int32.ceil_log2] got invalid input" [ "", sexp_of_int32 i ]);
    (* The [i = 1] check is needed because clz(0) is undefined *)
    if Caml.Int32.equal i Caml.Int32.one then 0 else num_bits - clz (Caml.Int32.pred i)
  ;;
end

include Pow2
include Conv.Make (T)

include Conv.Make_hex (struct
    type t = int32 [@@deriving_inline compare, hash]

    let compare = (compare_int32 : t -> t -> int)

    let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
      hash_fold_int32

    and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
      let func = hash_int32 in
      fun x -> func x
    ;;

    [@@@end]

    let zero = zero
    let neg = ( ~- )
    let ( < ) = ( < )
    let to_string i = Printf.sprintf "%lx" i
    let of_string s = Caml.Scanf.sscanf s "%lx" Fn.id
    let module_name = "Base.Int32.Hex"
  end)

include Pretty_printer.Register (struct
    type nonrec t = t

    let to_string = to_string
    let module_name = "Base.Int32"
  end)

module Pre_O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  let ( ** ) = ( ** )

  include (Compare : Comparisons.Infix with type t := t)

  let abs = abs
  let neg = neg
  let zero = zero
  let of_int_exn = of_int_exn
end

module O = struct
  include Pre_O

  include Int_math.Make (struct
      type nonrec t = t

      include Pre_O

      let rem = rem
      let to_float = to_float
      let of_float = of_float
      let of_string = T.of_string
      let to_string = T.to_string
    end)

  let ( land ) = bit_and
  let ( lor ) = bit_or
  let ( lxor ) = bit_xor
  let lnot = bit_not
  let ( lsl ) = shift_left
  let ( asr ) = shift_right
  let ( lsr ) = shift_right_logical
end

include O

(* [Int32] and [Int32.O] agree value-wise *)
