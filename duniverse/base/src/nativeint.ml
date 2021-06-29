open! Import
open! Caml.Nativeint
include Nativeint_replace_polymorphic_compare

module T = struct
  type t = nativeint [@@deriving_inline hash, sexp, sexp_grammar]

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_nativeint

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_nativeint in
    fun x -> func x
  ;;

  let t_of_sexp = (nativeint_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (sexp_of_nativeint : t -> Ppx_sexp_conv_lib.Sexp.t)

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "nativeint" ]
      ; ggid = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
      ; types = [ "t", Implicit_var 0 ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ nativeint_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "nativeint.ml.T"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]

  let compare = Nativeint_replace_polymorphic_compare.compare
  let to_string = to_string
  let of_string = of_string
end

include T
include Comparator.Make (T)

include Comparable.Validate_with_zero (struct
    include T

    let zero = zero
  end)

module Conv = Int_conversions
include Conv.Make (T)

include Conv.Make_hex (struct
    open Nativeint_replace_polymorphic_compare

    type t = nativeint [@@deriving_inline compare, hash]

    let compare = (compare_nativeint : t -> t -> int)

    let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
      hash_fold_nativeint

    and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
      let func = hash_nativeint in
      fun x -> func x
    ;;

    [@@@end]

    let zero = zero
    let neg = neg
    let ( < ) = ( < )
    let to_string i = Printf.sprintf "%nx" i
    let of_string s = Caml.Scanf.sscanf s "%nx" Fn.id
    let module_name = "Base.Nativeint.Hex"
  end)

include Pretty_printer.Register (struct
    type nonrec t = t

    let to_string = to_string
    let module_name = "Base.Nativeint"
  end)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Nativeint_replace_polymorphic_compare

let invariant (_ : t) = ()
let num_bits = Word_size.num_bits Word_size.word_size
let float_lower_bound = Float0.lower_bound_for_int num_bits
let float_upper_bound = Float0.upper_bound_for_int num_bits
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
let to_float = to_float
let of_float_unchecked = of_float

let of_float f =
  if Float_replace_polymorphic_compare.( >= ) f float_lower_bound
  && Float_replace_polymorphic_compare.( <= ) f float_upper_bound
  then of_float f
  else
    Printf.invalid_argf
      "Nativeint.of_float: argument (%f) is out of range or NaN"
      (Float0.box f)
      ()
;;

module Pow2 = struct
  open! Import
  open Nativeint_replace_polymorphic_compare
  module Sys = Sys0

  let raise_s = Error.raise_s

  let non_positive_argument () =
    Printf.invalid_argf "argument must be strictly positive" ()
  ;;

  let ( lor ) = Caml.Nativeint.logor
  let ( lsr ) = Caml.Nativeint.shift_right_logical
  let ( land ) = Caml.Nativeint.logand

  (** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
  let ceil_pow2 (x : nativeint) =
    if x <= 0n then non_positive_argument ();
    let x = Caml.Nativeint.pred x in
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    (* The next line is superfluous on 32-bit architectures, but it's faster to do it
       anyway than to branch *)
    let x = x lor (x lsr 32) in
    Caml.Nativeint.succ x
  ;;

  (** "floor power of 2" - Largest power of 2 less than or equal to x. *)
  let floor_pow2 x =
    if x <= 0n then non_positive_argument ();
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    let x = x lor (x lsr 32) in
    Caml.Nativeint.sub x (x lsr 1)
  ;;

  let is_pow2 x =
    if x <= 0n then non_positive_argument ();
    x land Caml.Nativeint.pred x = 0n
  ;;

  (* C stubs for nativeint clz and ctz to use the CLZ/BSR/CTZ/BSF instruction where possible *)
  external clz
    :  (nativeint[@unboxed])
    -> (int[@untagged])
    = "Base_int_math_nativeint_clz" "Base_int_math_nativeint_clz_unboxed"
  [@@noalloc]

  external ctz
    :  (nativeint[@unboxed])
    -> (int[@untagged])
    = "Base_int_math_nativeint_ctz" "Base_int_math_nativeint_ctz_unboxed"
  [@@noalloc]

  (** Hacker's Delight Second Edition p106 *)
  let floor_log2 i =
    if Poly.( <= ) i Caml.Nativeint.zero
    then
      raise_s
        (Sexp.message
           "[Nativeint.floor_log2] got invalid input"
           [ "", sexp_of_nativeint i ]);
    num_bits - 1 - clz i
  ;;

  (** Hacker's Delight Second Edition p106 *)
  let ceil_log2 i =
    if Poly.( <= ) i Caml.Nativeint.zero
    then
      raise_s
        (Sexp.message
           "[Nativeint.ceil_log2] got invalid input"
           [ "", sexp_of_nativeint i ]);
    if Caml.Nativeint.equal i Caml.Nativeint.one
    then 0
    else num_bits - clz (Caml.Nativeint.pred i)
  ;;
end

include Pow2

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

let ( / ) = div
let ( * ) = mul
let ( - ) = sub
let ( + ) = add
let ( ~- ) = neg
let incr r = r := !r + one
let decr r = r := !r - one
let of_nativeint t = t
let of_nativeint_exn = of_nativeint
let to_nativeint t = t
let to_nativeint_exn = to_nativeint
let popcount = Popcount.nativeint_popcount
let of_int = Conv.int_to_nativeint
let of_int_exn = of_int
let to_int = Conv.nativeint_to_int
let to_int_exn = Conv.nativeint_to_int_exn
let to_int_trunc = Conv.nativeint_to_int_trunc
let of_int32 = Conv.int32_to_nativeint
let of_int32_exn = of_int32
let to_int32 = Conv.nativeint_to_int32
let to_int32_exn = Conv.nativeint_to_int32_exn
let to_int32_trunc = Conv.nativeint_to_int32_trunc
let of_int64 = Conv.int64_to_nativeint
let of_int64_exn = Conv.int64_to_nativeint_exn
let of_int64_trunc = Conv.int64_to_nativeint_trunc
let to_int64 = Conv.nativeint_to_int64
let pow b e = of_int_exn (Int_math.Private.int_pow (to_int_exn b) (to_int_exn e))
let ( ** ) b e = pow b e

module Pre_O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  let ( ** ) = ( ** )

  include (Nativeint_replace_polymorphic_compare : Comparisons.Infix with type t := t)

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

(* [Nativeint] and [Nativeint.O] agree value-wise *)

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Nativeint_replace_polymorphic_compare

external bswap : t -> t = "%bswap_native"
