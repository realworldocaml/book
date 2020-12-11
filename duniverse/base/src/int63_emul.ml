(* A 63bit integer is a 64bit integer with its bits shifted to the left
   and its lowest bit set to 0.
   This is the same kind of encoding as OCaml int on 64bit architecture.
   The only difference being the lowest bit (immediate bit) set to 1. *)

open! Import
include Int64_replace_polymorphic_compare


module T0 = struct
  module T = struct
    type t = int64 [@@deriving_inline compare, hash, sexp, sexp_grammar]

    let compare = (compare_int64 : t -> t -> int)

    let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
      hash_fold_int64

    and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
      let func = hash_int64 in
      fun x -> func x
    ;;

    let t_of_sexp = (int64_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
    let sexp_of_t = (sexp_of_int64 : t -> Ppx_sexp_conv_lib.Sexp.t)

    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group)
        =
        { implicit_vars = [ "int64" ]
        ; ggid = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
        ; types = [ "t", Implicit_var 0 ]
        }
      in
      let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
        { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
        ; apply_implicit = [ int64_sexp_grammar ]
        ; generic_group = _the_generic_group
        ; origin = "int63_emul.ml.T0.T"
        }
      in
      let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
        Ref ("t", _the_group)
      in
      t_sexp_grammar
    ;;

    [@@@end]
  end

  include T
  include Comparator.Make (T)
end

module Conv = Int_conversions

module W : sig

  type t = int64

  include module type of struct
    include T0
  end
  with type t := t

  val wrap_exn : Caml.Int64.t -> t
  val wrap_modulo : Caml.Int64.t -> t
  val unwrap : t -> Caml.Int64.t

  (** Returns a non-negative int64 that is equal to the input int63 modulo 2^63. *)
  val unwrap_unsigned : t -> Caml.Int64.t

  val invariant : t -> unit
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val abs : t -> t
  val succ : t -> t
  val pred : t -> t
  val mul : t -> t -> t
  val pow : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val popcount : t -> int
  val bit_not : t -> t
  val bit_xor : t -> t -> t
  val bit_or : t -> t -> t
  val bit_and : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val min_value : t
  val max_value : t
  val to_int64 : t -> Caml.Int64.t
  val of_int64 : Caml.Int64.t -> t option
  val of_int64_exn : Caml.Int64.t -> t
  val of_int64_trunc : Caml.Int64.t -> t
  val compare : t -> t -> int
  val ceil_pow2 : t -> t
  val floor_pow2 : t -> t
  val ceil_log2 : t -> int
  val floor_log2 : t -> int
  val is_pow2 : t -> bool
  val clz : t -> int
  val ctz : t -> int
end = struct
  type t = int64

  include (
    T0 :
      module type of struct
      include T0
    end
    with type t := t)

  let wrap_exn x =
    (* Raises if the int64 value does not fit on int63. *)
    Conv.int64_fit_on_int63_exn x;
    Caml.Int64.mul x 2L
  ;;

  let wrap x =
    if Conv.int64_is_representable_as_int63 x then Some (Caml.Int64.mul x 2L) else None
  ;;

  let wrap_modulo x = Caml.Int64.mul x 2L
  let unwrap x = Caml.Int64.shift_right x 1
  let unwrap_unsigned x = Caml.Int64.shift_right_logical x 1

  (* This does not use wrap or unwrap to avoid generating exceptions in the case of
     overflows. This is to preserve the semantics of int type on 64 bit architecture. *)
  let f2 f a b =
    Caml.Int64.mul (f (Caml.Int64.shift_right a 1) (Caml.Int64.shift_right b 1)) 2L
  ;;

  let mask = 0xffff_ffff_ffff_fffeL
  let m x = Caml.Int64.logand x mask
  let invariant t = assert (m t = t)
  let add x y = Caml.Int64.add x y
  let sub x y = Caml.Int64.sub x y
  let neg x = Caml.Int64.neg x
  let abs x = Caml.Int64.abs x
  let one = wrap_exn 1L
  let succ a = add a one
  let pred a = sub a one
  let min_value = m Caml.Int64.min_int
  let max_value = m Caml.Int64.max_int
  let bit_not x = m (Caml.Int64.lognot x)
  let bit_and = Caml.Int64.logand
  let bit_xor = Caml.Int64.logxor
  let bit_or = Caml.Int64.logor
  let shift_left x i = Caml.Int64.shift_left x i
  let shift_right x i = m (Caml.Int64.shift_right x i)
  let shift_right_logical x i = m (Caml.Int64.shift_right_logical x i)
  let pow = f2 Int_math.Private.int63_pow_on_int64
  let mul a b = Caml.Int64.mul a (Caml.Int64.shift_right b 1)
  let div a b = wrap_modulo (Caml.Int64.div a b)
  let rem a b = Caml.Int64.rem a b
  let popcount x = Popcount.int64_popcount x
  let to_int64 t = unwrap t
  let of_int64 t = wrap t
  let of_int64_exn t = wrap_exn t
  let of_int64_trunc t = wrap_modulo t
  let t_of_sexp x = wrap_exn (int64_of_sexp x)
  let sexp_of_t x = sexp_of_int64 (unwrap x)
  let compare (x : t) y = compare x y
  let is_pow2 x = Int64.is_pow2 (unwrap x)

  let clz x =
    (* We run Int64.clz directly on the wrapped int63 value. This is correct because the
       bits of the int63_emul are left-aligned in the Int64. *)
    Int64.clz x
  ;;

  let ctz x = Int64.ctz (unwrap x)
  let floor_pow2 x = Int64.floor_pow2 (unwrap x) |> wrap_exn
  let ceil_pow2 x = Int64.floor_pow2 (unwrap x) |> wrap_exn
  let floor_log2 x = Int64.floor_log2 (unwrap x)
  let ceil_log2 x = Int64.ceil_log2 (unwrap x)
end

open W

module T = struct
  type t = W.t [@@deriving_inline hash, sexp, sexp_grammar]

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    W.hash_fold_t

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = W.hash in
    fun x -> func x
  ;;

  let t_of_sexp = (W.t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (W.sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t)

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "W.t" ]
      ; ggid = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
      ; types = [ "t", Implicit_var 0 ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ W.t_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "int63_emul.ml.T"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]

  type comparator_witness = W.comparator_witness

  let comparator = W.comparator
  let compare = W.compare
  let invariant = W.invariant

  (* We don't expect [hash] to follow the behavior of int in 64bit architecture *)
  let _ = hash
  let hash (x : t) = Caml.Hashtbl.hash x
  let invalid_str x = Printf.failwithf "Int63.of_string: invalid input %S" x ()

  (*
     "sign" refers to whether the number starts with a '-'
     "signedness = false" means the rest of the number is parsed as unsigned and then cast
     to signed with wrap-around modulo 2^i
     "signedness = true" means no such craziness happens

     The terminology and the logic is due to the code in byterun/ints.c in ocaml 4.03
     ([parse_sign_and_base] function).

     Signedness equals true for plain decimal number (e.g. 1235, -6789)

     Signedness equals false in the following cases:
     - [0xffff], [-0xffff] (hexadecimal representation)
     - [0b0101], [-0b0101] (binary representation)
     - [0o1237], [-0o1237] (octal representation)
     - [0u9812], [-0u9812] (unsigned decimal representation - available from OCaml 4.03) *)
  let sign_and_signedness x =
    let len = String.length x in
    let open Int_replace_polymorphic_compare in
    let pos, sign =
      if 0 < len
      then (
        match x.[0] with
        | '-' -> 1, `Neg
        | '+' -> 1, `Pos
        | _ -> 0, `Pos)
      else 0, `Pos
    in
    if pos + 2 < len
    then (
      let c1 = x.[pos] in
      let c2 = x.[pos + 1] in
      match c1, c2 with
      | '0', '0' .. '9' -> sign, true
      | '0', _ -> sign, false
      | _ -> sign, true)
    else sign, true
  ;;

  let to_string x = Caml.Int64.to_string (unwrap x)

  let of_string str =
    try
      let sign, signedness = sign_and_signedness str in
      if signedness
      then of_int64_exn (Caml.Int64.of_string str)
      else (
        let pos_str =
          match sign with
          | `Neg -> String.sub str ~pos:1 ~len:(String.length str - 1)
          | `Pos -> str
        in
        let int64 = Caml.Int64.of_string pos_str in
        (* unsigned 63-bit int must parse as a positive signed 64-bit int *)
        if Int64_replace_polymorphic_compare.( < ) int64 0L then invalid_str str;
        let int63 = wrap_modulo int64 in
        match sign with
        | `Neg -> neg int63
        | `Pos -> int63)
    with
    | _ -> invalid_str str
  ;;

  let bswap16 t = wrap_modulo (Int64.bswap16 (unwrap t))
  let bswap32 t = wrap_modulo (Int64.bswap32 (unwrap t))
  let bswap48 t = wrap_modulo (Int64.bswap48 (unwrap t))
end

include T

let num_bits = 63
let float_lower_bound = Float0.lower_bound_for_int num_bits
let float_upper_bound = Float0.upper_bound_for_int num_bits
let shift_right_logical = shift_right_logical
let shift_right = shift_right
let shift_left = shift_left
let bit_not = bit_not
let bit_xor = bit_xor
let bit_or = bit_or
let bit_and = bit_and
let popcount = popcount
let abs = abs
let pred = pred
let succ = succ
let pow = pow
let rem = rem
let neg = neg
let max_value = max_value
let min_value = min_value
let minus_one = wrap_exn Caml.Int64.minus_one
let one = wrap_exn Caml.Int64.one
let zero = wrap_exn Caml.Int64.zero
let is_pow2 = is_pow2
let floor_pow2 = floor_pow2
let ceil_pow2 = ceil_pow2
let floor_log2 = floor_log2
let ceil_log2 = ceil_log2
let clz = clz
let ctz = ctz
let to_float x = Caml.Int64.to_float (unwrap x)
let of_float_unchecked x = wrap_modulo (Caml.Int64.of_float x)

let of_float t =
  let open Float_replace_polymorphic_compare in
  if t >= float_lower_bound && t <= float_upper_bound
  then wrap_modulo (Caml.Int64.of_float t)
  else
    Printf.invalid_argf
      "Int63.of_float: argument (%f) is out of range or NaN"
      (Float0.box t)
      ()
;;

let of_int64 = of_int64
let of_int64_exn = of_int64_exn
let of_int64_trunc = of_int64_trunc
let to_int64 = to_int64

include Comparable.Validate_with_zero (struct
    include T

    let zero = zero
  end)

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
let ( ** ) b e = pow b e
let incr r = r := !r + one
let decr r = r := !r - one

(* We can reuse conversion function from/to int64 here. *)
let of_int x = wrap_exn (Conv.int_to_int64 x)
let of_int_exn x = of_int x
let to_int x = Conv.int64_to_int (unwrap x)
let to_int_exn x = Conv.int64_to_int_exn (unwrap x)
let to_int_trunc x = Conv.int64_to_int_trunc (unwrap x)
let of_int32 x = wrap_exn (Conv.int32_to_int64 x)
let of_int32_exn x = of_int32 x
let to_int32 x = Conv.int64_to_int32 (unwrap x)
let to_int32_exn x = Conv.int64_to_int32_exn (unwrap x)
let to_int32_trunc x = Conv.int64_to_int32_trunc (unwrap x)
let of_nativeint x = of_int64 (Conv.nativeint_to_int64 x)
let of_nativeint_exn x = wrap_exn (Conv.nativeint_to_int64 x)
let of_nativeint_trunc x = of_int64_trunc (Conv.nativeint_to_int64 x)
let to_nativeint x = Conv.int64_to_nativeint (unwrap x)
let to_nativeint_exn x = Conv.int64_to_nativeint_exn (unwrap x)
let to_nativeint_trunc x = Conv.int64_to_nativeint_trunc (unwrap x)

include Conv.Make (T)

include Conv.Make_hex (struct
    type t = T.t [@@deriving_inline compare, hash]

    let compare = (T.compare : t -> t -> int)

    let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
      T.hash_fold_t

    and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
      let func = T.hash in
      fun x -> func x
    ;;

    [@@@end]

    let zero = zero
    let neg = ( ~- )
    let ( < ) = ( < )

    let to_string i =
      (* the use of [unwrap_unsigned] here is important for the case of [min_value] *)
      Printf.sprintf "%Lx" (unwrap_unsigned i)
    ;;

    let of_string s = of_string ("0x" ^ s)
    let module_name = "Base.Int63.Hex"
  end)

include Pretty_printer.Register (struct
    type nonrec t = t

    let to_string x = to_string x
    let module_name = "Base.Int63"
  end)

module Pre_O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  let ( ** ) = ( ** )

  include (Int64_replace_polymorphic_compare : Comparisons.Infix with type t := t)

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

(* [Int63] and [Int63.O] agree value-wise *)

module Repr = struct
  type emulated = t

  type ('underlying_type, 'intermediate_type) t =
    | Int : (int, int) t
    | Int64 : (int64, emulated) t
end

let repr = Repr.Int64

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Int64_replace_polymorphic_compare
