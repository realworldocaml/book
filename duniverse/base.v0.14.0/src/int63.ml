open! Import

let raise_s = Error.raise_s

module Repr = Int63_emul.Repr

(* In a world where the compiler would understand [@@immediate64] attributes on type
   declarations, this module is how one would produce a [type t] with this attribute. *)
module Immediate64 : sig
  module type Non_immediate = sig
    type t
  end

  module type Immediate = sig
    type t [@@immediate]
  end

  module Make (Immediate : Immediate) (Non_immediate : Non_immediate) : sig
    type t [@@immediate64]

    type 'a repr =
      | Immediate : Immediate.t repr
      | Non_immediate : Non_immediate.t repr

    val repr : t repr
  end
end = struct
  module type Non_immediate = sig
    type t
  end

  module type Immediate = sig
    type t [@@immediate]
  end

  module Make (Immediate : Immediate) (Non_immediate : Non_immediate) = struct
    type t [@@immediate64]

    type 'a repr =
      | Immediate : Immediate.t repr
      | Non_immediate : Non_immediate.t repr

    let repr =
      match Word_size.word_size with
      | W64 -> (Caml.Obj.magic Immediate : t repr)
      | W32 -> (Caml.Obj.magic Non_immediate : t repr)
    ;;
  end
end

include Immediate64.Make (Int) (Int63_emul)

module Backend = struct
  module type S = sig
    type t

    include Int_intf.S with type t := t

    val of_int : int -> t
    val to_int : t -> int option
    val to_int_trunc : t -> int
    val of_int32 : int32 -> t
    val to_int32 : t -> Int32.t option
    val to_int32_trunc : t -> Int32.t
    val of_int64 : Int64.t -> t option
    val of_int64_trunc : Int64.t -> t
    val of_nativeint : nativeint -> t option
    val to_nativeint : t -> nativeint option
    val of_nativeint_trunc : nativeint -> t
    val to_nativeint_trunc : t -> nativeint
    val of_float_unchecked : float -> t
    val repr : (t, t) Int63_emul.Repr.t
    val bswap16 : t -> t
    val bswap32 : t -> t
    val bswap48 : t -> t
  end
  with type t := t

  module Native = struct
    include Int

    let to_int x = Some x
    let to_int_trunc x = x

    (* [of_int32_exn] is a safe operation on platforms with 64-bit word sizes. *)
    let of_int32 = of_int32_exn
    let to_nativeint_trunc x = to_nativeint x
    let to_nativeint x = Some (to_nativeint x)
    let repr = Int63_emul.Repr.Int
    let bswap32 t = Int64.to_int_trunc (Int64.bswap32 (Int64.of_int t))
    let bswap48 t = Int64.to_int_trunc (Int64.bswap48 (Int64.of_int t))
  end

  let impl : (module S) =
    match repr with
    | Immediate -> (module Native : S)
    | Non_immediate -> (module Int63_emul : S)
  ;;
end

include (val Backend.impl : Backend.S)

module Overflow_exn = struct
  let ( + ) t u =
    let sum = t + u in
    if bit_or (bit_xor t u) (bit_xor t (bit_not sum)) < zero
    then sum
    else
      raise_s
        (Sexp.message
           "( + ) overflow"
           [ "t", sexp_of_t t; "u", sexp_of_t u; "sum", sexp_of_t sum ])
  ;;

  let ( - ) t u =
    let diff = t - u in
    let pos_diff = t > u in
    if t <> u && Bool.( <> ) pos_diff (is_positive diff)
    then
      raise_s
        (Sexp.message
           "( - ) overflow"
           [ "t", sexp_of_t t; "u", sexp_of_t u; "diff", sexp_of_t diff ])
    else diff
  ;;

  let abs t = if t = min_value then failwith "abs overflow" else abs t
  let neg t = if t = min_value then failwith "neg overflow" else neg t
end

let () = assert (Int.( = ) num_bits 63)

let random_of_int ?(state = Random.State.default) bound =
  of_int (Random.State.int state (to_int_exn bound))
;;

let random_of_int64 ?(state = Random.State.default) bound =
  of_int64_exn (Random.State.int64 state (to_int64 bound))
;;

let random =
  match Word_size.word_size with
  | W64 -> random_of_int
  | W32 -> random_of_int64
;;

let random_incl_of_int ?(state = Random.State.default) lo hi =
  of_int (Random.State.int_incl state (to_int_exn lo) (to_int_exn hi))
;;

let random_incl_of_int64 ?(state = Random.State.default) lo hi =
  of_int64_exn (Random.State.int64_incl state (to_int64 lo) (to_int64 hi))
;;

let random_incl =
  match Word_size.word_size with
  | W64 -> random_incl_of_int
  | W32 -> random_incl_of_int64
;;

let floor_log2 t =
  match Word_size.word_size with
  | W64 -> t |> to_int_exn |> Int.floor_log2
  | W32 ->
    if t <= zero
    then raise_s (Sexp.message "[Int.floor_log2] got invalid input" [ "", sexp_of_t t ]);
    let floor_log2 = ref (Int.( - ) num_bits 2) in
    while equal zero (bit_and t (shift_left one !floor_log2)) do
      floor_log2 := Int.( - ) !floor_log2 1
    done;
    !floor_log2
;;

module Private = struct
  module Repr = Repr

  let repr = repr

  module Emul = Int63_emul
end
