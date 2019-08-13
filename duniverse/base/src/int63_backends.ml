open! Import

let raise_s = Error.raise_s

module type Int_or_more = sig
  type t [@@deriving_inline hash]
  include
  sig
    [@@@ocaml.warning "-32"]
    val hash_fold_t :
      Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
    val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
  end[@@ocaml.doc "@inline"]
  [@@@end]
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
end

module Native : Int_or_more with type t = private int = struct
  include Int
  let to_int x = Some x
  let to_int_trunc x = x
  (* [of_int32_exn] is a safe operation on platforms with 64-bit word sizes. *)
  let of_int32 = of_int32_exn
  let to_nativeint_trunc x = to_nativeint x
  let to_nativeint x = Some (to_nativeint x)
  let repr = Int63_emul.Repr.Int
end

module Emulated : Int_or_more with type t = Int63_emul.t = Int63_emul

let dynamic : (module Int_or_more) =
  match Word_size.word_size with
  | W64 -> (module Native   : Int_or_more)
  | W32 -> (module Emulated : Int_or_more)

module Dynamic = (val dynamic)
