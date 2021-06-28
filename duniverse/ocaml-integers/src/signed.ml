(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Pervasives = Pervasives [@@ocaml.warning "-3"]

module type Infix = sig
  type t
  include Unsigned.Infix with type t := t
  val (asr) : t -> int -> t
end

module type S = sig
  type t

  module Infix : Infix with type t := t

  include Unsigned.S with type t := t
                     with module Infix := Infix

  val neg : t -> t
  val abs : t -> t
  val minus_one : t
  val min_int : t
  val shift_right_logical : t -> int -> t
  val of_nativeint : nativeint -> t
  val to_nativeint : t -> nativeint
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
end

module type Basics = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
end

module MakeInfix(S : Basics) =
struct
  open S
  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = div
  let (mod) = rem
  let (land) = logand
  let (lor) = logor
  let (lxor) = logxor
  let (lsl) = shift_left
  let (lsr) = shift_right_logical
  let (asr) = shift_right
end

module Int =
struct
  module Basics =
  struct
    type t = int
    let add = ( + )
    let sub = ( - )
    let mul = ( * )
    let div = ( / )
    let rem = ( mod )
    let max_int = Pervasives.max_int
    let min_int = Pervasives.min_int
    let logand = ( land )
    let logor = ( lor )
    let logxor = ( lxor )
    let shift_left = ( lsl )
    let shift_right = ( asr )
    let shift_right_logical = ( lsr )
    let of_int x = x
    let to_int x = x
    let of_string = int_of_string
    let to_string = string_of_int
    let zero = 0
    let one = 1
    let minus_one = -1
    let lognot = lnot
    let succ = Pervasives.succ
    let pred = Pervasives.pred
    let compare = Pervasives.compare
    let equal = Pervasives.(=)
    let max = Pervasives.max
    let min = Pervasives.min
  end
  include Basics
  module Infix = MakeInfix(Basics)
  let to_int64 = Int64.of_int
  let of_int64 = Int64.to_int
  let to_nativeint = Nativeint.of_int
  let of_nativeint = Nativeint.to_int
  let abs = Pervasives.abs
  let neg x = -x
  let pp fmt n = Format.fprintf fmt "%d" n
end

module Int32 = 
struct
  (* Int32.equal was introduced in OCaml 4.03.0 *)
  let equal (x:int32) (y:int32) = x = y [@@ocaml.warning "-32"]
  include Int32
  module Infix = MakeInfix(Int32)
  let of_nativeint = Nativeint.to_int32
  let to_nativeint = Nativeint.of_int32
  let of_int64 = Int64.to_int32
  let to_int64 = Int64.of_int32
  let max = Pervasives.max
  let min = Pervasives.min
  let pp fmt n = Format.fprintf fmt "%ld" n
end

module Int64 = 
struct
  (* Int64.equal was introduced in OCaml 4.03.0 *)
  let equal (x:int64) (y:int64) = x = y [@@ocaml.warning "-32"]
  include Int64
  module Infix = MakeInfix(Int64)
  let of_int64 x = x
  let to_int64 x = x
  let max = Pervasives.max
  let min = Pervasives.min
  let pp fmt n = Format.fprintf fmt "%Ld" n
end

(* C guarantees that sizeof(t) == sizeof(unsigned t) *)
external int_size : unit -> int = "integers_uint_size"
external long_size : unit -> int = "integers_ulong_size"
external llong_size : unit -> int = "integers_ulonglong_size"

let of_byte_size : int -> (module S) = function
  | 4 -> (module Int32)
  | 8 -> (module Int64)
  | _ -> invalid_arg "Signed.of_byte_size"

module SInt = (val of_byte_size (int_size ()))
module Long = (val of_byte_size (long_size ()))
module LLong = (val of_byte_size (llong_size ()))

type sint = SInt.t
type long = Long.t
type llong = LLong.t
