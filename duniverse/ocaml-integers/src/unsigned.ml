(*
 * Copyright (c) 2013 Jeremy Yallop.
 * Copyright (c) 2021 Nomadic Labs
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

external init : unit -> unit = "integers_unsigned_init"
let () = init ()

(* Boxed unsigned types *)
module type Basics = sig
  type t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val max_int : t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val of_int : int -> t
  val to_int : t -> int
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t
  val to_string : t -> string
  val to_hexstring : t -> string
end


module type Extras = sig
  type t

  val zero : t
  val one : t
  val lognot : t -> t
  val succ : t -> t
  val pred : t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val max : t -> t -> t
  val min : t -> t -> t
  val of_string_opt : string -> t option
  val pp : Format.formatter -> t -> unit
  val pp_hex : Format.formatter -> t -> unit
end


module type Infix = sig
  type t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t
  val (mod) : t -> t -> t
  val (land) : t -> t -> t
  val (lor) : t -> t -> t
  val (lxor) : t -> t -> t
  val (lsl) : t -> int -> t
  val (lsr) : t -> int -> t
end


module type S = sig
  include Basics
  include Extras with type t := t

  module Infix : Infix with type t := t
end


module MakeInfix (B : Basics) =
struct
  open B
  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = div
  let (mod) = rem
  let (land) = logand
  let (lor) = logor
  let (lxor) = logxor
  let (lsl) = shift_left
  let (lsr) = shift_right
end


module Extras(Basics : Basics) : Extras with type t := Basics.t =
struct
  open Basics
  let zero = of_int 0
  let one = of_int 1
  let succ n = add n one
  let pred n = sub n one
  let lognot n = logxor n max_int
  let compare (x : t) (y : t) = Stdlib.compare x y
  let equal (x : t) (y : t) = Stdlib.(=) x y
  let max (x : t) (y : t) = Stdlib.max x y
  let min (x : t) (y : t) = Stdlib.min x y
  let of_string_opt (s : string) = try Some (of_string s) with Failure _ -> None
  let pp fmt x = Format.fprintf fmt "%s" (to_string x)
  let pp_hex fmt x = Format.fprintf fmt "%s" (to_hexstring x)
end

external format_int : string -> int -> string = "caml_format_int"

module UInt8 : S with type t = private int =
struct
  module B =
  struct
    type t = int
    let max_int = 255
    let add : t -> t -> t = fun x y -> (x + y) land max_int
    let sub : t -> t -> t = fun x y -> (x - y) land max_int
    let mul : t -> t -> t = fun x y -> (x * y) land max_int
    let div : t -> t -> t = (/)
    let rem : t -> t -> t = (mod)
    let logand: t -> t -> t = (land)
    let logor: t -> t -> t = (lor)
    let logxor : t -> t -> t = (lxor)
    let shift_left : t -> int -> t = fun x y -> (x lsl y) land max_int
    let shift_right : t -> int -> t = (lsr)
    let of_int (x: int): t =
      (* For backwards compatibility, this wraps *)
      x land max_int
    external to_int : t -> int = "%identity"
    let of_int64 : int64 -> t = fun x -> of_int (Int64.to_int x)
    let to_int64 : t -> int64 = fun x -> Int64.of_int (to_int x)
    external of_string : string -> t = "integers_uint8_of_string"
    let to_string : t -> string = string_of_int
    let to_hexstring : t -> string = format_int "%x"
  end
  include B
  include Extras(B)
  module Infix = MakeInfix(B)
end


module UInt16 : S with type t = private int =
struct
  module B =
  struct
    type t = int
    let max_int = 65535
    let add : t -> t -> t = fun x y -> (x + y) land max_int
    let sub : t -> t -> t = fun x y -> (x - y) land max_int
    let mul : t -> t -> t = fun x y -> (x * y) land max_int
    let div : t -> t -> t = (/)
    let rem : t -> t -> t = (mod)
    let logand: t -> t -> t = (land)
    let logor: t -> t -> t = (lor)
    let logxor : t -> t -> t = (lxor)
    let shift_left : t -> int -> t = fun x y -> (x lsl y) land max_int
    let shift_right : t -> int -> t = (lsr)
    let of_int (x: int): t =
      (* For backwards compatibility, this wraps *)
      x land max_int
    external to_int : t -> int = "%identity"
    let of_int64 : int64 -> t = fun x -> Int64.to_int x |> of_int
    let to_int64 : t -> int64 = fun x -> to_int x |> Int64.of_int
    external of_string : string -> t = "integers_uint16_of_string"
    let to_string : t -> string = string_of_int
    let to_hexstring : t -> string = format_int "%x"
  end
  include B
  include Extras(B)
  module Infix = MakeInfix(B)
end


module UInt32 : sig
  include S
  val of_int32 : int32 -> t
  val to_int32 : t -> int32
end =
struct
  module B =
  struct
    type t
    external add : t -> t -> t = "integers_uint32_add"
    external sub : t -> t -> t = "integers_uint32_sub"
    external mul : t -> t -> t = "integers_uint32_mul"
    external div : t -> t -> t = "integers_uint32_div"
    external rem : t -> t -> t = "integers_uint32_rem"
    external logand : t -> t -> t = "integers_uint32_logand"
    external logor : t -> t -> t = "integers_uint32_logor"
    external logxor : t -> t -> t = "integers_uint32_logxor"
    external shift_left : t -> int -> t = "integers_uint32_shift_left"
    external shift_right : t -> int -> t = "integers_uint32_shift_right"
    external of_string : string -> t = "integers_uint32_of_string"
    external to_string : t -> string = "integers_uint32_to_string"
    external to_hexstring : t -> string = "integers_uint32_to_hexstring"
    external of_int : int -> t = "integers_uint32_of_int"
    external to_int : t -> int = "integers_uint32_to_int"

    external of_int32 : int32 -> t = "integers_uint32_of_int32"
    let half_max_plus_two = of_string "0x80000001"
    let half_max_minus_one_signed = 0x7fffffffl
    let of_int32 i32 =
       if i32 >= 0l then
          of_int32 i32
       else
          add half_max_plus_two (of_int32 (Int32.add i32 half_max_minus_one_signed))

    external to_int32 : t -> int32 = "integers_int32_of_uint32"
    let max_signed = of_int32 Int32.max_int
    let to_int32 u32 =
       if Stdlib.compare u32 max_signed <= 0 then
          to_int32 u32
       else
          Int32.sub (to_int32 (sub u32 half_max_plus_two)) half_max_minus_one_signed

    external of_int64 : int64 -> t = "integers_uint32_of_int64"
    external to_int64 : t -> int64 = "integers_uint32_to_int64"
    external _max_int : unit -> t = "integers_uint32_max"
    let max_int = _max_int ()
  end
  include B
  include Extras(B)
  module Infix = MakeInfix(B)
end


module UInt64 : sig
  include S
  external of_uint32 : UInt32.t -> t = "integers_uint64_of_uint32"
  external to_uint32 : t -> UInt32.t = "integers_uint32_of_uint64"
end = 
struct
  module B =
  struct
    type t
    external add : t -> t -> t = "integers_uint64_add"
    external sub : t -> t -> t = "integers_uint64_sub"
    external mul : t -> t -> t = "integers_uint64_mul"
    external div : t -> t -> t = "integers_uint64_div"
    external rem : t -> t -> t = "integers_uint64_rem"
    external logand : t -> t -> t = "integers_uint64_logand"
    external logor : t -> t -> t = "integers_uint64_logor"
    external logxor : t -> t -> t = "integers_uint64_logxor"
    external shift_left : t -> int -> t = "integers_uint64_shift_left"
    external shift_right : t -> int -> t = "integers_uint64_shift_right"
    external of_int : int -> t = "integers_uint64_of_int"
    external to_int : t -> int = "integers_uint64_to_int"
    external of_string : string -> t = "integers_uint64_of_string"
    external to_string : t -> string = "integers_uint64_to_string"
    external to_hexstring : t -> string = "integers_uint64_to_hexstring"

    external of_int64 : int64 -> t = "integers_uint64_of_int64"
    let half_max_plus_two = of_string "0x8000000000000001"
    let half_max_minus_one_signed = 0x7fffffffffffffffL
    let of_int64 i64 =
       if i64 >= 0L then
          of_int64 i64
       else
          add half_max_plus_two (of_int64 (Int64.add i64 half_max_minus_one_signed))

    external to_int64 : t -> int64 = "integers_uint64_to_int64"
    let max_signed = of_int64 Int64.max_int
    let to_int64 u64 =
       if Stdlib.compare u64 max_signed <= 0 then
          to_int64 u64
       else
          Int64.sub (to_int64 (sub u64 half_max_plus_two)) half_max_minus_one_signed

    external of_uint32 : UInt32.t -> t = "integers_uint64_of_uint32"
    external to_uint32 : t -> UInt32.t = "integers_uint32_of_uint64"
    external _max_int : unit -> t = "integers_uint64_max"
    let max_int = _max_int ()
  end
  include B
  include Extras(B)
  module Infix = MakeInfix(B)
end


let of_byte_size : int -> (module S) = function
  | 1 -> (module UInt8)
  | 2 -> (module UInt16)
  | 4 -> (module UInt32)
  | 8 -> (module UInt64)
  | _ -> invalid_arg "Unsigned.of_byte_size"

      
external size_t_size : unit -> int = "integers_size_t_size"
external ushort_size : unit -> int = "integers_ushort_size"
external uint_size : unit -> int = "integers_uint_size"
external ulong_size : unit -> int = "integers_ulong_size"
external ulonglong_size : unit -> int = "integers_ulonglong_size"

module Size_t : S = (val of_byte_size (size_t_size ()))
module UChar = UInt8
module UShort : S = (val of_byte_size (ushort_size ()))
module UInt : S = (val of_byte_size (uint_size ()))
module ULong : S = (val of_byte_size (ulong_size ()))
module ULLong : S = (val of_byte_size (ulonglong_size ()))

type uchar = UChar.t
type uint8 = UInt8.t
type uint16 = UInt16.t
type uint32 = UInt32.t
type uint64 = UInt64.t
type size_t = Size_t.t
type ushort = UShort.t
type uint = UInt.t
type ulong = ULong.t
type ullong = ULLong.t
