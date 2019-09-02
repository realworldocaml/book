(*
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open Foreign

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F
  let callback_returns_int8_t = foreign "callback_returns_int8_t"
      (funptr Ctypes.(void @-> returning int8_t) @-> returning int8_t)

  let callback_returns_int16_t = foreign "callback_returns_int16_t"
      (funptr Ctypes.(void @-> returning int16_t) @-> returning int16_t)

  let callback_returns_int32_t = foreign "callback_returns_int32_t"
      (funptr Ctypes.(void @-> returning int32_t) @-> returning int32_t)

  let callback_returns_int64_t = foreign "callback_returns_int64_t"
      (funptr Ctypes.(void @-> returning int64_t) @-> returning int64_t)

  let callback_returns_uint8_t = foreign "callback_returns_uint8_t"
      (funptr Ctypes.(void @-> returning uint8_t) @-> returning uint8_t)

  let callback_returns_uint16_t = foreign "callback_returns_uint16_t"
      (funptr Ctypes.(void @-> returning uint16_t) @-> returning uint16_t)

  let callback_returns_uint32_t = foreign "callback_returns_uint32_t"
      (funptr Ctypes.(void @-> returning uint32_t) @-> returning uint32_t)

  let callback_returns_uint64_t = foreign "callback_returns_uint64_t"
      (funptr Ctypes.(void @-> returning uint64_t) @-> returning uint64_t)

  let callback_returns_float = foreign "callback_returns_float"
      (funptr Ctypes.(void @-> returning float) @-> returning float)

  let callback_returns_double = foreign "callback_returns_double"
      (funptr Ctypes.(void @-> returning double) @-> returning double)

  let callback_returns_bool = foreign "callback_returns_bool"
      (funptr Ctypes.(void @-> returning bool) @-> returning bool)

end
