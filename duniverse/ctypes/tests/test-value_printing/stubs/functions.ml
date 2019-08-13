(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the value printing tests. *)

open Ctypes

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  let retrieve_CHAR_MIN = foreign "retrieve_CHAR_MIN"
    (void @-> returning char)
  let retrieve_CHAR_MAX = foreign "retrieve_CHAR_MAX"
    (void @-> returning char)

  let retrieve_SCHAR_MIN = foreign "retrieve_SCHAR_MIN"
    (void @-> returning schar)

  let retrieve_SCHAR_MAX = foreign "retrieve_SCHAR_MAX"
    (void @-> returning schar)

  let retrieve_SHRT_MIN = foreign "retrieve_SHRT_MIN"
    (void @-> returning short)
  let retrieve_SHRT_MAX = foreign "retrieve_SHRT_MAX"
    (void @-> returning short)

  let retrieve_INT_MIN = foreign "retrieve_INT_MIN"
    (void @-> returning int)

  let retrieve_INT_MAX = foreign "retrieve_INT_MAX"
    (void @-> returning int)

  let retrieve_LONG_MAX = foreign "retrieve_LONG_MAX"
    (void @-> returning long)

  let retrieve_LONG_MIN = foreign "retrieve_LONG_MIN"
    (void @-> returning long)

  let retrieve_LLONG_MAX = foreign "retrieve_LLONG_MAX"
    (void @-> returning llong)

  let retrieve_LLONG_MIN = foreign "retrieve_LLONG_MIN"
    (void @-> returning llong)

  let retrieve_UCHAR_MAX = foreign "retrieve_UCHAR_MAX"
    (void @-> returning uchar)

  let retrieve_USHRT_MAX = foreign "retrieve_USHRT_MAX"
    (void @-> returning ushort)

  let retrieve_UINT_MAX = foreign "retrieve_UINT_MAX"
    (void @-> returning uint)

  let retrieve_ULONG_MAX = foreign "retrieve_ULONG_MAX"
    (void @-> returning ulong)

  let retrieve_ULLONG_MAX = foreign "retrieve_ULLONG_MAX"
    (void @-> returning ullong)

  let retrieve_INT8_MIN = foreign "retrieve_INT8_MIN"
    (void @-> returning int8_t)

  let retrieve_INT8_MAX = foreign "retrieve_INT8_MAX"
    (void @-> returning int8_t)

  let retrieve_INT16_MIN = foreign "retrieve_INT16_MIN"
    (void @-> returning int16_t)

  let retrieve_INT16_MAX = foreign "retrieve_INT16_MAX"
    (void @-> returning int16_t)

  let retrieve_INT32_MIN = foreign "retrieve_INT32_MIN"
    (void @-> returning int32_t)

  let retrieve_INT32_MAX = foreign "retrieve_INT32_MAX"
    (void @-> returning int32_t)

  let retrieve_INT64_MIN = foreign "retrieve_INT64_MIN"
    (void @-> returning int64_t)

  let retrieve_INT64_MAX = foreign "retrieve_INT64_MAX"
    (void @-> returning int64_t)

  let retrieve_UINT8_MAX = foreign "retrieve_UINT8_MAX"
    (void @-> returning uint8_t)

  let retrieve_UINT16_MAX = foreign "retrieve_UINT16_MAX"
    (void @-> returning uint16_t)

  let retrieve_UINT32_MAX = foreign "retrieve_UINT32_MAX"
    (void @-> returning uint32_t)

  let retrieve_UINT64_MAX = foreign "retrieve_UINT64_MAX"
    (void @-> returning uint64_t)

  let retrieve_SIZE_MAX = foreign "retrieve_SIZE_MAX"
    (void @-> returning size_t)

  (* float *)
  let retrieve_FLT_MIN = foreign "retrieve_FLT_MIN"
    (void @-> returning float)

  let retrieve_FLT_MAX = foreign "retrieve_FLT_MAX"
    (void @-> returning float)

  let retrieve_DBL_MIN = foreign "retrieve_DBL_MIN"
    (void @-> returning double)

  let retrieve_DBL_MAX = foreign "retrieve_DBL_MAX"
    (void @-> returning double)
end
