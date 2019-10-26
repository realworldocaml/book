(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Struct_stubs(S : Ctypes.TYPE) =
struct
  open S

  let _SCHAR_MIN = constant "SCHAR_MIN" schar
  let _SCHAR_MAX = constant "SCHAR_MAX" schar
  let _UCHAR_MAX = constant "UCHAR_MAX" uchar
  let _CHAR_MIN = constant "CHAR_MIN" char
  let _CHAR_MAX = constant "CHAR_MAX" char
  let _SHRT_MIN = constant "SHRT_MIN" short
  let _SHRT_MAX = constant "SHRT_MAX" short
  let _USHRT_MAX = constant "USHRT_MAX" ushort
  let _INT_MIN = constant "INT_MIN" sint
  let _INT_MAX = constant "INT_MAX" sint
  let _UINT_MAX = constant "UINT_MAX" uint
  let _LONG_MAX = constant "LONG_MAX" long
  let _LONG_MIN = constant "LONG_MIN" long
  let _ULONG_MAX = constant "ULONG_MAX" ulong
  let _LLONG_MAX = constant "LLONG_MAX" llong
  let _LLONG_MIN = constant "LLONG_MIN" llong
  let _ULLONG_MAX = constant "ULLONG_MAX" ullong
  let _INT8_MIN = constant "INT8_MIN" int8_t
  let _INT16_MIN = constant "INT16_MIN" int16_t
  let _INT32_MIN = constant "INT32_MIN" int32_t
  let _INT64_MIN = constant "INT64_MIN" int64_t
  let _INT8_MAX = constant "INT8_MAX" int8_t
  let _INT16_MAX = constant "INT16_MAX" int16_t
  let _INT32_MAX = constant "INT32_MAX" int32_t
  let _INT64_MAX = constant "INT64_MAX" int64_t
  let _UINT8_MAX = constant "UINT8_MAX" uint8_t
  let _UINT16_MAX = constant "UINT16_MAX" uint16_t
  let _UINT32_MAX = constant "UINT32_MAX" uint32_t
  let _UINT64_MAX = constant "UINT64_MAX" uint64_t
  let _SIZE_MAX = constant "SIZE_MAX" size_t
  let _true = constant "true" bool
  let _false = constant "false" bool

  let i32_inverted = view int32_t
      ~read:Int32.neg ~write:Int32.neg
  let neg_INT16_MAX = constant "INT16_MAX" i32_inverted
  let neg_INT16_MIN = constant "INT16_MIN" i32_inverted

  let _A = constant "A" int
  let _B = constant "B" int
  let _C = constant "C" int
  let _D = constant "D" int
end
