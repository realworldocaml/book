(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type _ kind =
    Kind_float32 : float kind
  | Kind_float64 : float kind
  | Kind_int8_signed : int kind
  | Kind_int8_unsigned : int kind
  | Kind_int16_signed : int kind
  | Kind_int16_unsigned : int kind
  | Kind_int32 : int32 kind
  | Kind_int64 : int64 kind
  | Kind_int : int kind
  | Kind_nativeint : nativeint kind
  | Kind_complex32 : Complex.t kind
  | Kind_complex64 : Complex.t kind
  | Kind_char : char kind

let kind : type a b. (a, b) Bigarray_compat.kind -> a kind = function
  | Bigarray_compat.Float32 -> Kind_float32
  | Bigarray_compat.Float64 -> Kind_float64
  | Bigarray_compat.Int8_signed -> Kind_int8_signed
  | Bigarray_compat.Int8_unsigned -> Kind_int8_unsigned
  | Bigarray_compat.Int16_signed -> Kind_int16_signed
  | Bigarray_compat.Int16_unsigned -> Kind_int16_unsigned
  | Bigarray_compat.Int32 -> Kind_int32
  | Bigarray_compat.Int64 -> Kind_int64
  | Bigarray_compat.Int -> Kind_int
  | Bigarray_compat.Nativeint -> Kind_nativeint
  | Bigarray_compat.Complex32 -> Kind_complex32
  | Bigarray_compat.Complex64 -> Kind_complex64
  | Bigarray_compat.Char -> Kind_char

external address : 'b -> Ctypes_ptr.voidp
  = "ctypes_bigarray_address"

external view : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray_compat.layout -> ('a, 'b, 'l) Bigarray_compat.Genarray.t
  = "ctypes_bigarray_view"

external view1 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray_compat.layout -> ('a, 'b, 'l) Bigarray_compat.Array1.t
  = "ctypes_bigarray_view"

external view2 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray_compat.layout -> ('a, 'b, 'l) Bigarray_compat.Array2.t
  = "ctypes_bigarray_view"

external view3 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray_compat.layout -> ('a, 'b, 'l) Bigarray_compat.Array3.t
  = "ctypes_bigarray_view"
