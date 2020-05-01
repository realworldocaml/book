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

let kind : type a b. (a, b) Bigarray.kind -> a kind = function
  | Bigarray.Float32 -> Kind_float32
  | Bigarray.Float64 -> Kind_float64
  | Bigarray.Int8_signed -> Kind_int8_signed
  | Bigarray.Int8_unsigned -> Kind_int8_unsigned
  | Bigarray.Int16_signed -> Kind_int16_signed
  | Bigarray.Int16_unsigned -> Kind_int16_unsigned
  | Bigarray.Int32 -> Kind_int32
  | Bigarray.Int64 -> Kind_int64
  | Bigarray.Int -> Kind_int
  | Bigarray.Nativeint -> Kind_nativeint
  | Bigarray.Complex32 -> Kind_complex32
  | Bigarray.Complex64 -> Kind_complex64
  | Bigarray.Char -> Kind_char

external address : 'b -> Ctypes_ptr.voidp
  = "ctypes_bigarray_address"

external view : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray.layout -> ('a, 'b, 'l) Bigarray.Genarray.t
  = "ctypes_bigarray_view"

external view1 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray.layout -> ('a, 'b, 'l) Bigarray.Array1.t
  = "ctypes_bigarray_view"

external view2 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray.layout -> ('a, 'b, 'l) Bigarray.Array2.t
  = "ctypes_bigarray_view"

external view3 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray.layout -> ('a, 'b, 'l) Bigarray.Array3.t
  = "ctypes_bigarray_view"
