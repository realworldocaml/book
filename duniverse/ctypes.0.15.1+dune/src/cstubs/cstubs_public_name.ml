(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Publicly visible names for type values *)

open Ctypes_path

let ident_of_ml_prim : type a. a Ctypes_primitive_types.ml_prim -> path =
  let open Ctypes_primitive_types in function
   | ML_char -> path_of_string "char"
   | ML_bool -> path_of_string "bool"
   | ML_complex -> path_of_string "Complex.t"
   | ML_float -> path_of_string "float"
   | ML_int -> path_of_string "int"
   | ML_int32 -> path_of_string "int32"
   | ML_int64 -> path_of_string "int64"
   | ML_llong -> path_of_string "Signed.llong"
   | ML_long -> path_of_string "Signed.long"
   | ML_sint -> path_of_string "Signed.sint"
   | ML_nativeint -> path_of_string "nativeint"
   | ML_size_t -> path_of_string "Unsigned.size_t"
   | ML_uchar -> path_of_string "Unsigned.uchar"
   | ML_uint -> path_of_string "Unsigned.uint"
   | ML_uint16 -> path_of_string "Unsigned.uint16"
   | ML_uint32 -> path_of_string "Unsigned.uint32"
   | ML_uint64 -> path_of_string "Unsigned.uint64"
   | ML_uint8 -> path_of_string "Unsigned.uint8"
   | ML_ullong -> path_of_string "Unsigned.ullong"
   | ML_ulong -> path_of_string "Unsigned.ulong"
   | ML_ushort -> path_of_string "Unsigned.ushort"
   | ML_ldouble -> path_of_string "LDouble.t"
   | ML_complexld -> path_of_string "ComplexL.t"

let constructor_ident_of_prim : type a. a Ctypes_primitive_types.prim -> path =
  let open Ctypes_primitive_types in function
   | Char -> path_of_string "Ctypes.char"
   | Schar -> path_of_string "Ctypes.schar"
   | Uchar -> path_of_string "Ctypes.uchar"
   | Bool -> path_of_string "Ctypes.bool"
   | Short -> path_of_string "Ctypes.short"
   | Int -> path_of_string "Ctypes.int"
   | Long -> path_of_string "Ctypes.long"
   | Llong -> path_of_string "Ctypes.llong"
   | Ushort -> path_of_string "Ctypes.ushort"
   | Sint -> path_of_string "Ctypes.sint"
   | Uint -> path_of_string "Ctypes.uint"
   | Ulong -> path_of_string "Ctypes.ulong"
   | Ullong -> path_of_string "Ctypes.ullong"
   | Size_t -> path_of_string "Ctypes.size_t"
   | Int8_t -> path_of_string "Ctypes.int8_t"
   | Int16_t -> path_of_string "Ctypes.int16_t"
   | Int32_t -> path_of_string "Ctypes.int32_t"
   | Int64_t -> path_of_string "Ctypes.int64_t"
   | Uint8_t -> path_of_string "Ctypes.uint8_t"
   | Uint16_t -> path_of_string "Ctypes.uint16_t"
   | Uint32_t -> path_of_string "Ctypes.uint32_t"
   | Uint64_t -> path_of_string "Ctypes.uint64_t"
   | Camlint -> path_of_string "Ctypes.camlint"
   | Nativeint -> path_of_string "Ctypes.nativeint"
   | Float -> path_of_string "Ctypes.float"
   | Double -> path_of_string "Ctypes.double"
   | LDouble -> path_of_string "Ctypes.ldouble"
   | Complex32 -> path_of_string "Ctypes.complex32"
   | Complex64 -> path_of_string "Ctypes.complex64"
   | Complexld -> path_of_string "Ctypes.complexld"

let constructor_cident_of_prim :
  type a. ?module_name:string -> a Ctypes_primitive_types.prim -> path =
  fun ?(module_name="Cstubs_internals") ->
    let path ident =
      path_of_string (Printf.sprintf "%s.%s" module_name ident)
    in Ctypes_primitive_types.(function
    | Char -> path "Char"
    | Schar -> path "Schar"
    | Uchar -> path "Uchar"
    | Bool -> path "Bool"
    | Short -> path "Short"
    | Int -> path "Int"
    | Long -> path "Long"
    | Llong -> path "Llong"
    | Ushort -> path "Ushort"
    | Sint -> path "Sint"
    | Uint -> path "Uint"
    | Ulong -> path "Ulong"
    | Ullong -> path "Ullong"
    | Size_t -> path "Size_t"
    | Int8_t -> path "Int8_t"
    | Int16_t -> path "Int16_t"
    | Int32_t -> path "Int32_t"
    | Int64_t -> path "Int64_t"
    | Uint8_t -> path "Uint8_t"
    | Uint16_t -> path "Uint16_t"
    | Uint32_t -> path "Uint32_t"
    | Uint64_t -> path "Uint64_t"
    | Camlint -> path "Camlint"
    | Nativeint -> path "Nativeint"
    | Float -> path "Float"
    | Double -> path "Double"
    | LDouble -> path "LDouble"
    | Complex32 -> path "Complex32"
    | Complex64 -> path "Complex64"
    | Complexld -> path "Complexld")
