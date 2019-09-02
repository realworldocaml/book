(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Unsigned
open Signed

type _ prim =
 | Char : char prim
 | Schar : int prim
 | Uchar : uchar prim
 | Bool : bool prim
 | Short : int prim
 | Int : int prim
 | Long : long prim
 | Llong : llong prim
 | Ushort : ushort prim
 | Sint : sint prim
 | Uint : uint prim
 | Ulong : ulong prim
 | Ullong : ullong prim
 | Size_t : size_t prim
 | Int8_t : int prim
 | Int16_t : int prim
 | Int32_t : int32 prim
 | Int64_t : int64 prim
 | Uint8_t : uint8 prim
 | Uint16_t : uint16 prim
 | Uint32_t : uint32 prim
 | Uint64_t : uint64 prim
 | Camlint : int prim
 | Nativeint : nativeint prim
 | Float : float prim
 | Double : float prim
 | LDouble : LDouble.t prim
 | Complex32 : Complex.t prim
 | Complex64 : Complex.t prim
 | Complexld : ComplexL.t prim

type _ ml_prim = 
  | ML_char :  char ml_prim
  | ML_complex :  Complex.t ml_prim
  | ML_complexld :  ComplexL.t ml_prim
  | ML_float :  float ml_prim
  | ML_ldouble :  LDouble.t ml_prim
  | ML_int :  int ml_prim
  | ML_int32 :  int32 ml_prim
  | ML_int64 :  int64 ml_prim
  | ML_llong :  llong ml_prim
  | ML_long :  long ml_prim
  | ML_sint : sint ml_prim
  | ML_nativeint :  nativeint ml_prim
  | ML_size_t :  size_t ml_prim
  | ML_uchar :  uchar ml_prim
  | ML_bool :  bool ml_prim
  | ML_uint :  uint ml_prim
  | ML_uint16 :  uint16 ml_prim
  | ML_uint32 :  uint32 ml_prim
  | ML_uint64 :  uint64 ml_prim
  | ML_uint8 :  uint8 ml_prim
  | ML_ullong :  ullong ml_prim
  | ML_ulong :  ulong ml_prim
  | ML_ushort :  ushort ml_prim

let ml_prim : type a. a prim -> a ml_prim = function
  | Char -> ML_char
  | Schar -> ML_int
  | Uchar -> ML_uchar
  | Bool -> ML_bool
  | Short -> ML_int
  | Int -> ML_int
  | Long -> ML_long
  | Llong -> ML_llong
  | Ushort -> ML_ushort
  | Sint -> ML_sint
  | Uint -> ML_uint
  | Ulong -> ML_ulong
  | Ullong -> ML_ullong
  | Size_t -> ML_size_t
  | Int8_t -> ML_int
  | Int16_t -> ML_int
  | Int32_t -> ML_int32
  | Int64_t -> ML_int64
  | Uint8_t -> ML_uint8
  | Uint16_t -> ML_uint16
  | Uint32_t -> ML_uint32
  | Uint64_t -> ML_uint64
  | Camlint -> ML_int
  | Nativeint -> ML_nativeint
  | Float -> ML_float
  | Double -> ML_float
  | LDouble -> ML_ldouble
  | Complex32 -> ML_complex
  | Complex64 -> ML_complex
  | Complexld -> ML_complexld
