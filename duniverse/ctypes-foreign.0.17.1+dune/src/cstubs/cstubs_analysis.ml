(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Analysis for stub generation *)

open Ctypes_static

let is_float_primitive : type a. a typ -> bool =
  let open Ctypes_primitive_types in function
  | Primitive Float -> true
  | Primitive Double -> true
  | _ -> false

let rec float : type a. a fn -> bool = function
  | Returns t -> is_float_primitive t
  | Function (f, t) -> is_float_primitive f && float t

(* A value of type 'a noalloc says that reading a value of type 'a
   will not cause an OCaml allocation in C code. *)
type _ noalloc =
  Noalloc_unit : unit noalloc
| Noalloc_int : int noalloc
| Noalloc_uint8_t : Unsigned.uint8 noalloc
| Noalloc_uint16_t : Unsigned.uint16 noalloc
| Noalloc_char : char noalloc
| Noalloc_bool : bool noalloc
| Noalloc_view : ('a, 'b) view * 'b noalloc -> 'a noalloc

(* A value of type 'a alloc says that reading a value of type 'a
   may cause an OCaml allocation in C code. *)
type _ alloc =
| Alloc_sint : Signed.sint alloc
| Alloc_long : Signed.long alloc
| Alloc_llong : Signed.llong alloc
| Alloc_uint : Unsigned.uint alloc
| Alloc_uchar : Unsigned.uchar alloc
| Alloc_ushort : Unsigned.ushort alloc
| Alloc_ulong : Unsigned.ulong alloc
| Alloc_ullong : Unsigned.ullong alloc
| Alloc_size_t : Unsigned.size_t alloc
| Alloc_int32_t : int32 alloc
| Alloc_int64_t : int64 alloc
| Alloc_uint32_t : Unsigned.uint32 alloc
| Alloc_uint64_t : Unsigned.uint64 alloc
| Alloc_nativeint : nativeint alloc
| Alloc_float : float alloc
| Alloc_ldouble : LDouble.t alloc
| Alloc_complex : Complex.t alloc
| Alloc_complexld : ComplexL.t alloc
| Alloc_pointer : (_, _) pointer alloc
| Alloc_funptr : _ static_funptr alloc
| Alloc_structured : (_, _) structured alloc
| Alloc_array : _ carray alloc
| Alloc_bigarray : (_, 'a, _) Ctypes_bigarray.t -> 'a alloc
| Alloc_view : ('a, 'b) view * 'b alloc -> 'a alloc

type 'a allocation = [ `Noalloc of 'a noalloc | `Alloc of 'a alloc ]

let primitive_allocation : type a. a Ctypes_primitive_types.prim -> a allocation =
 let open Ctypes_primitive_types in function
 | Char -> `Noalloc Noalloc_char
 | Bool -> `Noalloc Noalloc_bool
 | Schar -> `Noalloc Noalloc_int
 | Short -> `Noalloc Noalloc_int
 | Int -> `Noalloc Noalloc_int
 | Int8_t -> `Noalloc Noalloc_int
 | Int16_t -> `Noalloc Noalloc_int
 | Uint8_t -> `Noalloc Noalloc_uint8_t
 | Uint16_t -> `Noalloc Noalloc_uint16_t
 | Camlint -> `Noalloc Noalloc_int
 | Long -> `Alloc Alloc_long
 | Llong -> `Alloc Alloc_llong
 | Ushort -> `Alloc Alloc_ushort
 | Uchar -> `Alloc Alloc_uchar
 | Sint -> `Alloc Alloc_sint
 | Uint -> `Alloc Alloc_uint
 | Ulong -> `Alloc Alloc_ulong
 | Ullong -> `Alloc Alloc_ullong
 | Size_t -> `Alloc Alloc_size_t
 | Int32_t -> `Alloc Alloc_int32_t
 | Int64_t -> `Alloc Alloc_int64_t
 | Uint32_t -> `Alloc Alloc_uint32_t
 | Uint64_t -> `Alloc Alloc_uint64_t
 | Nativeint -> `Alloc Alloc_nativeint
 | Float -> `Alloc Alloc_float
 | Double -> `Alloc Alloc_float
 | LDouble -> `Alloc Alloc_ldouble
 | Complex32 -> `Alloc Alloc_complex
 | Complex64 -> `Alloc Alloc_complex
 | Complexld -> `Alloc Alloc_complexld

let rec allocation : type a. a typ -> a allocation = function
 | Void -> `Noalloc Noalloc_unit
 | Primitive p -> primitive_allocation p
 | Pointer _ -> `Alloc Alloc_pointer
 | Funptr _ -> `Alloc Alloc_funptr
 | Struct _ -> `Alloc Alloc_structured
 | Union _ -> `Alloc Alloc_structured
 | Abstract _ -> `Alloc Alloc_structured
 | View v ->
   begin match allocation v.ty with
   | `Alloc a -> `Alloc (Alloc_view (v, a))
   | `Noalloc na -> `Noalloc (Noalloc_view (v, na))
   end
 | Array _ -> `Alloc Alloc_array
 | Bigarray ba -> `Alloc (Alloc_bigarray ba)
 | OCaml _ -> `Alloc Alloc_pointer

let rec may_allocate : type a. a fn -> bool = function
  | Returns t ->
    begin match allocation t with
    | `Noalloc _ -> false
    | `Alloc _ -> true
    end
  | Function (_, t) -> may_allocate t
