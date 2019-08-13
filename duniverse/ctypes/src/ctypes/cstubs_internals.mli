(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Types and functions used by generated ML code.  This is an internal
   interface and subject to change. *)

open Ctypes
open Signed
open Unsigned

type voidp = Ctypes_ptr.voidp
type managed_buffer = Ctypes_memory_stubs.managed_buffer
type 'a fatptr = 'a typ Ctypes_ptr.Fat.t
type 'a fatfunptr = 'a fn Ctypes_ptr.Fat.t

val make_structured :
  ('a, 's) structured typ -> managed_buffer -> ('a, 's) structured

val make_ptr : 'a typ -> voidp -> 'a ptr
val make_fun_ptr : 'a fn -> voidp -> 'a Ctypes_static.static_funptr

val cptr : 'a ptr -> 'a typ Ctypes_ptr.Fat.t
val fptr : 'a Ctypes_static.static_funptr -> 'a fn Ctypes_ptr.Fat.t

type 'a ocaml_type = 'a Ctypes_static.ocaml_type =
  String     : string ocaml_type
| Bytes      : Bytes.t ocaml_type
| FloatArray : float array ocaml_type

type 'a typ = 'a Ctypes_static.typ =
    Void            :                              unit typ
  | Primitive       : 'a Ctypes_primitive_types.prim        -> 'a typ
  | Pointer         : 'a typ                    -> 'a ptr typ
  | Funptr          : 'a fn                     -> 'a static_funptr typ
  | Struct          : 'a Ctypes_static.structure_type  -> 'a Ctypes_static.structure typ
  | Union           : 'a Ctypes_static.union_type      -> 'a Ctypes_static.union typ
  | Abstract        : Ctypes_static.abstract_type      -> 'a Ctypes_static.abstract typ
  | View            : ('a, 'b) view             -> 'a typ
  | Array           : 'a typ * int              -> 'a Ctypes_static.carray typ
  | Bigarray        : (_, 'a, _) Ctypes_bigarray.t -> 'a typ
  | OCaml           : 'a ocaml_type             -> 'a ocaml typ
and ('a, 'b) pointer = ('a, 'b) Ctypes_static.pointer =
  CPointer : 'a typ Ctypes_ptr.Fat.t -> ('a, [`C]) pointer
| OCamlRef : int * 'a * 'a ocaml_type -> ('a, [`OCaml]) pointer
and 'a ptr = ('a, [`C]) pointer
and 'a ocaml = ('a, [`OCaml]) pointer
and 'a static_funptr = 'a Ctypes_static.static_funptr =
  Static_funptr of 'a fn Ctypes_ptr.Fat.t
and ('a, 'b) view = ('a, 'b) Ctypes_static.view = {
  read : 'b -> 'a;
  write : 'a -> 'b;
  format_typ: ((Format.formatter -> unit) -> Format.formatter -> unit) option;
  format: (Format.formatter -> 'a -> unit) option;
  ty: 'b typ;
}

type 'a fn = 'a Ctypes_static.fn =
  | Returns  : 'a typ   -> 'a fn
  | Function : 'a typ * 'b fn  -> ('a -> 'b) fn

type 'a prim = 'a Ctypes_primitive_types.prim =
  Char : char prim
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

val build_enum_type :
  string -> Ctypes_static.arithmetic -> ?typedef:bool ->
  ?unexpected:(int64 -> 'a) -> ('a * int64) list -> 'a typ

val use_value : 'a -> unit
