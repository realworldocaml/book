(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes_bigarray_stubs

let prim_of_kind : type a. a kind -> a Ctypes_primitive_types.prim
  = let open Ctypes_primitive_types in function
    Kind_float32 -> Float
  | Kind_float64 -> Double
  | Kind_int8_signed -> Int8_t
  | Kind_int8_unsigned -> Int8_t
  | Kind_int16_signed -> Int16_t
  | Kind_int16_unsigned -> Int16_t
  | Kind_int32 -> Int32_t
  | Kind_int64 -> Int64_t
  | Kind_int -> Camlint
  | Kind_nativeint -> Nativeint
  | Kind_complex32 -> Complex32
  | Kind_complex64 -> Complex64
  | Kind_char -> Char

let bigarray_kind_sizeof k = Ctypes_primitives.sizeof (prim_of_kind k)

let bigarray_kind_alignment k = Ctypes_primitives.alignment (prim_of_kind k)

type (_, _, _) dims =
| DimsGen : int array -> ('a, ('a, _, 'l) Bigarray.Genarray.t, 'l) dims
| Dims1 : int -> ('a, ('a, _, 'l) Bigarray.Array1.t, 'l) dims
| Dims2 : int * int -> ('a, ('a, _, 'l) Bigarray.Array2.t, 'l) dims
| Dims3 : int * int * int -> ('a, ('a, _, 'l) Bigarray.Array3.t, 'l) dims

type ('a, 'b, 'l) t = ('a, 'b, 'l) dims * 'a kind * 'l Bigarray.layout

let elements : type a b l. (b, a, l) dims -> int = function
  | DimsGen ds -> Array.fold_left ( * ) 1 ds
  | Dims1 d -> d
  | Dims2 (d1, d2) -> d1 * d2
  | Dims3 (d1, d2, d3) -> d1 * d2 * d3

let element_type (_, k, _) = prim_of_kind k

let dimensions : type a b l. (b, a, l) t -> int array = function
| DimsGen dims, _, _ -> dims
| Dims1 x, _, _ -> [| x |]
| Dims2 (x, y), _, _ -> [| x; y |]
| Dims3 (x, y, z), _, _ -> [| x; y; z |]

let sizeof (d, k, _) = elements d * bigarray_kind_sizeof k

let alignment (_, k, _) = bigarray_kind_alignment k

let bigarray ds k l = (DimsGen ds, kind k, l)
let bigarray1 d k l = (Dims1 d, kind k, l)
let bigarray2 d1 d2 k l = (Dims2 (d1, d2), kind k, l)
let bigarray3 d1 d2 d3 k l = (Dims3 (d1, d2, d3), kind k, l)

let type_name : type a b l. (b, a, l) dims -> string list = function
  | DimsGen _ -> ["Bigarray"; "Genarray"; "t"]
  | Dims1 _ -> ["Bigarray"; "Array1"; "t"]
  | Dims2 _ -> ["Bigarray"; "Array2"; "t"]
  | Dims3 _ -> ["Bigarray"; "Array3"; "t"]

let kind_type_names : type a. a kind -> _ = function
  | Kind_float32 ->
    (`Ident ["float"],
     `Ident ["Bigarray"; "float32_elt"])
  | Kind_float64 ->
    (`Ident ["float"],
     `Ident ["Bigarray"; "float64_elt"])
  | Kind_int8_signed ->
    (`Ident ["int"],
     `Ident ["Bigarray"; "int8_signed_elt"])
  | Kind_int8_unsigned ->
    (`Ident ["int"],
     `Ident ["Bigarray"; "int8_unsigned_elt"])
  | Kind_int16_signed ->
    (`Ident ["int"],
     `Ident ["Bigarray"; "int16_signed_elt"])
  | Kind_int16_unsigned ->
    (`Ident ["int"],
     `Ident ["Bigarray"; "int16_unsigned_elt"])
  | Kind_int32 ->
    (`Ident ["int32"],
     `Ident ["Bigarray"; "int32_elt"])
  | Kind_int64 ->
    (`Ident ["int64"],
     `Ident ["Bigarray"; "int64_elt"])
  | Kind_int ->
    (`Ident ["int"],
     `Ident ["Bigarray"; "int_elt"])
  | Kind_nativeint ->
    (`Ident ["nativeint"],
     `Ident ["Bigarray"; "nativeint_elt"])
  | Kind_complex32 ->
    (`Ident ["Complex"; "t"],
     `Ident ["Bigarray"; "complex32_elt"])
  | Kind_complex64 ->
    (`Ident ["Complex"; "t"],
     `Ident ["Bigarray"; "complex64_elt"])
  | Kind_char ->
    (`Ident ["char"],
     `Ident ["Bigarray"; "int8_unsigned_elt"])

let layout_path : type a. a Bigarray.layout -> string list =
  function
  | Bigarray.C_layout -> ["Bigarray"; "c_layout"]
  | Bigarray.Fortran_layout -> ["Bigarray"; "fortran_layout"]

let type_expression : type a b l. (a, b, l) t -> _ =
  fun (t, ck, l) ->
  begin
    let a, b = kind_type_names ck in
    let layout = `Ident (layout_path l) in
    (`Appl (type_name t, [a; b; layout]))
  end

let prim_of_kind k = prim_of_kind (kind k)

let unsafe_address b = Ctypes_bigarray_stubs.address b

let view : type a b l. (a, b, l) t -> _ Ctypes_ptr.Fat.t -> b =
  let open Ctypes_bigarray_stubs in
  fun (dims, kind, layout) ptr -> let ba : b = match dims with
  | DimsGen ds -> view kind ~dims:ds ptr layout
  | Dims1 d -> view1 kind ~dims:[| d |] ptr layout
  | Dims2 (d1, d2) -> view2 kind ~dims:[| d1; d2 |] ptr layout
  | Dims3 (d1, d2, d3) -> view3 kind ~dims:[| d1; d2; d3 |] ptr layout in
  match Ctypes_ptr.Fat.managed ptr with
  | None -> ba
  | Some src -> Gc.finalise (fun _ -> Ctypes_memory_stubs.use_value src) ba; ba
