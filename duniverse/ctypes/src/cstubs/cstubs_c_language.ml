(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C code representation. *)

[@@@warning "-9"]

open Ctypes_static

let fresh_var =
  let var_counter = ref 0 in
  fun ?(prefix="x") () ->
    incr var_counter;
    Printf.sprintf "%s%d" prefix !var_counter

type ty = Ty : _ typ -> ty
type tfn = Fn : _ fn -> tfn

type fieldname = string

type cfunction = {
  fname: string;
  allocates: bool;
  reads_ocaml_heap: bool;
  fn: tfn;
}

type cglobal = {
  name: string;
  typ: ty;
  references_ocaml_heap: bool;
}

type clocal = [ `Local of string * ty ]
type cvar = [ clocal | `Global of cglobal ]
type storage_class = [`Static | `Extern]
type cconst = [ `Int of Signed.sint ]
type cexp = [ cconst
            | clocal
            | `Cast of ty * cexp
            | `Addr of cvar ]
type clvalue = [ cvar
               | `Index of clvalue * cexp
               | `Field of clvalue * fieldname 
               | `PointerField of clvalue * fieldname ]
type camlop = [ `CAMLparam0
              | `CAMLlocalN of cexp * cexp
              | `CAMLdrop ]
type ceff = [ cexp
            | camlop
            | `Global of cglobal
            | `App of cfunction * cexp list
            | `Index of ceff * cexp
            | `Deref of cexp
            | `DerefField of cexp * fieldname ]
type cbind = clocal * ceff
type ccomp = [ ceff
             | `CAMLparam of string list * ccomp
             | `LetConst of clocal * cconst * ccomp
             | `LetAssign of clvalue * ceff * ccomp
             | `CAMLreturnT of ty * cexp
             | `Return of ty * cexp
             | `Let of cbind * ccomp ]
type cfundec = [ `Fundec of string * (string * ty) list * ty ]
type cfundef = [ `Function of cfundec * ccomp * storage_class ]

let rec return_type : type a. a fn -> ty = function
  | Function (_, f) -> return_type f
  | Returns t -> Ty t

let args : type a. a fn -> (string * ty) list = fun fn ->
  let rec loop : type a. a Ctypes.fn -> (string * ty) list = function
    | Ctypes_static.Function (ty, fn) -> (fresh_var (), Ty ty) :: loop fn
    | Ctypes_static.Returns _ -> []
  in loop fn

module Type_C =
struct
  let cexp : cexp -> ty = function
    | `Int _ -> Ty sint
    | `Local (_, ty) -> ty
    | `Cast (Ty ty, _) -> Ty ty
    | `Addr (`Global { typ = Ty ty }) -> Ty (Pointer ty)
    | `Addr (`Local (_,  Ty ty)) -> Ty (Pointer ty)

  let camlop : camlop -> ty = function
    | `CAMLparam0
    | `CAMLlocalN _
    | `CAMLdrop -> Ty Void

  let rec ceff : ceff -> ty = function
    | #cexp as e -> cexp e
    | #camlop as o -> camlop o
    | `Global { typ } -> typ
    | `App ({ fn = Fn f }, _) -> return_type f
    | `Index (e, _) -> reference_ceff e
    | `Deref e -> reference_ceff (e :> ceff)
    | `DerefField (e, f) -> field_ceff (e :> ceff) f
  and reference_ceff : ceff -> ty =
    fun e ->
      begin match ceff e with
      | Ty (Pointer ty) -> Ty ty
      | Ty (Array (ty, _)) -> Ty ty
      | Ty t -> Cstubs_errors.internal_error
        "dereferencing expression of non-pointer type %s"
        (Ctypes.string_of_typ t)
      end
  and field_ceff : ceff -> fieldname -> ty =
    fun e f ->
      begin match ceff e with
          Ty (Pointer (Struct { fields } as s)) -> lookup_field f s fields
        | Ty t -> Cstubs_errors.internal_error
          "accessing a field %s in an expression of type %s, which is not a pointer-to-struct type"
          f (Ctypes.string_of_typ t)
      end
  and lookup_field : type s a. string -> a typ -> s boxed_field list -> ty =
    fun f ty fields -> match fields with
        [] -> Cstubs_errors.internal_error
                "field %s not found in struct %s" f
                (Ctypes.string_of_typ ty)
      | BoxedField { ftype; fname } :: _ when fname = f -> Ty ftype
      | _ :: fields -> lookup_field f ty fields

  let rec ccomp : ccomp -> ty = function
    | #cexp as e -> cexp e
    | #ceff as e -> ceff e
    | `CAMLparam (_, c) -> ccomp c
    | `Let (_, c)
    | `LetConst (_, _, c) -> ccomp c
    | `LetAssign (_, _, c) -> ccomp c
    | `CAMLreturnT (ty, _) -> ty
    | `Return (ty, _) -> ty
end

let value : [`value] abstract typ = abstract ~name:"value" ~size:0 ~alignment:0

let reader fname fn =
  { fname; allocates = false; reads_ocaml_heap = true; fn = Fn fn }
let conser fname fn =
  { fname; allocates = true; reads_ocaml_heap = false; fn = Fn fn }
let immediater fname fn =
  { fname; allocates = false; reads_ocaml_heap = false; fn = Fn fn }

module Unchecked_function_types =
struct
  (* We're using an abstract type ([value]) as an argument and return type, so
     we'll use the [Function] and [Return] constructors directly.  The smart
     constructors [@->] and [returning] would reject the abstract type. *)
  let (@->) f t = Function (f, t)
  let returning t = Returns t
end

let prim_prj : type a. a Ctypes_primitive_types.prim -> _ =
  let open Unchecked_function_types in
  let open Ctypes_primitive_types in function
  | Char -> reader "Int_val" (value @-> returning int)
  | Schar -> reader "Int_val" (value @-> returning int)
  | Uchar -> reader "Uint8_val" (value @-> returning uint8_t)
  | Bool -> reader "Bool_val" (value @-> returning bool)
  | Short -> reader "Int_val" (value @-> returning int)
  | Int -> reader "Long_val" (value @-> returning int)
  | Long -> reader "ctypes_long_val" (value @-> returning long)
  | Llong -> reader "ctypes_llong_val" (value @-> returning llong)
  | Ushort -> reader "ctypes_ushort_val" (value @-> returning ushort)
  | Sint -> reader "ctypes_sint_val" (value @-> returning sint)
  | Uint -> reader "ctypes_uint_val" (value @-> returning uint)
  | Ulong -> reader "ctypes_ulong_val" (value @-> returning ulong)
  | Ullong -> reader "ctypes_ullong_val" (value @-> returning ullong)
  | Size_t -> reader "ctypes_size_t_val" (value @-> returning size_t)
  | Int8_t -> reader "Int_val" (value @-> returning int)
  | Int16_t -> reader "Int_val" (value @-> returning int)
  | Int32_t -> reader "Int32_val" (value @-> returning int32_t)
  | Int64_t -> reader "Int64_val" (value @-> returning int64_t)
  | Uint8_t -> reader "Uint8_val" (value @-> returning uint8_t)
  | Uint16_t -> reader "Uint16_val" (value @-> returning uint16_t)
  | Uint32_t -> reader "Uint32_val" (value @-> returning uint32_t)
  | Uint64_t -> reader "Uint64_val" (value @-> returning uint64_t)
  | Camlint -> reader "Long_val" (value @-> returning int)
  | Nativeint -> reader "Nativeint_val" (value @-> returning nativeint)
  | Float -> reader "Double_val" (value @-> returning double)
  | Double -> reader "Double_val" (value @-> returning double)
  | LDouble -> reader "ctypes_ldouble_val" (value @-> returning ldouble)
  | Complex32 -> reader "ctypes_float_complex_val" (value @-> returning complex32)
  | Complex64 -> reader "ctypes_double_complex_val" (value @-> returning complex64)
  | Complexld -> reader "ctypes_ldouble_complex_val" (value @-> returning complexld)

let prim_inj : type a. a Ctypes_primitive_types.prim -> _ =
  let open Unchecked_function_types in
  let open Ctypes_primitive_types in function
  | Char -> immediater "Ctypes_val_char" (int @-> returning value)
  | Schar -> immediater "Val_int" (int @-> returning value)
  | Uchar -> immediater "Integers_val_uint8" (uint8_t @-> returning value)
  | Bool -> immediater "Val_bool" (bool @-> returning value)
  | Short -> immediater "Val_int" (int @-> returning value)
  | Int -> immediater "Val_long" (int @-> returning value)
  | Long -> conser "ctypes_copy_long" (long @-> returning value)
  | Llong -> conser "ctypes_copy_llong" (llong @-> returning value)
  | Ushort -> conser "ctypes_copy_ushort" (ushort @-> returning value)
  | Sint -> conser "ctypes_copy_sint" (sint @-> returning value)
  | Uint -> conser "ctypes_copy_uint" (uint @-> returning value)
  | Ulong -> conser "ctypes_copy_ulong" (ulong @-> returning value)
  | Ullong -> conser "ctypes_copy_ullong" (ullong @-> returning value)
  | Size_t -> conser "ctypes_copy_size_t" (size_t @-> returning value)
  | Int8_t -> immediater "Val_int" (int @-> returning value)
  | Int16_t -> immediater "Val_int" (int @-> returning value)
  | Int32_t -> conser "caml_copy_int32" (int32_t @-> returning value)
  | Int64_t -> conser "caml_copy_int64" (int64_t @-> returning value)
  | Uint8_t -> immediater "Integers_val_uint8" (uint8_t @-> returning value)
  | Uint16_t -> immediater "Integers_val_uint16" (uint16_t @-> returning value)
  | Uint32_t -> conser "integers_copy_uint32" (uint32_t @-> returning value)
  | Uint64_t -> conser "integers_copy_uint64" (uint64_t @-> returning value)
  | Camlint -> immediater "Val_long" (int @-> returning value)
  | Nativeint -> conser "caml_copy_nativeint" (nativeint @-> returning value)
  | Float -> conser "caml_copy_double" (double @-> returning value)
  | Double -> conser "caml_copy_double" (double @-> returning value)
  | LDouble -> conser "ctypes_copy_ldouble" (ldouble @-> returning value)
  | Complex32 -> conser "ctypes_copy_float_complex" (complex32 @-> returning value)
  | Complex64 -> conser "ctypes_copy_double_complex" (complex64 @-> returning value)
  | Complexld -> conser "ctypes_copy_ldouble_complex" (complexld @-> returning value)
