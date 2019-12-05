(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Coercions *)

[@@@warning "-27"]

open Ctypes_static

type uncoercible_info =
    Types : _ typ * _ typ -> uncoercible_info
  | Functions : _ fn * _ fn -> uncoercible_info

exception Uncoercible of uncoercible_info

let show_uncoercible = function
    Uncoercible (Types (l, r)) ->
    let pr ty = Ctypes_type_printing.string_of_typ ty in
    Some (Format.sprintf
            "Coercion failure: %s is not coercible to %s" (pr l) (pr r))
  | Uncoercible (Functions (l, r)) ->
    let pr ty = Ctypes_type_printing.string_of_fn ty in
    Some (Format.sprintf
            "Coercion failure: %s is not coercible to %s" (pr l) (pr r))
  | _ -> None

let () = Printexc.register_printer show_uncoercible

let uncoercible : 'a 'b 'c. 'a typ -> 'b typ -> 'c =
  fun l r -> raise (Uncoercible (Types (l, r)))

let uncoercible_functions : 'a 'b 'c. 'a fn -> 'b fn -> 'c =
  fun l r -> raise (Uncoercible (Functions (l, r)))

let id x = x

type (_, _) coercion =
  | Id : ('a, 'a) coercion
  | Coercion : ('a -> 'b) -> ('a, 'b) coercion

let ml_prim_coercion :
  type a b. a Ctypes_primitive_types.ml_prim -> b Ctypes_primitive_types.ml_prim ->
  (a, b) coercion option =
  let open Ctypes_primitive_types in
  fun l r -> match l, r with
  | ML_char, ML_char -> Some Id
  | ML_complex, ML_complex -> Some Id
  | ML_float, ML_float -> Some Id
  | ML_int, ML_int -> Some Id
  | ML_int32, ML_int32 -> Some Id
  | ML_int64, ML_int64 -> Some Id
  | ML_llong, ML_llong -> Some Id
  | ML_long, ML_long -> Some Id
  | ML_nativeint, ML_nativeint -> Some Id
  | ML_size_t, ML_size_t -> Some Id
  | ML_uchar, ML_uchar -> Some Id
  | ML_bool, ML_bool -> Some Id
  | ML_uint, ML_uint -> Some Id
  | ML_uint16, ML_uint16 -> Some Id
  | ML_uint32, ML_uint32 -> Some Id
  | ML_uint64, ML_uint64 -> Some Id
  | ML_uint8, ML_uint8 -> Some Id
  | ML_ullong, ML_ullong -> Some Id
  | ML_ulong, ML_ulong -> Some Id
  | ML_ushort, ML_ushort -> Some Id
  | l, r -> None

let rec coercion : type a b. a typ -> b typ -> (a, b) coercion =
  fun atyp btyp -> match atyp, btyp with
  | _, Void -> Coercion ignore
  | Primitive l, Primitive r ->
    (match Ctypes_primitive_types.(ml_prim_coercion (ml_prim l) (ml_prim r)) with
       Some c -> c
     | None -> uncoercible atyp btyp)
  | View av, b ->
    begin match coercion av.ty b with
    | Id -> Coercion av.write
    | Coercion coerce -> Coercion (fun v -> coerce (av.write v))
    end
  | a, View bv ->
    begin match coercion a bv.ty with
    | Id -> Coercion bv.read
    | Coercion coerce -> Coercion (fun v -> bv.read (coerce v))
    end
  | Pointer a, Pointer b ->
    begin match coercion a b with
    | Id -> Id
    | Coercion _ ->
       Coercion (fun (CPointer p) -> CPointer (Ctypes_ptr.Fat.coerce p b))
    | exception Uncoercible _ ->
       Coercion (fun (CPointer p) -> CPointer (Ctypes_ptr.Fat.coerce p b))
    end
  | Pointer a, Funptr b ->
    Coercion (fun (CPointer p) -> Static_funptr (Ctypes_ptr.Fat.coerce p b))
  | Funptr a, Pointer b ->
    Coercion (fun (Static_funptr p) -> CPointer (Ctypes_ptr.Fat.coerce p b))
  | Funptr a, Funptr b ->
    begin match fn_coercion a b with
    | Id -> Id
    | Coercion _ ->
       Coercion (fun (Static_funptr p) -> Static_funptr (Ctypes_ptr.Fat.coerce p b))
    | exception Uncoercible _ ->
       Coercion (fun (Static_funptr p) -> Static_funptr (Ctypes_ptr.Fat.coerce p b))
    end
  | l, r -> uncoercible l r

and fn_coercion : type a b. a fn -> b fn -> (a, b) coercion =
  fun afn bfn -> match afn, bfn with
  | Function (af, at), Function (bf, bt) ->
    begin match coercion bf af, fn_coercion at bt with
    | Id, Id -> Id
    | Id, Coercion h ->
      Coercion (fun g x -> h (g x))
    | Coercion f, Id ->
      Coercion (fun g x -> g (f x))
    | Coercion f, Coercion h ->
      Coercion (fun g x -> h (g (f x)))
    end
  | Returns at, Returns bt -> coercion at bt
  | l, r -> uncoercible_functions l r

let coerce : type a b. a typ -> b typ -> a -> b =
  fun atyp btyp -> match coercion atyp btyp with
  | Id -> id
  | Coercion c -> c

let coerce_fn : type a b. a fn -> b fn -> a -> b =
  fun afn bfn -> match fn_coercion afn bfn with
  | Id -> id
  | Coercion c -> c
