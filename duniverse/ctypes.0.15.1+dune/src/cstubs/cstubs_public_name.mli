(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Publicly visible names for type values *)

val ident_of_ml_prim : 'a Ctypes_primitive_types.ml_prim -> Ctypes_path.path
(* The type that should appear in the extern signature *)

val constructor_ident_of_prim : 'a Ctypes_primitive_types.prim -> Ctypes_path.path
(* The path to a value that represents the primitive type *)

val constructor_cident_of_prim :
  ?module_name:string -> 'a Ctypes_primitive_types.prim -> Ctypes_path.path
(* The path to a constructor that represents the primitive type *)
