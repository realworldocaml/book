(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for binding to libffi. *)

open Ctypes_ptr

(* The type of structure types *)
type 'a ffitype = voidp
type struct_ffitype

external primitive_ffitype : 'a Ctypes_primitive_types.prim -> 'a ffitype
 = "ctypes_primitive_ffitype"

external pointer_ffitype : unit -> voidp ffitype
 = "ctypes_pointer_ffitype"

external void_ffitype : unit -> unit ffitype
 = "ctypes_void_ffitype"


(* Allocate a new C typed buffer specification *)
external allocate_struct_ffitype : int -> struct_ffitype
  = "ctypes_allocate_struct_ffitype"

external struct_type_set_argument : struct_ffitype -> int -> _ ffitype -> unit
  = "ctypes_struct_ffitype_set_argument"

(* Produce a structure type representation from the buffer specification. *)
external complete_struct_type : struct_ffitype -> unit
  = "ctypes_complete_structspec"

external ffi_type_of_struct_type : struct_ffitype -> _ ffitype
  = "ctypes_block_address"

(* A specification of argument C-types and C-return values *)
type callspec

(* Allocate a new C call specification *)
external allocate_callspec : check_errno:bool -> runtime_lock:bool ->
  thread_registration:bool -> callspec
  = "ctypes_allocate_callspec"

(* Add an argument to the C buffer specification *)
external add_argument : callspec -> _ ffitype -> int
  = "ctypes_add_argument"

(* Pass the return type and conclude the specification preparation *)
external prep_callspec : callspec -> int -> _ ffitype -> unit
  = "ctypes_prep_callspec"

(* Call the function specified by `callspec' at the given address.
   The callback functions write the arguments to the buffer and read
   the return value. *)
external call : string -> _ Ctypes_static.fn Fat.t -> callspec ->
  (voidp -> (Obj.t * int) array -> unit) -> (voidp -> 'a) -> 'a
  = "ctypes_call"


(* nary callbacks *)
type boxedfn =
  | Done of (voidp -> unit) * callspec
  | Fn of (voidp -> boxedfn)

type funptr_handle

(* Construct a pointer to an OCaml function represented by an identifier *)
external make_function_pointer : callspec -> int -> funptr_handle
  = "ctypes_make_function_pointer"

external raw_address_of_function_pointer : funptr_handle -> voidp
  = "ctypes_raw_address_of_function_pointer"

(* Set the function used to retrieve functions by identifier. *)
external set_closure_callback : (int -> Obj.t) -> unit
  = "ctypes_set_closure_callback"


(* An internal error: for example, an `ffi_type' object passed to ffi_prep_cif
   was incorrect. *)
exception Ffi_internal_error of string
let () = Callback.register_exception "FFI_internal_error"
  (Ffi_internal_error "")

(* A closure passed to C was collected by the OCaml garbage collector before
   it was called. *)
exception CallToExpiredClosure
let () = Callback.register_exception "CallToExpiredClosure"
  CallToExpiredClosure
