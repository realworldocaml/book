(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for standard views. *)

(* Convert a C string to an OCaml string *)
external string_of_cstring : char Ctypes_static.typ Ctypes_ptr.Fat.t -> string
  = "ctypes_string_of_cstring"

(* Convert an OCaml string to a C string *)
external cstring_of_string : string -> Ctypes_memory_stubs.managed_buffer
  = "ctypes_cstring_of_string"

(* Size information for uintptr_t *)
external uintptr_t_size : unit -> int = "integers_uintptr_t_size"

(* Size information for uintptr_t *)
external intptr_t_size : unit -> int = "integers_intptr_t_size"

(* Size information for ptrdiff_t *)
external ptrdiff_t_size : unit -> int = "integers_ptrdiff_t_size"
