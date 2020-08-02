(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for reading and writing memory. *)

open Ctypes_ptr

[@@@warning "-3"] (* TODO check when @@noalloc was introduced to ocaml *)

(* A reference, managed by the garbage collector, to a region of memory in the
   C heap. *)
type managed_buffer

(* Allocate a region of stable, zeroed memory managed by a custom block. *)
external allocate : int -> int -> managed_buffer
  = "ctypes_allocate"

(* Obtain the address of the managed block. *)
external block_address : managed_buffer -> voidp
  = "ctypes_block_address"

(* Read a C value from a block of memory *)
external read : 'a Ctypes_primitive_types.prim -> _ Fat.t -> 'a
  = "ctypes_read"

(* Write a C value to a block of memory *)
external write : 'a Ctypes_primitive_types.prim -> 'a -> _ Fat.t -> unit
  = "ctypes_write" [@@noalloc]

module Pointer =
struct
  external read : _ Fat.t -> voidp
    = "ctypes_read_pointer"

  external write : _ Fat.t -> _ Fat.t -> unit
  = "ctypes_write_pointer"
end

(* Copy [size] bytes from [src] to [dst]. *)
external memcpy : dst:_ Fat.t -> src:_ Fat.t -> size:int -> unit
  = "ctypes_memcpy"

(* Read a fixed length OCaml string from memory *)
external string_of_array : _ Fat.t -> len:int -> string
  = "ctypes_string_of_array"

(* Do nothing, concealing from the optimizer that nothing is being done. *)
external use_value : 'a -> unit
  = "ctypes_use" [@@noalloc]
