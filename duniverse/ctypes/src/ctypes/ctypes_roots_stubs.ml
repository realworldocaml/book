(*
 * Copyright (c) 2015 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

external root : 'a -> Ctypes_ptr.voidp =
  "ctypes_caml_roots_create"

external set : Ctypes_ptr.voidp -> 'a -> unit =
  "ctypes_caml_roots_set"

external get : Ctypes_ptr.voidp -> 'a =
  "ctypes_caml_roots_get"

external release : Ctypes_ptr.voidp -> unit =
  "ctypes_caml_roots_release"
