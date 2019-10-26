(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type uncoercible_info

exception Uncoercible of uncoercible_info

val coerce : 'a Ctypes_static.typ -> 'b Ctypes_static.typ -> 'a -> 'b

val coerce_fn : 'a Ctypes_static.fn -> 'b Ctypes_static.fn -> 'a -> 'b
