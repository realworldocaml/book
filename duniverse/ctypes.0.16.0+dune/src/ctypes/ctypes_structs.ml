(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes_static

module type S =
sig
  type (_, _) field
  val field : 't typ -> string -> 'a typ ->
    ('a, (('s, [<`Struct | `Union]) structured as 't)) field
  val seal : (_, [< `Struct | `Union]) Ctypes_static.structured Ctypes_static.typ -> unit
end
