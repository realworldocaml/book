(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Ctypes_static

include Ctypes_structs_computed

include Ctypes_type_printing

include Ctypes_memory

include Ctypes_std_views

include Ctypes_value_printing

include Ctypes_coerce

let lift_typ x = x

module type FOREIGN =
sig
  type 'a fn
  type 'a return
  val (@->) : 'a typ -> 'b fn -> ('a -> 'b) fn
  val returning : 'a typ -> 'a return fn

  type 'a result
  val foreign : string -> ('a -> 'b) fn -> ('a -> 'b) result
  val foreign_value : string -> 'a typ -> 'a ptr result
end

module type TYPE =
sig
  include Ctypes_types.TYPE

  type 'a const
  val constant : string -> 'a typ -> 'a const
  val enum : string -> ?typedef:bool ->
    ?unexpected:(int64 -> 'a) -> ('a * int64 const) list -> 'a typ
end
