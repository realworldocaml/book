(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Structs and unions whose layouts are computed from the sizes and alignment
    requirements of the constituent field types. *)

include Ctypes_structs.S
  with type ('a, 's) field := ('a, 's) Ctypes_static.field
