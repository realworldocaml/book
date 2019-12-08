(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Bindings for the foreign value tests. *)

open Ctypes

module Common (F: Ctypes.FOREIGN) =
struct
  let s : [`global_struct] structure typ = structure "global_struct"
  let (-:) ty label = field s label ty
  let len = size_t       -: "len"
  let str = array 1 char -: "str"
  let () = seal s

  let global_struct = F.foreign_value "global_struct" s

  let plus =
    F.(foreign_value "plus_callback"
         (Foreign.funptr_opt Ctypes.(int @-> int @-> returning int)))

  let sum = F.(foreign "sum_range_with_plus_callback"
                 (int @-> int @-> returning int))

  let string_array = F.(foreign_value "string_array" (array 2 string))
  let int_array = F.(foreign_value "int_array" (bigarray array1 5 Bigarray.int32))
end


module Stubs (F: Ctypes.FOREIGN) =
struct
  include Common(F)
  let environ = F.(foreign_value "environ" (ptr string_opt))
end
