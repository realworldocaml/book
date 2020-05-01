(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the enum tests. *)

open Ctypes

(* These functions can only be bound using stub generation, since Foreign
   doesn't support passing enums. *)
module Stubs(F : Ctypes.FOREIGN) =
struct
  open F

  module T = Types.Struct_stubs(Generated_struct_bindings)

  let classify_integer = foreign "classify_integer"
      (int @-> returning T.signed)

  let out_of_range = foreign "out_of_range"
      (void @-> returning T.signed)

  let next_fruit = foreign "next_fruit"
      (T.fruit @-> returning T.fruit)
end
