(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the bool tests. *)

open Ctypes

(* These functions can be bound either dynamically using Foreign or statically
   using stub generation. *)
module Common(F : Ctypes.FOREIGN) =
struct
  let bool_and = F.(foreign "bool_and" (bool @-> bool @-> returning bool))
end
