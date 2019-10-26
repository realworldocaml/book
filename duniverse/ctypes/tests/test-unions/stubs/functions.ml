(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the union tests. *)

open Ctypes

type padded
let padded : padded union typ = union "padded"
let (-:) ty label = field padded label ty
let i = int64_t                         -: "i"
let a = array (sizeof int64_t + 1) char -: "a"
let () = seal padded

(* These functions can be bound either dynamically using Foreign or statically
   using stub generation. *)
module Common (F: Ctypes.FOREIGN) =
struct
  let sum_union_components =
    F.(foreign "sum_union_components"
         (ptr padded @-> size_t @-> returning int64_t))
end

(* These functions can only be bound using stub generation, since Foreign
   doesn't support passing unions by value. *)
module Stubs_only(F : Ctypes.FOREIGN) =
struct
  let add_unions =
    F.(foreign "add_unions"
         (padded @-> padded @-> returning padded))
end

module Stubs (F: Ctypes.FOREIGN) =
struct
  include Common(F)
  include Stubs_only(F)
end
