(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the struct tests. *)

open Ctypes

(* These functions can be bound either dynamically using Foreign or statically
   using stub generation. *)
module Common (F: Ctypes.FOREIGN) =
struct
  open F

  type simple
  let simple : simple structure typ = structure "simple"
  let i = field simple "i" int
  let f = field simple "f" double
  let self = field simple "self" (ptr simple)
  let () = seal simple

  let accept_struct = foreign "accept_struct"
    (simple @-> returning int)

  let return_struct = foreign "return_struct"
    (void @-> returning simple)

  (* Forward declarations *)
  let s1 : [`s1] structure typ = structure "s1"
  and s5 : [`s5] structure typ = structure "s5"

  let call_s5 = foreign "call_s5"
    (ptr s1 @-> ptr s5 @-> returning int);
end


(* These functions can only be bound using stub generation, since Foreign
   doesn't support passing structs with union or array members. *)
module Stubs_only(F : Ctypes.FOREIGN) =
struct
  open F

  type number
  let number : number union typ = union "number"
  let i = field number "i" int
  let d = field number "d" double
  let () = seal number

  type tagged
  let tagged : tagged structure typ = structure "tagged"
  let tag = field tagged "tag" char
  let num = field tagged "num" number
  let () = seal tagged

  type triple
  let triple : triple structure typ = structure "triple"
  let elements = field triple "elements" (array 3 double)
  let () = seal triple

  let add_tagged_numbers = foreign "add_tagged_numbers"
    (tagged @-> tagged @-> returning tagged)

  let add_triples = foreign "add_triples"
    (triple @-> triple @-> returning triple)
end


module Stubs (F: Ctypes.FOREIGN) =
struct
  include Common(F)
  include Stubs_only(F)
end
