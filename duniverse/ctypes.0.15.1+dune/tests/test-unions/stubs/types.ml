(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

module Struct_stubs(S : Ctypes.TYPE) =
struct
  open S

  (* missing fields *)
  let u1 : [`u1] union typ = union "u1"
  let x1 = field u1 "x1" char
  let () = seal u1

  (* adding fields through views (typedefs) *)
  let union_u2 : [`s7] union typ = union ""
  let u2 = typedef union_u2 "u2"
  let t1 = field u2 "t1" int
  let t2 = field u2 "t2" float
  let () = seal u2
end
