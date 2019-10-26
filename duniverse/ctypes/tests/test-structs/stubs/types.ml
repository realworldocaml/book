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
  let s1 : [`s1] structure typ = structure "s1"
  let x1 = field s1 "x1" int
  let x4 = field s1 "x4" int
  let () = seal s1

  (* fields reordered *)
  let s2 : [`s2] structure typ = structure "s2"
  let y2 = field s2 "y2" int
  let y1 = field s2 "y1" int
  let () = seal s2

  (* one struct depending on another *)
  let s3 : [`s3] structure typ = structure "s3"
  let z1 = field s3 "z1" int
  let z2 = field s3 "z2" (ptr s3)
  let () = seal s3

  let s4 : [`s4] structure typ = structure "s4"
  let z3 = field s4 "z3" s3
  let z4 = field s4 "z4" (ptr s3)
  let () = seal s4

  (* dependencies involving function pointers *)

  (* (incomplete types are available in the present) *)
  let s1_fwd : [`s1] Ctypes.structure Ctypes.typ = Ctypes.structure "s1"

  let s5 : [`s5] structure typ = structure "s5"
  let w1 = field s5 "w1" (lift_typ (Foreign.funptr Ctypes.(ptr s1_fwd @-> returning int)))
  let () = seal s5

  (* adding fields through views (typedefs) *)
  let struct_s6 : [`s6] structure typ = structure ""
  let s6 = typedef struct_s6 "s6"
  let v1 = field s6 "v1" int
  let v2 = field s6 "v2" float
  let () = seal s6
end
