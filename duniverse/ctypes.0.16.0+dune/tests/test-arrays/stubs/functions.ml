(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the arrays tests. *)

open Ctypes

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  (* union u {
        int i;
        double d;
     }
  *)
  type number
  let u : number union typ = union "number"
  let (-:) ty label = field u label ty
  let i = int    -: "i"
  let d = double -: "d"
  let () = seal u

  (* struct s {
        char tag;
        union u data;
     }
  *)
  type tagged
  let s : tagged structure typ = structure "tagged"
  let (-:) ty label = field s label ty
  let tag  = char -: "tag"
  let data = u    -: "num"
  let () = seal s

  let accepts_pointer_to_array_of_structs =
    foreign "accepts_pointer_to_array_of_structs"
      (ptr (array 5 s) @-> returning double)
end
