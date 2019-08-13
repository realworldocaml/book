(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the type printing tests. *)

let () = Tests_common.run Sys.argv
   ~structs:(module Types.Stubs)
   (module functor (B:Ctypes.FOREIGN) -> struct end)

