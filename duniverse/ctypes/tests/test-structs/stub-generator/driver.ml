(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the struct tests. *)

let () = Tests_common.run Sys.argv
   ~structs:(module Types.Struct_stubs)
   (module Functions.Stubs)
