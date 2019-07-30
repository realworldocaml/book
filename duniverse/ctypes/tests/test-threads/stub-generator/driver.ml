(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the threads tests. *)

let () = Tests_common.run ~concurrency:Cstubs.unlocked Sys.argv (module Functions.Stubs)
