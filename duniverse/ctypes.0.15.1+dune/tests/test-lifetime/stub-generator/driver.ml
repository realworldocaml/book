(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the lifetime tests. *)

let () = Tests_common.run Sys.argv (module Functions.Stubs)
  ~concurrency:Cstubs.unlocked
