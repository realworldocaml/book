(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the foreign value tests. *)

let cheader = "extern char **environ;"

let () = Tests_common.run ~cheader Sys.argv (module Functions.Stubs)
