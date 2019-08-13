(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the constants tests. *)

let cheader = "#include <limits.h>\n#include <stdbool.h>"

let () = Tests_common.run Sys.argv
   ~cheader
   ~structs:(module Types.Struct_stubs)
   (module functor (S: Cstubs.FOREIGN) -> struct end)
