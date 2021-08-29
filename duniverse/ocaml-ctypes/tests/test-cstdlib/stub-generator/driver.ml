(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the C standard library tests. *)

let cheader = "
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
"

let () = Tests_common.run ~cheader Sys.argv (module Functions.Stubs)
