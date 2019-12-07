(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the errno tests. *)

let cheader = "#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
"

let () = Tests_common.run ~cheader Sys.argv (module Functions.Stubs)
    ~structs:(module Types.Struct_stubs)
    ~errno:Cstubs.return_errno
