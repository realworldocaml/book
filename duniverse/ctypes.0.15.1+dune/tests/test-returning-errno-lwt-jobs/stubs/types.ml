(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open PosixTypes

module Struct_stubs(S : Ctypes.TYPE) =
struct
  open S

  let _ENOENT = constant "ENOENT" sint

  let ifdir = constant "S_IFDIR" (lift_typ mode_t)
  let ifmt = constant "S_IFMT" (lift_typ mode_t)

  let stat : [`stat] structure typ = structure "stat"
  let st_mode = field stat "st_mode" (lift_typ mode_t)
  let () = seal stat
end
