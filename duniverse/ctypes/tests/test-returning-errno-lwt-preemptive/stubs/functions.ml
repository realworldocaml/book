(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the errno / Lwt_preemptive tests. *)

open Ctypes

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  let struct_stat : [`stat] structure typ = structure "stat"
  let stat = foreign "stat"
      (string @-> ptr struct_stat @-> returning int)

  let sixargs = foreign "sixargs"
      (int @-> int @-> int @-> int @-> int @-> int @-> returning int)

  let return_10 = foreign "return_10"
      (void @-> returning int)

  let return_void = foreign "return_void"
      (ptr int @-> returning void)
end
