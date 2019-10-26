(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the coercion tests. *)

open Ctypes

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  let memchr = foreign "memchr"
    (ptr void @-> int @-> size_t @-> returning (ptr void))
end
