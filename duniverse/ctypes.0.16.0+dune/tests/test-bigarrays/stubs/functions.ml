(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the bigarrays tests. *)

open Ctypes

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  let matrix_mul = foreign "matrix_mul"
    (int @-> int @-> int @->
     ptr double @-> ptr double @-> ptr double @->
     returning void)

  let matrix_transpose = foreign "matrix_transpose"
    (int @-> int @-> ptr double @-> returning (ptr double))
end
