(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the builtins tests. *)

open Ctypes

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  (* *ptr |= value; return *ptr; *)
  let __sync_or_and_fetch = foreign "__sync_or_and_fetch"
    (ptr uint8_t @-> uint8_t @-> returning uint8_t)

  (* tmp = *ptr; *ptr &= value; return tmp; *)
  let __sync_fetch_and_and = foreign "__sync_fetch_and_and"
    (ptr uint8_t @-> uint8_t @-> returning uint8_t)
end
