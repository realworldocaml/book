(*
 * Copyright (c) 2017 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Stubs(S : Ctypes.TYPE) =
struct
  open S
  let fruit : int64 S.typ = enum "fruit" []

  let bears_t : int64 S.typ = enum "bears_t" []
      ~typedef:true

  let letter_t : int64 S.typ = typedef (enum "letter" []) "letter_t"
end
