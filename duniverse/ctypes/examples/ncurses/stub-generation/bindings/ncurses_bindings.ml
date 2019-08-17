(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

type window = unit ptr
let window : window typ = ptr void

module Bindings (F : Ctypes.FOREIGN) =
struct
  open F

  let initscr =
    foreign "initscr" (void @-> (returning window))

  let endwin =
    foreign "endwin" (void @-> (returning void))

  let refresh =
    foreign "refresh" (void @-> (returning void))

  let wrefresh =
    foreign "wrefresh" (window @-> (returning void))

  let newwin =
    foreign "newwin" (int @-> int @-> int @-> int @-> (returning window))

  let addch =
    foreign "addch" (char @-> (returning void))

  let mvwaddch =
    foreign "mvwaddch" (window @-> int @-> int @-> char @-> (returning void))

  let addstr =
    foreign "addstr" (string @-> (returning void))

  let mvwaddstr =
    foreign "mvwaddstr" (window @-> int @-> int @-> string @-> (returning void))

  let box =
    foreign "box" (window @-> int @-> int @-> (returning void))

  let cbreak =
    foreign "cbreak" (void @-> (returning void))
end
