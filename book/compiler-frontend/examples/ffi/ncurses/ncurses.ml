open Ctypes

type window = unit ptr
let window : window typ = ptr void

[@@@part "1"] ;;

open Foreign

let initscr =
  foreign "initscr" (void @-> returning window)

[@@@part "2"] ;;

let newwin =
  foreign "newwin"
    (int @-> int @-> int @-> int @-> returning window)

let endwin =
  foreign "endwin" (void @-> returning void)

let refresh =
  foreign "refresh" (void @-> returning void)

let wrefresh =
  foreign "wrefresh" (window @-> returning void)

let addstr =
  foreign "addstr" (string @-> returning void)

let mvwaddch =
  foreign "mvwaddch"
    (window @-> int @-> int @-> char @-> returning void)

let mvwaddstr =
  foreign "mvwaddstr"
    (window @-> int @-> int @-> string @-> returning void)

let box =
  foreign "box" (window @-> char @-> char @-> returning void)

let cbreak =
  foreign "cbreak" (void @-> returning int)
