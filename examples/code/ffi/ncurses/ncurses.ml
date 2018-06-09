open Ctypes

type window = unit ptr
let window : window typ = ptr void


[@@@part "1"];;
open Foreign

let libncurses = Dl.(dlopen ~filename:"libncursesw.so" ~flags:[RTLD_NOW])

let initscr =
  foreign "initscr" (void @-> returning window) ~from:libncurses


[@@@part "2"];;
let newwin =
  foreign "newwin"  ~from:libncurses
    (int @-> int @-> int @-> int @-> returning window)

let endwin =
  foreign "endwin" (void @-> returning void) ~from:libncurses

let refresh =
  foreign "refresh" (void @-> returning void) ~from:libncurses

let wrefresh =
  foreign "wrefresh" (window @-> returning void) ~from:libncurses

let addstr =
  foreign "addstr" (string @-> returning void) ~from:libncurses

let mvwaddch =
  foreign "mvwaddch" ~from:libncurses
    (window @-> int @-> int @-> char @-> returning void)

let mvwaddstr =
  foreign "mvwaddstr" ~from:libncurses
    (window @-> int @-> int @-> string @-> returning void)

let box =
  foreign "box" (window @-> char @-> char @-> returning void) ~from:libncurses

let cbreak =
  foreign "cbreak" (void @-> returning int) ~from:libncurses
