  $ (cd ../ffi/ncurses && corebuild -pkg ctypes.foreign -tag bin_annot ncurses.cmi)
  ocamlfind ocamldep -package ctypes.foreign -package core -ppx 'ppx-jane -as-ppx' -modules ncurses.mli > ncurses.mli.depends
  ocamlfind ocamlc -c -w A-4-33-40-41-42-43-34-44 -strict-sequence -g -bin-annot -short-paths -thread -package ctypes.foreign -package core -ppx 'ppx-jane -as-ppx' -o ncurses.cmi ncurses.mli
  $ ocp-index complete -I ../ffi Ncur
  Ncurses module
  $ ocp-index complete -I ../ffi Ncurses.a
  Ncurses.addstr val string -> unit
  $ ocp-index complete -I ../ffi Ncurses.
  Ncurses.window val window Ctypes.typ
  Ncurses.initscr val unit -> window
  Ncurses.endwin val unit -> unit
  Ncurses.refresh val unit -> unit
  Ncurses.wrefresh val window -> unit
  Ncurses.newwin val int -> int -> int -> int -> window
  Ncurses.mvwaddch val window -> int -> int -> char -> unit
  Ncurses.addstr val string -> unit
  Ncurses.mvwaddstr val window -> int -> int -> string -> unit
  Ncurses.box val window -> char -> char -> unit
  Ncurses.cbreak val unit -> int
