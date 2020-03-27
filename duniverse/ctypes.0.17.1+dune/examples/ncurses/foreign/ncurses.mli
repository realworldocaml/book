(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type window

(** The ncurses library routines give the user a terminal-independent method of
     updating character screens with reasonable optimization. *)

(** initscr  is  normally  the  first  curses  routine  to  call when
  initializing a program. It determines the terminal type and initializes all
  data structures.  initscr also causes the first call to [refresh] to clear the
  screen.  If errors occur,  initscr  writes an appropriate error message to
  standard error and exits; otherwise, a pointer is returned to [window]. *)
val initscr : unit -> window

(** A program should always call [endwin] before exiting or escaping from curses
  mode temporarily.  This routine restores tty modes, moves the cursor to the lower
  left-hand corner of the screen and resets the terminal into the proper non-visual 
  mode. *)
val endwin : unit -> unit

(** [refresh] must be called to get actual output to the terminal, as other routines
  merely manipulate data structures. *)
val refresh : unit -> unit

(** [wrefresh window] must be called to get actual output to the terminal for a
  specific sub-window, as other routines merely manipulate data structures. *)
val wrefresh : window -> unit

(** Initially the terminal may or may not be in [cbreak] mode, as the mode is
  inherited; therefore, a program should call [cbreak] explicitly.  Most interactive
  programs will need to be in this mode. *)
val cbreak : unit -> unit

(** [newwin nlines ncols begin_y begin_x] creates and returns a pointer to a new
  [window] with the [nlines] lines and [ncols] columns. The upper left-hand corner
  of the window is at line [begin_y] and column [begin_x]. 
  A new full-screen window is created by calling [newwin 0 0 0 0] *)
val newwin : int -> int -> int -> int -> window

(** [addch ch] puts the character [ch] into the given window at its current window
  position, which is then advanced. *)
val addch : char -> unit

(** [addstr s] is analogous to calling [addch] for each character in [s] *)
val addstr : string -> unit

(** [mvwaddch win y x ch] puts the character [ch] into the given window at line [y]
  and column [x]. *)
val mvwaddch : window -> int -> int -> char -> unit

(** [mvwaddrstr win y x s] is analogous to calling [mvwaddch] for each character [ch]
  in [s] *)
val mvwaddstr : window -> int -> int -> string -> unit 

(** [box TODO TODO] draws a border around the [window] *)
val box : window -> int -> int -> unit
