(*
 * Doubly-linked lists.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)

(* The type of elements in the list *)
type 'a elem

val nil_elem    : 'a elem
val create_elem : 'a -> 'a elem
val get         : 'a elem -> 'a
val prev_elem   : 'a elem -> 'a elem
val next_elem   : 'a elem -> 'a elem

(* The type of lists with elements of type 'a element *)
type 'a dllist

val create : unit -> 'a dllist
val insert : 'a dllist -> 'a elem -> unit
val remove : 'a dllist -> 'a elem -> unit

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
