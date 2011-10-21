(*
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

module MakeCell (T : sig type t end) =
struct
   class cell x =
   object
       val mutable x : T.t = x
       method private get = x
       method private set y = x <- y
   end
end

class type int_cons =
object
   method hd : int
   method tl : int_cons option
   method set_hd : int -> unit
   method set_tl : int_cons option -> unit
end

type int_list = int_cons option

module IntCell =
struct
   module Cell = MakeCell (struct type t = int end);;

   class type cell_type =
   object
      method private get_int : int
      method private set_int : int -> unit
   end

   class cell i : cell_type =
   object (self)
      inherit Cell.cell i
      method private get_int = self#get
      method private set_int = self#set
   end
end

module ListCell = MakeCell (struct type t = int_list end);;

class cons i : int_cons =
object
    inherit IntCell.cell i as value
    inherit ListCell.cell None as link

    method hd = value#get_int
    method tl = link#get
    method set_hd = value#set_int
    method set_tl = link#set
end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
