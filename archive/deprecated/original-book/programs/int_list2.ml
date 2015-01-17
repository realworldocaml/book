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

class type int_list =
object
    method is_nil : bool
    method hd : int
    method tl : int_list
    method set_hd : int -> unit
    method set_tl : int_list -> unit
    method iter : (int -> unit) -> unit
    method map : (int -> int) -> int_list
    method fold : 'a. ('a -> int -> 'a) -> 'a -> 'a
end

class nil =
object (self : #int_list as 'self)
   method is_nil = true
   method hd = raise (Invalid_argument "hd")
   method tl = raise (Invalid_argument "tl")
   method set_hd (_ : int) = raise (Invalid_argument "set_hd")
   method set_tl (_ : int_list) = raise (Invalid_argument "set_tl")
   method iter f = ()
   method map f = (self :> int_list)
   method fold f x = x
end

class cons hd tl =
object (self : #int_list as 'self)
   val mutable hd = hd
   val mutable tl = tl
   method is_nil = false
   method hd = hd
   method tl = tl
   method set_hd x = hd <- x
   method set_tl l = tl <- l
   method iter f = f hd; tl#iter f
   method map f = ({< hd = f hd; tl = tl#map f >} :> int_list)
   method fold f x = tl#fold f (f x hd)
end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
