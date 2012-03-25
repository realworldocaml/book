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
(* less-than: negative; equal: zero; greater-than: positive *)
type comparison = int

class virtual comparable =
object (self : 'self)
   method virtual compare : 'self -> comparison
   method less_than (x : 'self) = compare self x < 0
end;;

class virtual number =
object (self : 'self)
   method virtual zero : 'self
   method virtual neg : 'self
   method virtual compare : 'self -> comparison
   method abs =
      if self#compare self#zero < 0 then
         self#neg
      else
         self
end;;

class float_number x =
object (self : 'self)
   inherit comparable
   inherit number

   val number = x
   method representation = number
   method zero = {< number = 0.0 >}
   method neg = {< number = -. number >}
   method compare y = Pervasives.compare x y#representation
end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
