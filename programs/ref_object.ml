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
let int_coord (x, y) = (int_of_float x, int_of_float y);;

class poly vertices =
object
   val vertices = vertices
   method draw = Graphics.fill_poly (Array.map int_coord vertices)
   method transform matrix =
      {< vertices = Array.map matrix#transform vertices >}
end;;

type fcoord = float * float;;

class poly vertices =
object
   val vertices = vertices
   method draw = Graphics.fill_poly (Array.map int_coord vertices)
   method transform (matrix : < transform : fcoord -> fcoord >) =
      {< vertices = Array.map matrix#transform vertices >}
end;;

class poly vertices =
object (self : 'self)
   val vertices = vertices
   method draw = Graphics.fill_poly (Array.map int_coord vertices)
   method transform : 'a. (< transform : fcoord -> fcoord; .. > as 'a) -> 'self =
      (fun matrix -> {< vertices = Array.map matrix#transform vertices >})
end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
