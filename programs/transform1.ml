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

class type transform_interface =
object
   method transform : (float * float) -> (float * float)
end

module type TransformSig =
sig
   type 'a t
   type 'a transform = 'a t constraint 'a = #transform_interface

   val new_scale : float -> float -> 'a transform
end

module Transform : TransformSig =
struct
   class transform_implementation matrix =
   object (self : 'self)
      val matrix : float * float * float * float * float * float = matrix
      method transform (p : float * float) = p
      method representation = matrix
      method multiply (t2 : 'self) = t2
   end

   type 'a t = 'a constraint 'a = transform_implementation
   type 'a transform = 'a t constraint 'a = #transform_interface

   let new_scale sx sy =
      (new transform_implementation (sx, 0., 0., 0., sy, 0.) :> #transform_interface)
end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
