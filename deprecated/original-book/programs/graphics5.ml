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
Graphics.open_graph " 200x200";;

module Foo : sig end = struct

let transform =
object
   val matrix = (1., 0., 0., 0., 1., 0.)
   method new_scale sx sy =
      {< matrix = (sx, 0., 0., 0., sy, 0.) >}
   method new_rotate theta =
      let s, c = sin theta, cos theta in
      {< matrix = (c, -.s, 0., s, c, 0.) >}
   method new_translate dx dy =
      {< matrix = (1., 0., dx, 0., 1., dy) >}
   method transform (x, y) =
      let (m11, m12, m13, m21, m22, m23) = matrix in
      (m11 *. x +. m12 *. y +. m13,
       m21 *. x +. m22 *. y +. m23)
   method representation = matrix
   method scale = let (sx, _, _, _, _, _) = matrix in sx
   method multiply matrix2 =
      let (x11, x12, x13, x21, x22, x23) = matrix in
      let (y11, y12, y13, y21, y22, y23) = matrix2#representation in
      {< matrix =
            (x11 *. y11 +. x12 *. y21,
             x11 *. y12 +. x12 *. y22,
             x11 *. y13 +. x12 *. y23 +. x13,
             x21 *. y11 +. x22 *. y21,
             x21 *. y12 +. x22 *. y22,
             x21 *. y13 +. x22 *. y23 +. x23)
      >}
end;;

let ( ** ) m1 m2 = m1#multiply m2;;

let int_coord (x, y) =
   int_of_float x, int_of_float y

type coord = float * float
type transform = < transform : coord -> coord >
type blob = < draw : unit; transform : transform -> blob >
type collection =
   < add : blob -> unit;
     draw : unit;
     transform : transform -> collection
   >

let new_poly (vertices : coord array) : blob =
object
   val vertices = vertices
   method draw = Graphics.fill_poly (Array.map int_coord vertices)
   method transform matrix =
      {< vertices = Array.map matrix#transform vertices >}
end;;

let new_circle center radius : blob =
object
   val center = center
   val radius = radius
   method draw =
      let x, y = int_coord center in
      Graphics.fill_circle x y (int_of_float radius)
   method transform matrix =
      {< center = matrix#transform center >}
end;;

let new_collection () : collection =
object
   val mutable items = []
   method add item = items <- item :: items
   method draw = List.iter (fun item -> item#draw) items
   method transform matrix =
      {< items = List.map (fun item -> item#transform matrix) items >}
end;;

let poly = new_poly [|(0.0, 0.2); (0.1, 0.5); (0.0, 1.0); (-0.1, 0.5)|];;
let star = new_collection ();;
star#add (new_circle (0.0, 0.0) 0.1);;
for i = 0 to 9 do
   let trans = transform#new_rotate (0.628 *. (float_of_int i)) in
   star#add (poly#transform (trans :> transform))
done;;

let image = new_collection ();;
image#add (new_circle (0.0, 0.0) 0.1);;
image#add (star : collection :> blob);;

let reflectY_transform = transform#new_scale (-1.0) 1.0;;

let new_collection () =
   object (self : 'self)
      val mutable items = []
      method add item = items <- item :: items
      method draw = List.iter (fun item -> item#draw) items
      method transform matrix = {< items = List.map (fun item -> item#transform matrix) items >}
      method reflectY = {< items = [self; self#transform reflectY_transform] >}
   end;;
end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
