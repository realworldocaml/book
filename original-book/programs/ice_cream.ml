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
Random.init (Unix.getpid ());;

module Foo : sig end =
struct

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

class type transform =
object
   method transform : float * float -> float * float
end

class virtual colored_shape c =
object (self : 'self)
   method virtual draw_shape : unit
   method virtual transform : 'a. (#transform as 'a) -> 'self
   method draw =
      Graphics.set_color c;
      self#draw_shape
end

let int_coord (x, y) = (int_of_float x, int_of_float y)

class circle ~color center radius =
object (_ : 'self)
   inherit colored_shape color
   val center = center
   method draw_shape =
      let x, y = int_coord center in
      Graphics.fill_circle x y (int_of_float radius)
   method transform : 'a. (#transform as 'a) -> 'self =
      (fun matrix -> {< center = matrix#transform center >})
end

class poly ~color:c vertices =
object (_ : 'self)
   inherit colored_shape c
   val vertices = vertices
   method draw_shape = Graphics.fill_poly (Array.map int_coord vertices)
   method transform : 'a. (#transform as 'a) -> 'self =
       (fun matrix -> {< vertices = Array.map (fun v -> matrix#transform v) vertices >})
end

class drop = circle ~color:0x880022 (0., 0.) 10.
class sprinkle = poly ~color:0x220044 [|(0., 0.); (10., 0.); (10., 3.); (0., 3.)|]

class virtual drop_mixin1 =
object (self)
   method virtual add : 'a. (#colored_shape as 'a) -> unit
   method mixin1 = for i = 0 to 4 do self#add (new drop) done
end

class virtual sprinkle_mixin2 =
object (self)
   method virtual add : 'a. (#colored_shape as 'a) -> unit
   method mixin2 = for i = 0 to 200 do self#add (new sprinkle) done
end

class collection =
object
   val mutable items : colored_shape list = []
   method add : 'a. (#colored_shape as 'a) -> unit =
      (fun item -> items <- (item :> colored_shape) :: items)
   method map f = items <- List.map f items
   method draw = List.iter (fun item -> item#draw) items
end

class vanilla_ice_cream = object end

let recenter = transform#new_translate 100. 100.

class virtual mixed_ice_cream =
object (self)
   inherit vanilla_ice_cream
   inherit collection

   method virtual mixin1 : unit
   method virtual mixin2 : unit

   method stir =
      self#map (fun item ->
         let t = recenter ** transform#new_rotate (Random.float 6.28)
            ** transform#new_translate (Random.float 10.) (sqrt (Random.float 1e4))
         in item#transform t)

   initializer
      self#mixin1;
      self#mixin2;
      self#stir
end

class my_favorite_ice_cream =
object
   inherit mixed_ice_cream
   inherit drop_mixin1
   inherit sprinkle_mixin2
end;;

(new my_favorite_ice_cream)#draw;;

ignore (Graphics.wait_next_event [Graphics.Button_down]);;

end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
