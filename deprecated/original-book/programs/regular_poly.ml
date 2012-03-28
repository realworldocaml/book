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

class regular_poly n radius =
   let () = assert (n > 2) in
   let vertices = Array.create n (0, 0) in
   let step = 6.28 /. float_of_int n in
   let radius = float_of_int radius in
   let () =
      for i = 0 to n - 1 do
         let theta = float_of_int i *. step in
         let x = int_of_float (cos theta *. radius) in
         let y = int_of_float (sin theta *. radius) in
         vertices.(i) <- (x + 100, y + 100)
      done
   in
   object
      method draw = Graphics.fill_poly vertices
   end;;

Graphics.open_graph " 200x200";;
let p = new regular_poly 7 100;;
p#draw;;
ignore (Graphics.wait_next_event [Graphics.Button_down]);;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
