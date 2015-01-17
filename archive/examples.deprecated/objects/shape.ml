(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2012 Jason Hickey
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
 * @email{jasonh@gmail.com}
 * @end[license]
 *)

open Slist
open Slist.SList

type shape = < area : float >;;

class square w =
object (self : 'self)
  method area = self#width *. self#width
  method width = w
end;;

class rectangle h w =
object (self : 'self)
   inherit square w
   method area = self#width *. self#height
   method height = h
end;;

let square_of_rectangle h w : square =
   (new rectangle h w :> square);;

let squares =
   let l = SList.make () in
   l#insert (new square 1.0);
   l#insert (new square 2.0);
   l

let total_area (l : shape slist) : float =
   let total = ref 0.0 in
   let it = l#iterator in
   while it#has_value do
      total := !total +. it#get#area;
      it#next
   done;
   !total

type readonly_shape_slist = < iterator : shape iterator >;;

let total_area (l : readonly_shape_slist) : float =
   let total = ref 0.0 in
   let it = l#iterator in
   while it#has_value do
      total := !total +. it#get#area;
      it#next
   done;
   !total;;

total_area (squares :> readonly_shape_slist);;

let total_area (l : < iterator : < area : float; .. > iterator; ..>) : float =
   let total = ref 0.0 in
   let it = l#iterator in
   while it#has_value do
      total := !total +. it#get#area;
      it#next
   done;
   !total;;

let total_area (l : < area : float; .. > slist) : float =
   let total = ref 0.0 in
   let it = l#iterator in
   while it#has_value do
      total := !total +. it#get#area;
      it#next
   done;
   !total;;

total_area squares;;

class cshape =
object
   method area : float = 0.0
end;;

let total_area (l : #cshape slist) : float =
   let total = ref 0.0 in
   let it = l#iterator in
   while it#has_value do
      total := !total +. it#get#area;
      it#next
   done;
   !total;;

total_area squares;;


(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
