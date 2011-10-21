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

(* An element in the list is a triple (value, previous, next) *)
type 'a elem = 'a * 'a elem option ref * 'a elem option ref

(* The list is a reference to the first element *)
type 'a dllist = 'a elem option ref

let create () = ref None
let prev (_, p, _) = p
let next (_, _, n) = n

(* Add a new element to the front of the list *)
let add dll x =
   let elem1 = (x, ref None, ref None) in
      (match !dll with
          Some elem2 ->
             prev elem2 := Some elem1;
             next elem1 := Some elem2
        | None ->
             ());
      dll := Some elem1;
      elem1

(* Delete an element from the list *)
let delete (dll : 'a dllist) (elem : 'a elem) =
    (match !(next elem) with
        Some next_elem ->
            prev next_elem := !(prev elem)
      | None ->
            ());
    (match !(prev elem) with
        Some prev_elem ->
            next prev_elem := !(next elem)
      | None ->
            ());
    (match !dll with
        Some elem2 when elem2 == elem ->
           dll := !(next elem)
      | _ ->
           ())
(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
