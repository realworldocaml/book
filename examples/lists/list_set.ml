(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2013 Jason Hickey
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
open Core.Std

module OrderedSet : sig
  type 'a t

  val empty : 'a t
  val of_list : 'a list -> 'a t
  val insert : 'a -> 'a t -> 'a t
  val member : 'a -> 'a t -> bool
  val union : 'a t -> 'a t -> 'a t
  val isect : 'a t -> 'a t -> 'a t
end = struct
  type 'a t = 'a list

  let empty = []
  let of_list l = List.sort ~cmp:Pervasives.compare l
  let member x l = List.exists ~f:((=) x) l
  let union = List.merge ~cmp:Pervasives.compare

  let rec insert x = function
    | [] -> [x]
    | (h :: t) as l ->
      if h < x then
        h :: insert x t
      else if h > x then
        x :: l
      else (* x = h *)
        l

  let rec isect l1 l2 =
    match l1, l2 with
    | i1 :: t1, i2 :: t2 ->
      if i1 < i2 then
        isect t1 l2
      else if i1 > i2 then
        isect l1 t2
      else (* i1 = i2 *)
        i1 :: isect t1 t2
    | [], _
    | _, [] -> []
end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
