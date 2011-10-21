(*
 * Sample programs for an object-based version of a
 * map.
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

type comparison = Smaller | Equal | Larger

class ['a] set (compare : 'a -> 'a -> comparison) =
   let equal x y = compare x y = Equal in
   object (self : 'self)
      val elements : 'a list = []
      method mem x = List.exists (equal x) elements
      method add x = {< elements = x :: elements >}
      method find x = List.find (equal x) elements
   end;;

class ['key, 'value] map1 (compare : 'key -> 'key -> comparison) =
   let equal key1 (key2, _) = compare key1 key2 = Equal in
   object (self : 'self)
      val elements : ('key * 'value) list = []
      method mem key = List.exists (equal key) elements
      method add key value = {< elements = (key, value) :: elements >}
      method find key = snd (List.find (equal key) elements)
   end;;

let compare_int (i : int) (j : int) =
   if i < j then Smaller
   else if i > j then Larger
   else Equal

(* Error: value restriction *)
(* let empty_int_map = new map1 compare_int *)

class ['key] empty_int_map = [int, 'key] map1 compare_int

class virtual ['key, 'value] map2 =
   object (self : 'self)
      val elements : ('key * 'value) list = []
      method mem key = List.exists (self#equal key) elements
      method add key value = {< elements = (key, value) :: elements >}
      method find key = snd (List.find (self#equal key) elements)

      method private equal key1 (key2, _) =
         self#compare key1 key2 = Equal
      method virtual compare : 'key -> 'key -> comparison
end;;

class ['key] int_map2 =
object (self : 'self)
   inherit [int, 'key] map2
   method compare i j = compare_int i j
end;;

class ['key, 'value] map3 (compare : 'key -> 'key -> comparison) =
   let equal key1 (key2, _) = compare key1 key2 = Equal in
   object (self : 'self)
      val mutable elements : ('key * 'value) list = []
      method clear = elements <- []
      method mem key = List.exists (equal key) elements
      method insert key value = elements <- (key, value) :: elements
      method find key = snd (List.find (equal key) elements)
   end;;


class type ['key, 'value] map_type =
object ('self)
   method mem : 'key -> bool
   method add : 'key -> 'value -> 'self
   method find : 'key -> 'value
end

class ['key, 'value] map (compare : 'key -> 'key -> comparison) : ['key, 'value] map_type =
   let equal key1 (key2, _) = compare key1 key2 = Equal in
   object (self : 'self)
      val elements : ('key * 'value) list = []
      method mem key = List.exists (equal key) elements
      method add key value = {< elements = (key, value) :: elements >}
      method find key = snd (List.find (equal key) elements)
   end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
