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

class [+'a, +'b] pair (x : 'a) (y : 'b) =
object (self : 'self)
   method value : 'a * 'b = x, y
end

class [-'a, +'b] func (f : 'a -> 'b) =
object (self : 'self)
   method apply : 'a -> 'b = f
end

class type [+'a] olist =
object ('self)
   method is_nil : bool
   method hd : 'a
   method tl : ('a) olist
end

class [+'a] nil : ['a] olist =
object (self : 'self)
   method is_nil = true
   method hd : 'a = raise (Failure "hd")
   method tl : ('a) olist = raise (Failure "tl")
end

class [+'a] cons h t : ['a] olist =
object (self : 'self)
   method is_nil = false
   method hd : 'a = h
   method tl : ('a) olist = t
end

let l = new cons 1 (new cons 2 (new nil));;

class animal (name : string) =
object (self : 'self)
   method sleep = Printf.printf "%s sleeps.\n" name
end

class dog (name : string) =
object (self : 'self)
   inherit animal name
   method bark = Printf.printf "%s barks!\n" name
end

class type ['key, +'value] ro_map_type =
object ('self)
   method find : 'key -> 'value
end

class type ['key, 'value] map_type =
object ('self)
   method add  : 'key -> 'value -> 'self
   method find : 'key -> 'value
end

type comparison = Smaller | Equal | Larger

class ['key, 'value] map (compare : 'key -> 'key -> comparison) =
   let equal key1 (key2, _) = compare key1 key2 = Equal in
   object (self : 'self)
      val mutable elements : ('key * 'value) list = []
      method add key value = {< elements = (key, value) :: elements >}
      method find key = snd (List.find (equal key) elements)
   end;;

let compare s1 s2 =
   if s1 < s2 then Smaller
   else if s1 > s2 then Larger
   else Equal

let dogs =
   let table = new map (compare : string -> string -> comparison) in
   let table = table#add "Spot" (new dog "Spot") in
   let table = table#add "Rover" (new dog "Rover") in
   table

let ro_dogs = (dogs :> (string, dog) ro_map_type)
let animals : (string, animal) ro_map_type =
   (ro_dogs : (string, dog) ro_map_type :> (string, animal) ro_map_type)

class ['key, 'value] map2 (compare : 'key -> 'key -> comparison)
 : ['key, 'value] map_type =
   let equal key1 (key2, _) = compare key1 key2 = Equal in
   object (self : 'self)
      val elements : ('key * 'value) list = []
      method add key value = {< elements = (key, value) :: elements >}
      method find key = snd (List.find (equal key) elements)
   end;;

class ['key, 'value] map_map compare =
object (self : 'self)
   inherit ['key, 'value] map compare

   method map f =
      {< elements = List.map (fun (key, value) -> key, f value) elements >}
end

(************************************************************************
 * Imperative maps.
 *)
class type ['key, 'value] imp_map_type =
object ('self)
   method insert : 'key -> 'value -> unit
   method find   : 'key -> 'value
   method fold   : 'a. ('a -> 'key -> 'value -> 'a) -> 'a -> 'a
   method iter   : ('key -> 'value -> unit) -> unit
end

class ['key, 'value] imp_map (compare : 'key -> 'key -> comparison)
 : ['key, 'value] imp_map_type =
   let equal key1 (key2, _) = compare key1 key2 = Equal in
   object (self : 'self)
      val mutable elements : ('key * 'value) list = []
      method insert key value = elements <- (key, value) :: elements
      method find key = snd (List.find (equal key) elements)
      method fold : 'a. ('a -> 'key -> 'value -> 'a) -> 'a -> 'a =
         fun f x ->
            List.fold_left (fun x (key, value) ->
               f x key value) x elements
      method iter f =
         List.iter (fun (key, value) -> f key value) elements
   end;;

let dogs = new imp_map (compare : string -> string -> comparison);;
dogs#insert "Bob" (new dog "Spot");;
dogs#insert "Sue" (new dog "Rover");;

let goodnight animals =
   animals#iter (fun _ animal -> animal#sleep);;

goodnight dogs;;
(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
