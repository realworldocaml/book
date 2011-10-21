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

(*
 * Imperative maps.
 *)
type comparison = Smaller | Equal | Larger

class type ['key, 'value] imp_map_type =
object ('self)
   method insert : 'key -> 'value -> unit
   method find   : 'key -> 'value
   method iter   : ('key -> 'value -> unit) -> unit
end

class ['key, 'value] imp_map (compare : 'key -> 'key -> comparison)
 : ['key, 'value] imp_map_type =
   let equal key1 (key2, _) = compare key1 key2 = Equal in
   object (self : 'self)
      val mutable elements : ('key * 'value) list = []
      method insert key value = elements <- (key, value) :: elements
      method find key = snd (List.find (equal key) elements)
      method iter f =
         List.iter (fun (key, value) -> f key value) elements
   end;;

(*
 * Animals.
 *)
class animal (name : string) =
object (self : 'self)
   method sleep = Printf.printf "%s sleeps.\n" name
end

class dog (name : string) =
object (self : 'self)
   inherit animal name
   method bark = Printf.printf "%s barks!\n" name
end

let compare_string (s1 : string) (s2 : string) =
   if s1 < s2 then Smaller
   else if s1 > s2 then Larger
   else Equal

let dogs = new imp_map compare_string;;
dogs#insert "Bob" (new dog "Spot");;
dogs#insert "Sue" (new dog "Rover");;

let goodnight (animals : (string, animal) imp_map) =
   animals#iter (fun _ animal -> animal#sleep);;

class ['a, 'b] pair (x0 : 'a) (y0 : 'b) =
object (self : 'self)
   val mutable x = x0
   val mutable y = y0
   method value = x, y
end;;

class ['a, 'b] mut_pair x0 y0 =
object (self : 'self)
   inherit ['a, 'b] pair x0 y0
   method set_fst x' = x <- x'
   method set_snd y' = y <- y'
end;;

class ['a] animal_pair x0 y0 =
object (self : 'self)
   inherit ['a, 'a] pair x0 y0
   method sleep = x#sleep; y#sleep
end;;

class ['a] animal_pairs =
  object (self : 'self)
    val mutable pairs : 'a animal_pair list = []
    method insert x0 y0 =
      pairs <- new animal_pair x0 y0 :: pairs
    method sleep = List.iter (fun p -> p#sleep) pairs
  end;;

class type ['a] animal_pairs_type =
  object ('self)
    constraint 'a = < sleep : unit; .. >
    method insert : 'a -> 'a -> unit
    method sleep : unit
  end;;

class type [+'a] ro_animal_pairs_type =
  object ('self)
    method sleep : unit
end;;

class ['a] alt_animal_pair (p : 'a) =
  object (self : 'self)
    constraint 'a = ('b, 'b) #pair
    constraint 'b = #animal
    method sleep =
      let a1, a2 = p#value in
      a1#sleep; a2#sleep
  end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
