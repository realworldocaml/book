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
module type EqualSig = sig
   type t
   val equal : t -> t -> bool
end;;

module type SetSig = sig
   type t
   type elt
   val empty : t
   val mem : elt -> t -> bool
   val add : elt -> t -> t
   val find : elt -> t -> elt
end;;

module MakeSet (Equal : EqualSig)
: SetSig with type elt = Equal.t  =
struct
   open Equal
   type elt = Equal.t
   type t = elt list
   let empty = []
   let rec mem x s =
      List.exists (equal x) s
   let add x l = x :: l
   let find x s =
      List.find (equal x) s
end;;

module type ValueSig = sig
   type value
end;;

module type MapSig = sig
   type t
   type key
   type value
   val empty : t
   val add : t -> key -> value -> t
   val find : t -> key -> value
end;;

module MakeMap
 (Equal : EqualSig) (Value : ValueSig)
 : MapSig
   with type key = Equal.t
   with type value = Value.value
= struct
   type key = Equal.t
   type value = Value.value
   type item =
      Key of key
    | Pair of key * value
   module EqualItem = struct
      type t = item
      let equal
         (Key key1 | Pair (key1, _))
         (Key key2 | Pair (key2, _)) =
         Equal.equal key1 key2
   end;;
   module Set = MakeSet (EqualItem);;
   type t = Set.t
   let empty = Set.empty
   let add map key value =
      Set.add (Pair (key, value)) map
   let find map key =
      match Set.find (Key key) map with
         Pair (_, value) -> value
       | Key _ ->
            raise (Invalid_argument "find")
end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
