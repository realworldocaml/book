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
(* less-than: negative; equal: zero; greater-than: positive *)
type comparison = int

class type comparable =
object ('self)
   method compare : 'self -> comparison
end;;

let sort (l : #comparable list) =
   List.sort (fun x y -> x#compare y) l

module type CompareSig =
sig
   type t
   val compare : t -> t -> comparison
end;;

module type ComparableSig =
sig
   type t
   type rep

   class element : t ->
   object ('self)
       method representation : rep
       method compare : 'self -> comparison
   end
end;;

module MakeComparable (Compare : CompareSig)
: ComparableSig with type t = Compare.t =
struct
   type t = Compare.t
   type rep = Compare.t

   class element (x : t) =
   object (self : 'self)
      method representation = x
      method compare (item : 'self) =
         Compare.compare x item#representation
   end
end;;

module StringCompare =
struct
   type t = string
   let compare (s1 : string) (s2 : string) =
      if s1 < s2 then -1
      else if s1 > s2 then 1
      else 0
end;;

module StringComparable = MakeComparable (StringCompare);;

let strings =
   [new StringComparable.element "abc";
    new StringComparable.element "abd"];;

let strings' = sort strings;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
