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

type ordering = Smaller | Equal | Larger

class type ['a] tree =
  object ('self)
    method add : 'a -> 'a tree
    method mem : 'a -> bool
end;;

class ['a] node (compare : 'a -> 'a -> ordering)
    (x : 'a) (l : 'a tree) (r : 'a tree) =
  object (self : 'self)
    val label = x
    val left = l
    val right = r
    method mem y =
      match compare y label with
         Smaller -> left#mem y
       | Larger -> right#mem y
       | Equal -> true
    method add y =
      match compare y label with
         Smaller -> {< left = left#add y >}
       | Larger -> {< right = right#add y >}
       | Equal -> self
  end;;

class ['a] leaf (compare : 'a -> 'a -> ordering) =
  object (self : 'self)
    method mem (_ : 'a) = false
    method add x =
      new node compare x (self :> 'a tree) (self :> 'a tree)
  end;;

module type CompareSig = sig
   type t
   val compare : t -> t -> ordering
end;;

module MakeTree (Compare : CompareSig)
: sig val empty : Compare.t tree end =
struct
   open Compare
   type key = Compare.t
   type t = key tree

   class node x (l : t) (r : t) =
     object (self : 'self)
       val label = x
       val left = l
       val right = r
       method mem x =
         match compare x label with
            Smaller -> left#mem x
          | Larger -> right#mem x
          | Equal -> true
       method add x =
         match compare x label with
            Smaller -> {< left = left#add x >}
          | Larger -> {< right = right#add x >}
          | Equal -> self
   end;;

   class leaf =
     object (self : 'self)
       method mem _ = false
       method add x = new node x (self :> t) (self :> t)
     end;;

   let empty = new leaf;;
end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
