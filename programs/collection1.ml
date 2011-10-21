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
class type element = object method print : unit end;;

class type collection =
object
   method length : int
end;;

class type enumerable_collection =
  object
     inherit collection
     method iter : (element -> unit) -> unit
     method fold : ('a -> element -> 'a) -> 'a -> 'a
end;;

class list_collection =
object
   val mutable elements : element list = []
   method length = List.length elements

   method add x = elements <- x :: elements
   method remove = elements <- List.tl elements

   method iter f = List.iter f elements
(*
   method fold f x = List.fold_left f x elements
 *)
   method fold : 'a. ('a -> element -> 'a) -> 'a -> 'a =
      (fun f x -> List.fold_left f x elements)
end;;

class array_collection size init =
object
   val elements = Array.create size init
   method length = size
   method set i x = elements.(i) <- x
   method get i = elements.(i)
   method iter f = Array.iter f elements
   method fold : 'a. ('a -> element -> 'a) -> 'a -> 'a =
      (fun f x ->
         let rec loop i x =
            if i = size then x else loop (i + 1) (f x elements.(i))
         in
         loop 0 x)
end;;

let l = new list_collection;;
(l :> enumerable_collection);;

module A : sig end =
struct
class virtual collection =
object
   method virtual length : int
end;;

class virtual enumerable_collection =
object (self : 'self)
   inherit collection
   method virtual iter : (element -> unit) -> unit
   method virtual fold : 'a. ('a -> element -> 'a) -> 'a -> 'a
   method print = self#iter (fun element -> element#print)
end;;

class list_collection =
object
   inherit enumerable_collection

   val mutable elements : element list = []
   method length = List.length elements
   method add x = elements <- x :: elements
   method remove = elements <- List.tl elements
   method iter f = List.iter f elements
   method fold : 'a. ('a -> element -> 'a) -> 'a -> 'a =
      (fun f x -> List.fold_left f x elements)
end;;

class array_collection size init =
object
   val elements = Array.create size init
   method length = size
   method set i x = elements.(i) <- x
   method get i = elements.(i)
   method iter f = Array.iter f elements
   method fold : 'a. ('a -> element -> 'a) -> 'a -> 'a =
      (fun f x ->
         let rec loop i x =
            if i = size then x else loop (i + 1) (f x elements.(i))
         in
         loop 0 x)
end;;

let dummy = object method print = () end

class bounded_stack1 size =
object
   inherit array_collection size dummy
   val mutable index = 0
   method push x =
      if index = size then
         raise (Failure "stack is full");
      elements.(index) <- x;
      index <- index + 1
   method pop =
      if index = 0 then
         raise (Failure "stack is empty");
      index <- index - 1;
      elements.(index)
end;;

class bounded_stack2 size =
object (self : 'self)
   inherit bounded_stack1 size
   method virtual private set : _
   method virtual private get : _
end;;

end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
