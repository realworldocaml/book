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
   method head = List.hd elements
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

class virtual stack =
object (self : 'self)
   inherit collection
   method virtual push : element -> unit
   method virtual pop  : element
   method dup =
      let x = self#pop in
      self#push x;
      self#push x
   method swap =
      let x1 = self#pop in
      let x2 = self#pop in
      self#push x1;
      self#push x2
end;;

class bounded_stack size =
let dummy = object method print = () end in
object
   inherit stack

   val data = new array_collection size dummy
   val mutable index = 0

   method push x =
      if index = size then
         raise (Failure "stack is full");
      data#set index x;
      index <- index + 1
   method pop =
      if index = 0 then
         raise (Failure "stack is empty");
      index <- index - 1;
      data#get index
   method length = data#length
end;;

class unbounded_stack =
object (self : 'self)
   inherit list_collection
   inherit stack

   method push x = self#add x
   method pop =
      let x = self#head in
      self#remove;
      x
end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
