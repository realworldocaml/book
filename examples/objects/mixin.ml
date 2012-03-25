(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2012 Jason Hickey
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
 * @email{jasonh@gmail.com}
 * @end[license]
 *)

type 'a iterator = < get : 'a; has_value : bool; next : unit >;;

  class ['a] node x =
  object
    val mutable value : 'a = x
    val mutable next_node : 'a node option = None

    method get = value
    method set x = value <- x

    method next = next_node
    method set_next node = next_node <- node
  end;;

  class ['a] slist_iterator cur =
  object
    val mutable current : 'a node option = cur

    method has_value = current <> None

    method get =
       match current with
          Some node -> node#get
        | None -> raise (Invalid_argument "no value")

    method next =
       match current with
          Some node -> current <- node#next
        | None -> raise (Invalid_argument "no value")
  end;;

  class ['a] slist =
  object
     val mutable first : ('a) node option = None
     val mutable last : ('a) node option = None

     method is_empty = first = None

     method insert x =
        let new_node = Some (new node x) in
        match last with
           Some last_node ->
              last_node#set_next new_node;
              last <- new_node
         | None ->
              first <- new_node;
              last <- new_node

     method iterator = (new slist_iterator first :> 'a iterator)
  end;;

class virtual int_sum_mixin =
object (self : 'self)
   method virtual iterator : int iterator
   method sum =
      let it = self#iterator in
      let total = ref 0 in
      while it#has_value do
         total := !total + it#get;
         it#next
      done;
      !total
end;;

class int_slist =
object
  inherit [int] slist
  inherit int_sum_mixin
end;;

class virtual ['a] fold_mixin =
object (self : 'self)
   method virtual iterator : 'a iterator
   method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b =
      (fun f x ->
            let y = ref x in
            let it = self#iterator in
            while it#has_value do
               y := f !y it#get;
               it#next
            done;
            !y)
end;;

class ['a] slist_with_fold =
object
   inherit ['a] slist
   inherit ['a] fold_mixin
end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
