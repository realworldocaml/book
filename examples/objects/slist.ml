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

module SList =
struct
  type 'a iterator = < get : 'a; has_value : bool; next : unit >
  type 'a t = < is_empty : bool; insert : 'a -> unit; iterator : 'a iterator >

  class ['a] node x =
  object
    val mutable value : 'a = x
    val mutable next_node : 'a node option = None

    method get = value
    method set x = value <- x

    method next = next_node
    method set_next node = next_node <- node
  end

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
  end

  class ['a] slist =
  object (self : 'self)
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
  end

  class ['a] slist2 =
  object (self : 'self)
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

     method iter (f : 'a -> unit) =
        let it = self#iterator in
        while it#has_value do
           f it#get;
           it#next
        done
  end

  let make () = new slist
end

module AbstractSList : sig
  type 'a iterator = < get : 'a; has_value : bool; next : unit >
  type 'a t = < is_empty : bool; insert : 'a -> unit; iterator : 'a iterator >

  val make : unit -> 'a t
end = SList

module VisibleSList : sig
  type 'a iterator = < get : 'a; has_value : bool; next : unit >
  type 'a t = < is_empty : bool; insert : 'a -> unit; iterator : 'a iterator >

  class ['a] node : 'a ->
  object
     method get : 'a
     method set : 'a -> unit
     method next : 'a node option
     method set_next : 'a node option -> unit
  end

  class ['a] slist_iterator : 'a node option ->
  object
     method has_value : bool
     method get : 'a
     method next : unit
  end

  class ['a] slist :
  object
    val mutable first : 'a node option
    val mutable last : 'a node option
    method is_empty : bool
    method insert : 'a -> unit
    method iterator : 'a iterator
  end

  val make : unit -> 'a slist
end = SList

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
