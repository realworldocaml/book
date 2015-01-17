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
open Core

type 'a iterator =
   < has_value : bool; value : 'a; next : unit; remove : unit >

module ConcurrentHashMap : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> key:'a -> 'b option
  val remove : ('a, 'b) t -> key:'a -> unit
  val iterator : ('a, 'b) t -> ('a * 'b) iterator
end = struct
  type ('a, 'b) element = {
    key : 'a;
    mutable value : 'b
  }
  type ('a, 'b) t = {
    locks : Mutex.t array;
    mutable buckets : ('a, 'b) element list array
  }

  let num_locks = 32
  let num_buckets = 256

  let create () = {
    locks = Array.init num_locks (fun _ -> Mutex.create ());
    buckets = Array.create num_buckets []
  }

  let rec find_assoc key = function
  | element :: tl ->
       if element.key = key then
          Some element
       else
          find_assoc key tl
  | [] -> None

  let rec remove_assoc_exn key = function
  | element :: tl ->
       if element.key = key then
          tl
       else
          element :: remove_assoc_exn key tl
  | [] -> raise Not_found

  let synchronize table index f =
    let lock = table.locks.(index * num_locks / num_buckets) in
    Mutex.lock lock;
    let result = f () in
    Mutex.unlock lock;
    result

  let synchronize_exn table index f =
    let lock = table.locks.(index * num_locks / num_buckets) in
    Mutex.lock lock;
    try let result = f () in Mutex.unlock lock; result with
    | exn -> Mutex.unlock lock; raise exn

  let add table ~key ~data =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    let buckets = table.buckets in
    synchronize table index (fun () ->
          match find_assoc key buckets.(index) with
          | Some element ->
               element.value <- data
          | None ->
               buckets.(index) <- { key = key; value = data } :: buckets.(index))

  let remove table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    let buckets = table.buckets in
    synchronize table index (fun () ->
      try buckets.(index) <- remove_assoc_exn key buckets.(index) with
      | Not_found -> ())

  let find table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    (* Unsynchronized *)
    match find_assoc key table.buckets.(index) with
    | Some element -> Some element.value
    | None -> None

  let rec remove_element_exn elements = function
  | (hd :: tl) as elements' ->
       if elements' == elements then
          tl
       else
          hd :: remove_element_exn elements tl
  | [] -> raise Not_found

  let iterator table =
    let buckets = table.buckets in
    object (self)
      val mutable index = 0
      val mutable elements = buckets.(0)
      method has_value = elements <> []
      method value =
        match elements with
        | { key = key; value = value } :: _ -> key, value
        | [] -> raise (Invalid_argument "value")
      method next =
        elements <- List.tl elements;
        self#normalize
      method remove =
        synchronize table index (fun () ->
          try buckets.(index) <- remove_element_exn elements buckets.(index) with
            Not_found -> ());
        self#next
      method private normalize =
        while elements = [] && index < num_buckets do
          index <- index + 1;
	  elements <- buckets.(index)
        done
      initializer self#normalize
    end
end

let table = ConcurrentHashMap.create ();;

ConcurrentHashMap.add table ~key:"hello" ~data:"world";;

Printf.printf "Value: %s\n"
    (Option.value_exn (ConcurrentHashMap.find table ~key:"hello"));;


(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
