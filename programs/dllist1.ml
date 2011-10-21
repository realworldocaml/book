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
open Format

type 'a elem =
   Nil
 | Elem of 'a * 'a elem ref * 'a elem ref

let nil_elem = Nil
let create_elem x = Elem (x, ref Nil, ref Nil)

let get = function
   Elem (x, _, _) -> x
 | Nil -> raise (Invalid_argument "get")

let prev_elem = function
   Elem (_, prev, _) -> !prev
 | Nil -> raise (Invalid_argument "prev_elem")

let next_elem = function
   Elem (_, _, next) -> !next
 | Nil -> raise (Invalid_argument "prev_elem")

type 'a dllist = 'a elem ref

let create () = ref Nil

let insert list elem =
   match elem, !list with
      Elem (_, prev, next), Nil ->
         prev := Nil;
         next := Nil;
         list := elem
    | Elem (_, prev1, next1), (Elem (_, prev2, _) as head) ->
         prev1 := Nil;
         next1 := head;
         prev2 := elem;
         list := elem
    | Nil, _ ->
         raise (Invalid_argument "insert")

let remove list elem =
   match elem with
      Elem (_, prev, next) ->
         (match !prev with
             Elem (_, _, prev_next) -> prev_next := !next
           | Nil -> list := !next);
         (match !next with
             Elem (_, next_prev, _) -> next_prev := !prev
           | Nil -> ())
    | Nil ->
         raise (Invalid_argument "remove")

let rec iter f list =
   match !list with
      Nil ->
         ()
    | Elem (x, _, next) ->
         f x;
         iter f next

let rec remove_nth l i =
   match l with
      x :: l when i = 0 -> x, l
    | x :: l -> let w, l = remove_nth l (i - 1) in w, x:: l
    | [] -> raise (Invalid_argument "remove_nth")

let rec test list elems =
   eprintf "[";
   iter (fun i -> eprintf "%d;" i) list;
   eprintf "]@.";

   (* Remove an element *)
   let len = List.length elems in
   let i = Random.int len in
   let elem, elems = remove_nth elems i in
   eprintf "Removing %d@." i;
   remove list elem;

   (* Add an element *)
   let i = Random.int 10 in
   let elem = create_elem i in
   let elems = elem :: elems in
      insert list elem;
      test list elems

let main () =
   let elems = List.map create_elem [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
   let list = create () in
      List.iter (insert list) elems;
      test list elems

let () = main ()

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
