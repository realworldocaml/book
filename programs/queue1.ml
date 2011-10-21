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
type 'a deferred_value =
   Deferred of (unit -> 'a)
 | Forced of 'a

type 'a deferred = 'a deferred_value ref

let defer f = ref (Deferred f)

let force cell =
   match !cell with
      Deferred f ->
         let result = f () in
         cell := Forced result;
         result
    | Forced result ->
         result

type 'a lazy_list =
   Nil
 | Cons of 'a * 'a lazy_list
 | LazyCons of 'a * 'a lazy_list deferred

let nil = Nil
let cons h t = Cons (h, t)
let lazy_cons h f = LazyCons (h, defer f)

let is_nil = function
   Nil -> true
 | Cons _ | LazyCons _ -> false

let head = function
   Nil -> raise (Invalid_argument "head")
 | Cons (h, _)
 | LazyCons (h, _) -> h

let tail = function
   Nil -> raise (Invalid_argument "tail")
 | Cons (_, t) -> t
 | LazyCons (_, f) -> force f

let rec (@@) l1 l2 =
   match l1, l2 with
      Nil, l | l, Nil -> l
    | Cons (h, l1), l2 -> lazy_cons h (fun () -> l1 @@ l2)
    | LazyCons (h, l1), l2 -> lazy_cons h (fun () -> force l1 @@ l2)

type 'a queue = 'a lazy_list

let empty = nil

let add queue x = queue @@ (cons x nil)

let take queue = head queue, tail queue



(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
