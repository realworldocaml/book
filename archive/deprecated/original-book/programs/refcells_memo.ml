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

type ('a, 'b) memo =
   ('a * 'b) list ref

let create () =
   ref []

let add memo x y =
   memo := (x, y) :: !memo

let find memo x =
   let rec search results =
      match results with
         (x', y) :: results' ->
            if x' = x then
               Some y
            else
               search results'
       | [] ->
            None
   in
      search !memo

let rec fib memo i =
   match find memo i with
      Some j ->
         j
    | None ->
         let j =
            if i = 0 || i = 1 then
               i
            else
               fib memo (i - 1) + fib memo (i - 2)
         in
            add memo i j;
            j

let fib_memo = create ();;

(*
# let fib_memo = create ();;
val fib_memo : '_a list ref = {contents = []}
# Sys.time ();;
- : float = 0.048991
# fib fib_memo 40;;
- : int = 102334155
# Sys.time ();;
- : float = 0.049991
# memo;;
Unbound value memo
# fib_memo;;
- : (int * int) list ref =
{contents =
  [(40, 102334155); (39, 63245986); (38, 39088169); (37, 24157817);
   (36, 14930352); (35, 9227465); (34, 5702887); (33, 3524578);
   (32, 2178309); (31, 1346269); (30, 832040); (29, 514229); (28, 317811);
   (27, 196418); (26, 121393); (25, 75025); (24, 46368); (23, 28657);
   (22, 17711); (21, 10946); (20, 6765); (19, 4181); (18, 2584); (17, 1597);
   (16, 987); (15, 610); (14, 377); (13, 233); (12, 144); (11, 89); (10, 55);
   (9, 34); (8, 21); (7, 13); (6, 8); (5, 5); (4, 3); (3, 2); (2, 1);
   (1, 1); (0, 0)]}
*)

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
