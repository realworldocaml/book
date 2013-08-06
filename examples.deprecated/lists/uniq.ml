(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2013 Jason Hickey
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

let rec uniq = function
  | [] -> []
  | [_] as l -> l
  | i1 :: ((i2 :: _) as t) ->
    if i1 = i2 then
      uniq t
    else
      i1 :: uniq t;;

uniq [1; 3; 3; 3; 2];;

let rec uniq = function
  | ([] | [_]) as l -> l
  | i1 :: ((i2 :: _) as t) ->
    if i1 = i2 then
      uniq t
    else
      i1 :: uniq t;;

let rec inefficient_uniq l =
  match l with
  | ([] | [_]) -> l
  | i1 :: i2 :: t ->
    if i1 = i2 then
      inefficient_uniq (i2 :: t)
    else
      i1 :: inefficient_uniq (i2 :: t);;

uniq [1; 3; 3; 3; 2];;

(*
let rec broken_uniq = function
  | ([] | [_]) as l -> l
  | i1 :: ((i2 :: _) as t) when i1 = i2 ->
    broken_uniq t
  | i1 :: ((i2 :: _) as t) when i1 <> i2 ->
    i1 :: broken_uniq t;;
*)

let rec subtle_uniq = function
  | ([] | [_]) as l -> l
  | i1 :: ((i2 :: _) as t) when i1 = i2 -> subtle_uniq t
  | i1 :: t -> i1 :: subtle_uniq t;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
