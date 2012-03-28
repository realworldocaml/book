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
open Printf

let is_white = function
   ' ' | '\t' | '\n' -> true
 | _ -> false

let words_of_string s =
   let len = String.length s in
   let rec skip_white words i =
      if i = len then
         words
      else if is_white s.[i] then
         skip_white words (i + 1)
      else
         collect_word words i (i + 1)
   and collect_word words left right =
      if right = len then
         String.sub s left (right - left) :: words
      else if is_white s.[right] then
         let word = String.sub s left (right - left) in
            skip_white (word :: words) (right + 1)
      else
         collect_word words left (right + 1)
   in
      List.rev (skip_white [] 0)

let words = words_of_string "\tHello world   goodbye   ";;

List.iter (printf "|%s|\n") words;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
