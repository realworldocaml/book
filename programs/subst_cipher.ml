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

let length = 4

let letters1 =
   let rec collect l i =
      if i = length then
         List.rev l
      else
         collect ((Char.chr (Char.code 'A' + i), Random.bits ()) :: l) (i + 1)
   in
      collect [] 0

let letters2 =
   List.sort (fun (c1, i1) (c2, i2) ->
         if i1 < i2 then
            -1
         else if i1 > i2 then
            1
         else if c1 < c2 then
            -1
         else if c1 > c2 then
            1
         else
            0) letters1

let alpha1 = Array.of_list (List.map fst letters1)
let alpha2 = Array.of_list (List.map fst letters2)

let () =
   for i = 0 to pred length do
      printf " & %c" alpha1.(i)
   done;
   printf "\n";
   for i = 0 to pred length do
      printf " & %c" alpha2.(i)
   done;
   printf "\n"

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
