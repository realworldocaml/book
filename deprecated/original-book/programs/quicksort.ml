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

let quicksort1 data =
    let partition low high =
       let pivot = data.(low + Random.int (high - low)) in
       let rec loop left right =
          if left = right then
             right
          else if data.(right) >= pivot then
             loop left (right - 1)
          else if data.(left) < pivot then
             loop (left + 1) right
          else
             let tmp = data.(left) in
             data.(left) <- data.(right);
             data.(right) <- tmp;
             loop left right
       in
          loop low high
    in
    let rec sort low high =
       if low < high then
          let mid = partition low high in
          sort low mid;
          sort (mid + 1) high
    in
    sort 0 (Array.length data - 1)

let quicksort2 data =
   let partition low high =
      let pivot = data.(low + Random.int (high - low)) in
      let rec loop_right left right =
         if left = right then
            right
         else if data.(right) >= pivot then
            loop_right left (right - 1)
         else
            loop_left left right
      and loop_left left right =
         if left = right then
            right
         else if data.(left) < pivot then
            loop_left (left + 1) right
         else
            let tmp = data.(left) in
               data.(left) <- data.(right);
               data.(right) <- tmp;
               loop_right left right
      in
         loop_right low high
    in
    let rec sort low high =
       if low < high then
          let mid = partition low high in
          sort low mid;
          sort (mid + 1) high
    in
       sort 0 (Array.length data - 1)

let quicksort = quicksort2

let data = Array.init 100 (fun _ -> Random.int 1000);;
let () = quicksort data;;

for i = 0 to Array.length data - 1 do
   printf "%d\n" data.(i)
done

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
