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

type 'a lazy_value =
   Delayed of (unit -> 'a)
 | Result of 'a

type 'a lazy_cell = 'a lazy_value ref

let create f =
   ref (Delayed f)

let get cell =
   match !cell with
      Delayed f ->
         let result = f () in
             cell := Result result;
             result
    | Result result ->
         result;;

let rec fib i =
   if i = 0 || i = 1 then
      i
   else
      fib (i - 1) + fib (i - 2);;

let c40 = create (fun () -> fib 40);;

Sys.time ();;

get c40;;

Sys.time ();;

get c40;;

Sys.time ();;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
