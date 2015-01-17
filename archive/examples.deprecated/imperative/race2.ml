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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
let value = ref 0
let mutex = Mutex.create ()

let loop () =
   for i = 1 to 3 do
     Mutex.lock mutex;
     let i = !value in
     Printf.printf "i = %d\n" i;
     flush stdout;
     value := i + 1;
     Mutex.unlock mutex
   done

let thread1 = Thread.create loop ();;
let thread2 = Thread.create loop ();;

Thread.join thread1;;
Thread.join thread2;;

Printf.printf "value = %d\n" !value

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
