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

class persistent_ref_value filename =
object (self)
    (* persistent_value *)
    val mutable x : int list =
       let fin = open_in_bin filename in
       let x = input_value fin in
       close_in fin;
       x
    method get = x
    method set y = x <- y; self#save
    method private save =
       let fout = open_out_bin filename in
       output_value fout x;
       close_out fout

    (* ref_value *)
    val mutable ref_count = 1
    method add_ref = ref_count <- ref_count + 1
    method rm_ref =
       ref_count <- ref_count - 1;
       if ref_count = 0 then Sys.remove filename
end

class persistent_value filename =
object (self)
    val mutable x : int list =
       let fin = open_in_bin filename in
       let x = input_value fin in
       close_in fin;
       x
    method get = x
    method set y = x <- y; self#save
    method private save =
       let fout = open_out_bin filename in
       output_value fout x;
       close_out fout
    method private delete = Sys.remove filename
end

class virtual ref_value =
object (self)
    val mutable ref_count = 1
    method add_ref = ref_count <- ref_count + 1
    method rm_ref =
       ref_count <- ref_count - 1;
       if ref_count = 0 then self#delete
    method private virtual delete : unit
end

class persistent_ref_value2 filename =
object
   inherit persistent_value filename
   inherit ref_value
end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
