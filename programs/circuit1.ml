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

type terminal = < set : bool -> unit >

class wire =
object
   val mutable terminals : terminal list = []
   val mutable value = false
   method add_terminal t = terminals <- t :: terminals
   method set x =
      if x <> value then begin
          value <- x;
          List.iter (fun t -> t#set x) terminals
      end
end

let dummy_wire = new wire

class virtual gate =
object (self : 'self)
   val mutable output_wire = dummy_wire
   method connect_output wire = output_wire <- wire
   method private set_output = output_wire#set self#compute_value
   method private virtual compute_value : bool
end

class virtual two_input_gate =
object (self : 'self)
   inherit gate
   val mutable a = false
   val mutable b = false
   method private set_input_a x = a <- x; self#set_output
   method private set_input_b x = b <- x; self#set_output
   method connect_input_a (wire : wire) =
       wire#add_terminal (object method set x = self#set_input_a x end)
   method connect_input_b (wire : wire) =
       wire#add_terminal (object method set x = self#set_input_b x end)
end

class nand2 =
object
   inherit two_input_gate
   method compute_value = not (a && b)
end

class nor2 =
object
   inherit two_input_gate
   method compute_value = not (a || b)
end

let gate1 = new nand2;;
let gate2 = new nand2;;
let wire1 = new wire;;
let wire2 = new wire;;
gate1#connect_output wire1;;
gate2#connect_output wire2;;
gate1#connect_input_b wire2;;
gate2#connect_input_a wire1;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
