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

module type Pipeline = sig
   type t
   val f : t -> unit
end

module type Filter = sig
   type t_in
   type t_out
   module F : functor (P : Pipeline with type t = t_out) ->
      Pipeline with type t = t_in
end

module StringOfChar
 : Filter with type t_in = string and type t_out = char =
struct
   type t_in = string
   type t_out = char
   module F (X : Pipeline with type t = char) = struct
      type t = string
      let f s = String.iter X.f s
   end
end

module type ComposeSig = functor (F1 : Filter) ->
  functor (F2 : Filter with type t_in = F1.t_out) ->
  Filter with type t_in = F1.t_in and type t_out = F2.t_out

module Compose
 (F1 : Filter)
 (F2 : Filter with type t_in = F1.t_out)
 : Filter with type t_in = F1.t_in and type t_out = F2.t_out =
struct
   type t_in = F1.t_in
   type t_out = F2.t_out
   module F (P3 : Pipeline with type t = t_out) = struct
      module Pipe = F1.F (F2.F (P3))
      type t = t_in
      let f = Pipe.f
   end
end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
