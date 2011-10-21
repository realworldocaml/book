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
class pet ~species ~owner ~name =
object
   method boo = species ^ owner ^ name
end;;

module type CatSig =
sig
   type cert

   class cat : owner:string -> name:string ->
   object
      inherit pet
      method cert : cert
      method make_sound : unit
   end
end

module Cat : CatSig =
struct
   type cert = unit

   class cat ~owner ~name =
   object
       inherit pet ~species:"cat" ~owner ~name
       method cert = ()
       method make_sound = Printf.printf "%s meows.\n" name
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
