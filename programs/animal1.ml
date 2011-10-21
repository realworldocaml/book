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
class animal species =
object
   method sleep = Printf.printf "A %s falls asleep\n" species
end;;

class pet ~species ~owner ~name =
object
   inherit animal species
   method owner : string = owner
   method name  : string = name
end;;

class pet_dog ~owner ~name =
object
   inherit pet ~species:"dog" ~owner ~name
   method make_sound = Printf.printf "%s barks!\n" name
end;;

class pet_cat ~owner ~name =
object
   inherit pet ~species:"cat" ~owner ~name
   method make_sound = Printf.printf "%s meows.\n" name
end;;

let clifford = new pet_dog ~name:"Clifford" ~owner:"Emily";;
clifford#make_sound;;

module Foo1 = struct
class pet ~species ~owner ~name =
object
   inherit animal species
   method owner : string = owner
   method name : string = name
   method sleep = Printf.printf "%s falls asleep.\n" name
end;;

class pet_dog ~owner ~name =
object (self : 'self)
   inherit pet ~species:"Dog" ~owner ~name as super
   method make_sound = Printf.printf "%s barks!\n" name
   method prepare_to_sleep =
      Printf.printf "%s turns three times.\n" name
   method sleep =
      self#prepare_to_sleep;
      super#sleep
end;;

let clifford = new pet_dog ~owner:"Emily" ~name:"Clifford";;
clifford#sleep
end

class type farm_animal =
object
   inherit animal
   method owner : string
end;;

class type vocal_animal =
object
   inherit animal
   method make_sound : unit
end;;

class cat ~owner ~name =
object
   inherit pet ~species:"cat" ~owner ~name
   method make_sound = Printf.printf "%s meows.\n" name
end;;

let fluffie = (clifford :> cat);;
fluffie#make_sound;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
