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
type 'a animal = < actual : 'a; eat : unit >
type 'a dog = < actual : 'a; eat : unit; bark : unit >
type 'a cat = < actual : 'a; eat : unit; meow : unit >
type 'a lizard = < actual : 'a; eat : unit >

type 'a tag = [> `Dog of 'a tag dog | `Cat of 'a tag cat ] as 'a

let fido : 'a tag dog =
object (self)
   method actual = `Dog self
   method eat = ()
   method bark = ()
end;;

let daphne : 'a tag cat =
object (self)
   method actual = `Cat self
   method eat = ()
   method meow = ()
end;;

let fred : 'a tag lizard =
object (self)
   method actual = `Lizard self
   method eat = ()
end;;

let animals = [(fido :> 'a tag animal); (daphne :> 'a tag animal); (fred :> 'a tag animal)]

let chorus (animals : 'a tag animal list) =
   List.iter (fun animal ->
      match animal#actual with
         `Dog dog -> dog#bark
       | `Cat cat -> cat#meow
       | _ -> ()) animals

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
