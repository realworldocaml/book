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
type actual = exn list -> exn
type animal = < actual : actual; eat : unit >
type dog = < actual : actual; eat : unit; bark : unit >
type cat = < actual : actual; eat : unit; meow : unit >
type hound = < actual : actual; eat : unit; bark : unit; howl : unit >

exception DogTag
exception CatTag
exception HoundTag

exception Dog of dog
exception Cat of cat
exception Hound of hound

let fido : hound =
object (self)
   method actual tags =
      match tags with
         HoundTag :: _ -> Hound self
       | DogTag :: _ -> Dog (self :> dog)
       | _ :: tags -> self#actual tags
       | [] -> Not_found
   method eat = ()
   method bark = ()
   method howl = ()
end;;

let chorus (animals : animal list) =
   List.iter (fun animal ->
      match animal#actual [DogTag; CatTag] with
         Dog dog -> dog#bark
       | Cat cat -> cat#meow
       | _ -> ()) animals

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
