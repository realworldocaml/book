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
 * @email{jasonh@gmail.com}
 * @end[license]
 *)

class m1 =
object (self : 'self)
   method get = 1
   method f = self#get
   method get = 2
end;;

class m2 =
object (self : 'self)
   val x = 1
   method f = x
   val x = 2
end;;

class refcell i =
object (self : 'self)
   val mutable x : int = i
   method set x' = x <- x'
   method get = x
end;;

class m3 =
object (self : 'self)
   inherit refcell 5 as super

   val mutable x : int = 7
   method set x' = x <- x'
   method get = x

   method super_set x' = super#set x'
   method super_get = super#get
end;;

class m4 = object method get = 2 end;;
class m5 =
object
  val mutable x = 1
  method get = x
  method set x' = x <- x'
  inherit m4
end;;

class m6 =
object (self : 'self)
   method f1 = self#g
   method private g = 1
end;;

class m7 =
object (self : 'self)
   method f2 = self#g
   method private g = 2
end;;

class m8 : object method f3 : int end =
object (self : 'self)
   method f3 = self#g
   method private g = 3
end;;

class m9 =
object (self : 'self)
   inherit m6
   inherit m7
   inherit m8
end;;


(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
