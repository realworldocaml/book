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

class type env =
object ('self)
   method add : string -> int -> 'self
   method find : string -> int
end

class type exp =
object
   method eval : 'a. (#env as 'a) -> int
end

class int_exp i =
object (_ : #exp as 'self)
   method eval env = i
end

class binary_exp op (e1 : #exp) (e2 : #exp) =
object
   method eval : 'a. (#env as 'a) -> int =
      (fun env -> op (e1#eval env) (e2#eval env))
end

class add_exp e1 e2 = binary_exp (+) e1 e2
class sub_exp e1 e2 = binary_exp (-) e1 e2

class var_exp v =
object
   method eval : 'a. (#env as 'a) -> int = (fun env -> env#find v)
end

class let_exp v (e1 : #exp) (e2 : #exp) =
object
   method eval : 'a. (#env as 'a) -> int =
      (fun env -> e2#eval (env#add v (e1#eval env)))
end

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
