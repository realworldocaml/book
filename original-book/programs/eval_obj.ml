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
open Printf

class type ['a] env_sig =
  object ('self)
    method add : string -> 'a -> 'self
    method find : string -> 'a
  end;;

class ['a] env : ['a] env_sig =
  object (self : 'self)
    val env : (string * 'a) list = []
    method add v x =
      {< env = (v, x) :: env >}
    method find v = List.assoc v env
  end;;

class type exp =
  object ('self)
    method eval : int env -> int
  end

class int_exp (i : int) =
  object (self : 'self)
    method eval (_ : int env) = i
  end

class var_exp v =
  object (self : 'self)
    method eval (env : int env) =
      env#find v
  end

class add_exp (e1 : #exp) (e2 : #exp) =
  object (self : 'self)
    method eval env =
      e1#eval env + e2#eval env
  end

class if_exp
  (e1 : #exp) (e2 : #exp) (e3 : #exp) =
  object (self : 'self)
    method eval env =
      if e1#eval env <> 0
      then e2#eval env
      else e3#eval env
  end

class let_exp
  (v : string) (e1 : #exp) (e2 : #exp) =
  object (self : 'self)
    method eval env =
      let i = e1#eval env in
      let env' = env#add v i in
        e2#eval env'
  end;;

let e =
   new let_exp "x" (new int_exp 3)
     (new add_exp (new var_exp "x")
        (new int_exp 4));;
let i = e#eval (new env);;
printf "%d\n" i;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
