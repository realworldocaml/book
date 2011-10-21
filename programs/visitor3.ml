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
    method add v x = {< env = (v, x) :: env >}
    method find v = List.assoc v env
end;;

class type visitor =
  object ('self)
    method visit_int : int -> 'self
    method visit_var : string -> 'self
    method visit_add : exp -> exp -> 'self
    method visit_if  : exp -> exp -> exp -> 'self
    method visit_let : string -> exp -> exp -> 'self
  end

and exp =
  object ('self)
    method eval   : int env -> int
    method accept : visitor -> visitor
  end

and int_exp_type =
  object ('self)
    inherit exp
    method explode : int
  end

and var_exp_type =
  object ('self)
    inherit exp
    method explode : string
  end

and add_exp_type =
  object ('self)
    inherit exp
    method explode : exp * exp
  end

and if_exp_type =
  object ('self)
    inherit exp
    method explode : exp * exp * exp
  end

and let_exp_type =
  object ('self)
    inherit exp
    method explode : string * exp * exp
  end

class int_exp (i : int) =
  object (self : 'self)
    method eval (_ : int env) = i
    method accept : 'a. (#visitor as 'a) -> 'a =
      fun visitor -> visitor#visit_int i
    method explode = i
  end

class var_exp v =
  object (self : 'self)
    method eval (env : int env) =
      env#find v
    method accept : 'a. (#visitor as 'a) -> 'a =
      fun visitor -> visitor#visit_var v
    method explode = v
  end

class add_exp (e1 : #exp) (e2 : #exp) =
  object (self : 'self)
    method eval env =
      e1#eval env + e2#eval env
    method accept : 'a. (#visitor as 'a) -> 'a =
      fun visitor -> visitor#visit_add e1 e2
    method explode = e1, e2
  end

class if_exp
  (e1 : #exp) (e2 : #exp) (e3 : #exp) =
  object (self : 'self)
    method eval env =
      if e1#eval env <> 0
      then e2#eval env
      else e3#eval env
    method accept : 'a. (#visitor as 'a) -> 'a =
      fun visitor -> visitor#visit_if e1 e2 e3
    method explode = e1, e2, e3
  end

class let_exp
  (v : string) (e1 : #exp) (e2 : #exp) =
  object (self : 'self)
    method eval env =
      let i = e1#eval env in
      let env' = env#add v i in
        e2#eval env'
    method accept : 'a. (#visitor as 'a) -> 'a =
      fun visitor -> visitor#visit_let v e1 e2
    method explode = v, e1, e2
  end;;

class eval_visitor =
  object (self : 'self)
    val result = 0
    val env = new env
    method value = result

    method visit_int (i : int) =
       {< result = i >}
    method visit_var v =
       {< result = env#find v >}
    method visit_add (e1 : exp) (e2 : exp) =
       {< result = (e1#accept self)#value + (e2#accept self)#value >}
    method visit_if (e1 : exp) (e2 : exp) (e3 : exp) =
       if (e1#accept self)#value <> 0
       then e2#accept self
       else e3#accept self
    method visit_let v (e1 : exp) (e2 : exp) =
       let i = (e1#accept self)#value in
       let visitor = {< env = env#add v i >} in
       e2#accept visitor
  end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
