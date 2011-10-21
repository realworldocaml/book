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

(*
class type ['a] visitor =
  object ('self)
    method visit_int : int -> 'a
    method visit_var : string -> 'a
    method visit_add : exp -> exp -> 'a
    method visit_if  : exp -> exp -> exp -> 'a
    method visit_let : string -> exp -> exp -> 'a
  end

and exp =
  object ('self)
    method eval   : int env -> int
    method accept : 'a. 'a visitor -> 'a
  end;;
 *)

class type ['a, 'exp] visitor =
  object ('self)
    method visit_int : int -> 'a
    method visit_var : string -> 'a
    method visit_add : 'exp -> 'exp -> 'a
    method visit_if  : 'exp -> 'exp -> 'exp -> 'a
    method visit_let : string -> 'exp -> 'exp -> 'a
  end;;

class type exp =
  object ('self)
    method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a
  end

class int_exp (i : int) : exp =
  object (self : 'self)
    method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a =
      fun visitor -> visitor#visit_int i
  end

class var_exp v : exp =
  object (self : 'self)
    method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a =
      fun visitor -> visitor#visit_var v
  end

class add_exp (e1 : #exp) (e2 : #exp) : exp =
  object (self : 'self)
    method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a =
      fun visitor -> visitor#visit_add e1 e2
  end

class if_exp
  (e1 : #exp) (e2 : #exp) (e3 : #exp) : exp =
  object (self : 'self)
    method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a =
      fun visitor -> visitor#visit_if e1 e2 e3
  end

class let_exp
  (v : string) (e1 : #exp) (e2 : #exp) : exp =
  object (self : 'self)
    method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a =
      fun visitor -> visitor#visit_let v e1 e2
  end;;

class eval_visitor : [int, exp] visitor =
  object (self : 'self)
    val env = new env

    method visit_int (i : int) =
       i
    method visit_var v =
       env#find v
    method visit_add (e1 : exp) (e2 : exp) =
       e1#accept self + e2#accept self
    method visit_if (e1 : exp) (e2 : exp) (e3 : exp) =
       if e1#accept self <> 0
       then e2#accept self
       else e3#accept self
    method visit_let v (e1 : exp) (e2 : exp) =
       e2#accept {< env = env#add v (e1#accept self) >}
  end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
