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

class type exp =
  object ('self)
    method eval   : int env -> int
    method accept : visitor -> unit
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

and visitor =
  object ('self)
    method visit_int : int_exp_type -> unit
    method visit_var : var_exp_type -> unit
    method visit_add : add_exp_type -> unit
    method visit_if  : if_exp_type  -> unit
    method visit_let : let_exp_type -> unit
  end;;

class int_exp (i : int) =
  object (self : 'self)
    method eval (_ : int env) = i
    method accept (visitor : visitor) = visitor#visit_int (self :> int_exp)
    method explode = i
  end

class var_exp v =
  object (self : 'self)
    method eval (env : int env) =
      env#find v
    method accept (visitor : visitor) = visitor#visit_var (self :> var_exp)
    method explode = v
  end

class add_exp (e1 : #exp) (e2 : #exp) =
  object (self : 'self)
    method eval env =
      e1#eval env + e2#eval env
    method accept (visitor : visitor) =
      visitor#visit_add (self :> add_exp)
    method explode = e1, e2
  end

class if_exp
  (e1 : #exp) (e2 : #exp) (e3 : #exp) =
  object (self : 'self)
    method eval env =
      if e1#eval env <> 0
      then e2#eval env
      else e3#eval env
    method accept (visitor : visitor) =
      visitor#visit_if (self :> if_exp)
    method explode = e1, e2, e3
  end

class let_exp
  (v : string) (e1 : #exp) (e2 : #exp) =
  object (self : 'self)
    method eval env =
      let i = e1#eval env in
      let env' = env#add v i in
        e2#eval env'
    method accept (visitor : visitor) =
      visitor#visit_let (self :> let_exp)
    method explode = v, e1, e2
  end;;

class print_visitor : visitor =
  object (self : 'self)
    method visit_int (e : int_exp_type) =
       print_int e#explode
    method visit_var (e : var_exp_type) =
       print_string e#explode
    method visit_add (e : add_exp_type) =
       let e1, e2 = e#explode in
       print_string "(";
       e1#accept (self :> visitor);
       print_string " + ";
       e2#accept (self :> visitor);
       print_string ")"
    method visit_if (e : if_exp_type) =
       let e1, e2, e3 = e#explode in
       print_string "(if ";
       e1#accept (self :> visitor);
       print_string " then ";
       e2#accept (self :> visitor);
       print_string " else ";
       e3#accept (self :> visitor);
       print_string ")"
    method visit_let (e : let_exp_type) =
       let v, e1, e2 = e#explode in
       printf "(let %s = " v;
       e1#accept (self :> visitor);
       print_string " in ";
       e2#accept (self :> visitor);
       print_string ")"
  end;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
