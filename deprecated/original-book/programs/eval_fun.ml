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

module type EnvSig = sig
   type 'a t
   val empty : 'a t
   val add : 'a t -> string -> 'a -> 'a t
   val find : 'a t -> string -> 'a
end;;

module Env : EnvSig = struct
   type 'a t = (string * 'a) list
   let empty = []
   let add env v x = (v, x) :: env
   let find env v = List.assoc v env
end;;

type exp =
   Int of int
 | Var of string
 | Add of exp * exp
 | If  of exp * exp * exp
 | Let of string * exp * exp

let rec eval env = function
   Int i -> i
 | Var v -> Env.find env v
 | Add (e1, e2) ->
    eval env e1 + eval env e2
 | If (e1, e2, e3) ->
    if eval env e1 <> 0
    then eval env e2
    else eval env e3
 | Let (v, e1, e2) ->
    let i = eval env e1 in
    let env' = Env.add env v i in
       eval env' e2;;

let e = Let ("x", Int 3, Add (Var "x", Int 4));;
let i = eval Env.empty e;;
printf "%d\n" i;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
