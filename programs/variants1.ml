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

type 'a exp = 'a constraint 'a =
 [> `Int of int
  | `Var of string
  | `Add of 'a * 'a
  | `If  of 'a * 'a * 'a
  | `Let of string * 'a * 'a ]

let rec eval1 env = function
   `Int i -> i
 | `Var v -> Env.find env v
 | `Add (e1, e2) ->
      eval1 env e1 + eval1 env e2
 | `If (e1, e2, e3) ->
      if eval1 env e1 <> 0
      then eval1 env e2
      else eval1 env e3
 | `Let (v, e1, e2) ->
      let i = eval1 env e1 in
      let env' = Env.add env v i in
      eval1 env' e2
 | _ ->
      raise (Failure "eval1")

let pre_eval1 eval_subterm env = function
   `Int i -> i
 | `Var v -> Env.find env v
 | `Add (e1, e2) ->
      eval_subterm env e1 + eval_subterm env e2
 | `If (e1, e2, e3) ->
      if eval_subterm env e1 <> 0
      then eval_subterm env e2
      else eval_subterm env e3
 | `Let (v, e1, e2) ->
      let i = eval_subterm env e1 in
      let env' = Env.add env v i in
      eval_subterm env' e2
 | _ ->
      raise (Failure "eval")

type 'a exp2 = 'a
   constraint 'a = 'a exp
   constraint 'a = [> `Mul of 'a * 'a ]

type 'a evaluator = int Env.t -> 'a -> int
type 'a pre_evaluator = 'a evaluator -> 'a evaluator

let pre_eval2 eval_subterm env = function
   `Mul (e1, e2) ->
      eval_subterm env e1 * eval_subterm env e2
 | e ->
      pre_eval1 eval_subterm env e

let rec make_eval pre_eval env e =
   pre_eval (make_eval pre_eval) env e

let eval2 env e = make_eval pre_eval2 env e

let e = `Let ("x", `Int 3, `Add (`Mul (`Var "x", `Int 3), `Int 5))
let i = eval2 Env.empty e;;
printf "%d\n" i


(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
