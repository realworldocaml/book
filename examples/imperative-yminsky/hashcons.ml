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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)

open Hashmap

module Exp : sig
  type t = private
   | Num of int
   | Var of string
   | Plus of t * t
   | Times of t * t

  val num : int -> t
  val var : string -> t
  val plus : t -> t -> t
  val times : t -> t -> t
end = struct
  type t =
   | Num of int
   | Var of string
   | Plus of t * t
   | Times of t * t

  let table = HashMap.create ()
  let merge exp =
     try HashMap.find table ~key:exp with
        Not_found ->
           HashMap.add table ~key:exp ~data:exp;
           exp

  let num i = merge (Num i)
  let var s = merge (Var s)
  let plus e1 e2 = merge (Plus (e1, e2))
  let times e1 e2 = merge (Times (e1, e2))
end;;

let rec eval env = function
 | Exp.Num i -> i
 | Exp.Var v -> HashMap.find env ~key:v
 | Exp.Plus (e1, e2) -> eval env e1 + eval env e2
 | Exp.Times (e1, e2) -> eval env e1 * eval env e2

module WExp : sig
  type t = private
   | Num of int
   | Var of string
   | Plus of int * t * t
   | Times of int * t * t

  val num : int -> t
  val var : string -> t
  val plus : t -> t -> t
  val times : t -> t -> t
end = struct
  type t =
   | Num of int
   | Var of string
   | Plus of int * t * t
   | Times of int * t * t

  module HashExp = struct
    type exp = t
    type t = exp
    let equal e1 e2 =
      match e1, e2 with
       | Num i1, Num i2 -> i1 = i2
       | Var v1, Var v2 -> v1 = v2
       | Plus (_, a1, a2), Plus (_, b1, b2)
       | Times (_, a1, a2), Times (_, b1, b2) ->
            a1 == b1 && a2 == b2
       | _ -> false
    let hash = function
     | Num i -> i lxor 0xabababab
     | Var v -> (Hashtbl.hash v) lxor 0xcdcdcdcdc
     | Plus (hash, _, _)
     | Times (hash, _, _) -> hash
  end

  module WeakHash = Weak.Make (HashExp);;

  let table = WeakHash.create 17
  let merge e = WeakHash.merge table e

  let num i = merge (Num i)
  let var s = merge (Var s)
  let plus e1 e2 =
     let hash = (HashExp.hash e1) lxor (HashExp.hash e2) lxor 0x12345678 in
     merge (Plus (hash, e1, e2))

  let times e1 e2 =
     let hash = (HashExp.hash e1) lxor (HashExp.hash e2) lxor 0xdeadbeef in
     merge (Times (hash, e1, e2))
end;;



(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
