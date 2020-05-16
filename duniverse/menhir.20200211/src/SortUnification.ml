(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* This module implements sort inference. *)

(* -------------------------------------------------------------------------- *)

(* The syntax of sorts is:

     sort ::= (sort, ..., sort) -> *

   where the arity (the number of sorts on the left-hand side of the arrow)
   can be zero. *)

module S = struct

  type 'a structure =
    | Arrow of 'a list

  let map f (Arrow xs) =
    Arrow (List.map f xs)

  let iter f (Arrow xs) =
    List.iter f xs

  exception Iter2

  let iter2 f (Arrow xs1) (Arrow xs2) =
    let n1 = List.length xs1
    and n2 = List.length xs2 in
    if n1 = n2 then
      List.iter2 f xs1 xs2
    else
      raise Iter2

end

include S

(* -------------------------------------------------------------------------- *)

(* Instantiate the unification algorithm with the above signature. *)

include Unifier.Make(S)

type sort = term =
  | TVar of int
  | TNode of sort structure

(* -------------------------------------------------------------------------- *)

(* Sort constructors. *)

let arrow (args : variable list) : variable =
  fresh (Some (Arrow args))

let star : variable =
  arrow []

let fresh () =
  fresh None

(* Sort accessors. *)

let domain (x : variable) : variable list option =
  match structure x with
  | Some (Arrow xs) ->
      Some xs
  | None ->
      None

(* -------------------------------------------------------------------------- *)

(* Converting between sorts and ground sorts. *)

let rec ground s =
  match s with
  | TVar _ ->
      (* All variables are replaced with [*]. *)
      GroundSort.GArrow []
  | TNode (Arrow ss) ->
      GroundSort.GArrow (List.map ground ss)

let rec unground (GroundSort.GArrow ss) =
  TNode (Arrow (List.map unground ss))

(* -------------------------------------------------------------------------- *)

(* A name generator for unification variables. *)

let make_gensym () : unit -> string =
  let c = ref 0 in
  let gensym () =
    let n = Misc.postincrement c in
    Printf.sprintf "%c%s"
      (char_of_int (Char.code 'a' + n mod 26))
      (let d = n / 26 in if d = 0 then "" else string_of_int d)
  in
  gensym

(* A memoized name generator. *)

let make_name () : int -> string =
  let gensym = make_gensym() in
  Fix.Memoize.Int.memoize (fun _x -> gensym())

(* -------------------------------------------------------------------------- *)

(* A printer. *)

let rec print name (b : Buffer.t) (sort : sort) =
  match sort with
  | TVar x ->
      Printf.bprintf b "%s" (name x)
  | TNode (S.Arrow []) ->
      Printf.bprintf b "*"
  | TNode (S.Arrow (sort :: sorts)) ->
      (* Always parenthesize the domain, so there is no ambiguity. *)
      Printf.bprintf b "(%a%a) -> *"
        (print name) sort
        (print_comma_sorts name) sorts

and print_comma_sorts name b sorts =
  List.iter (print_comma_sort name b) sorts

and print_comma_sort name b sort =
  Printf.bprintf b ", %a" (print name) sort

let print sort : string =
  let b = Buffer.create 32 in
  print (make_name()) b sort;
  Buffer.contents b
