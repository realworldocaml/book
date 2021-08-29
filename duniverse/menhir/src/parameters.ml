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

(* TEMPORARY clean up and write an .mli file *)

open Syntax
open Positions

let app p ps =
  match ps with
  | [] ->
      ParameterVar p
  | _ ->
      ParameterApp (p, ps)

let unapp = function
  | ParameterVar x ->
      (x, [])
  | ParameterApp (p, ps) ->
      (p, ps)
  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false

let unvar = function
  | ParameterVar x ->
      x
  | ParameterApp _
  | ParameterAnonymous _ ->
      assert false

let rec map f = function
  | ParameterVar x ->
      ParameterVar (f x)
  | ParameterApp (p, ps) ->
      ParameterApp (f p, List.map (map f) ps)
  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false

let rec fold f init = function
  | ParameterVar x ->
      f init x
  | ParameterApp (p, ps) ->
      f (List.fold_left (fold f) init ps) p
  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false

let identifiers m p =
  fold (fun accu x -> StringMap.add x.value x.position accu) m p

let rec occurs (x : symbol) (p : parameter) =
  match p with
  | ParameterVar y ->
      x = y.value
  | ParameterApp (y, ps) ->
      x = y.value || List.exists (occurs x) ps
  | ParameterAnonymous _ ->
      assert false

let occurs_shallow (x : symbol) (p : parameter) =
  match p with
  | ParameterVar y ->
      x = y.value
  | ParameterApp (y, _) ->
      assert (x <> y.value);
      false
  | ParameterAnonymous _ ->
      assert false

let occurs_deep (x : symbol) (p : parameter) =
  match p with
  | ParameterVar _ ->
      false
  | ParameterApp (_, ps) ->
      List.exists (occurs x) ps
  | ParameterAnonymous _ ->
      assert false

type t = parameter

let rec equal x y =
  match x, y with
    | ParameterVar x, ParameterVar y ->
        x.value = y.value
    | ParameterApp (p1, p2), ParameterApp (p1', p2') ->
        p1.value = p1'.value && List.for_all2 equal p2 p2'
    | _ ->
        (* Anonymous rules are eliminated early on. *)
        false

let hash = function
  | ParameterVar x
  | ParameterApp (x, _) ->
      Hashtbl.hash (Positions.value x)
  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false

let position = function
  | ParameterVar x
  | ParameterApp (x, _) ->
      Positions.position x
  | ParameterAnonymous bs ->
      Positions.position bs

let with_pos p =
  Positions.with_pos (position p) p

let rec print with_spaces = function
  | ParameterVar x
  | ParameterApp (x, []) ->
      x.value
  | ParameterApp (x, ps) ->
      let separator = if with_spaces then ", " else "," in
      Printf.sprintf "%s(%s)"
        x.value
        (Misc.separated_list_to_string (print with_spaces) separator ps)
  | ParameterAnonymous _ ->
      assert false
