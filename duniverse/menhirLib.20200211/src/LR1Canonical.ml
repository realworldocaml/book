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

(* This module formulates the construction of the canonical LR(1) automaton as
   a forward graph traversal. *)

type lr0state =
  Lr0.node

type lr1state =
  Lr0.lr1state

open Grammar

module Run () = struct

(* -------------------------------------------------------------------------- *)

(* Give an implicit definition of the graph that we wish to traverse. *)

module G = struct

  type t = lr1state

  let foreach_root f =
    ProductionMap.iter (fun _prod (c : lr0state) ->
      f (Lr0.start c)
    ) Lr0.entry

  let foreach_successor (state : lr1state) f =
    let symbols = Lr0.outgoing_symbols (Lr0.core state) in
    List.iter (fun symbol ->
      let successor = Lr0.transition symbol state in
      f successor
    ) symbols

end

(* -------------------------------------------------------------------------- *)

(* Traversing this graph yields a numbering of the LR(1) states in the
   canonical automaton. *)

type node =
  int

include Fix.GraphNumbering.ForOrderedType(Lr0.Lr1StateAsOrderedType)(G)
  (* This defines [n : int],
                  [encode : lr1state -> node],
                  [decode : node -> lr1state]. *)

(* -------------------------------------------------------------------------- *)

(* Expose the mapping of nodes to LR(1) states. *)

let state : node -> lr1state =
  decode

(* -------------------------------------------------------------------------- *)

(* Expose the entry nodes of the LR(1) automaton. *)

let entry : node ProductionMap.t =
  ProductionMap.map (fun (c : lr0state) ->
    encode (Lr0.start c)
  ) Lr0.entry

(* -------------------------------------------------------------------------- *)

(* Expose the transitions of the LR(1) automaton. *)

let transition symbol (i : node) : node =
  encode (Lr0.transition symbol (state i))

let outgoing_symbols (i : node) =
  Lr0.outgoing_symbols (Lr0.core (state i))

let transitions (i : node) : node SymbolMap.t =
  SymbolMap.init (fun symbol ->
    transition symbol i
  ) (outgoing_symbols i)

(* -------------------------------------------------------------------------- *)

(* Expose the bijection between nodes and numbers. *)

let number (i : node) : int =
  i

let node (i : int) : node =
  i

end
