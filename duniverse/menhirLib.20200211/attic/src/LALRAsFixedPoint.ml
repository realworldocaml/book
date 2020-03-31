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

(* This module formulates the construction of the LALR automaton as a least
   fixed point computation. *)

(* It is currently *not* used; the code in [LALR] is used instead when
   [--lalr] is passed on the command line, as it is about 3x faster. *)

type lr0state =
  Lr0.node

type lr1state =
  Lr0.lr1state

open Grammar

(* -------------------------------------------------------------------------- *)

(* A property is just an LR(1) state. In fact, for each LR(0) core [c], we
   have a different space of properties, namely the set of LR(1) states whose
   core is [c]. *)

(* In order to satisfy the signature expected by [Fix], we add an artificial
   [bottom] element. *)

module P = struct

  type property =
    lr1state option

  let bottom =
    None

  let equal p1 p2 =
    match p1, p2 with
    | None, None ->
        true
    | Some s1, Some s2 ->
        Lr0.equal s1 s2
    | None, Some _
    | Some _, None ->
        false

  let union p1 p2 =
    match p1, p2 with
    | None, p
    | p, None ->
        p
    | Some s1, Some s2 ->
        Some (Lr0.union s1 s2)

  let is_maximal _p =
    false

  let big_union (xs : 'a list) (f : 'a -> property) : property =
    List.fold_left (fun accu x ->
      union accu (f x)
    ) bottom xs

end

(* -------------------------------------------------------------------------- *)

(* Instantiate [Fix] to compute a mapping of LR(0) states to properties. *)

module F =
  Fix.Make
    (Maps.ArrayAsImperativeMaps(Lr0))
    (P)

(* -------------------------------------------------------------------------- *)

module Run () = struct

(* -------------------------------------------------------------------------- *)

(* Define the desired least fixed point. *)

let mu : lr0state -> P.property =

  F.lfp (fun (c : lr0state) get ->

    (* Test whether [c] is a start state of the LR(0) automaton. *)
    match Lr0.incoming_symbol c with
    | None ->
        (* [c] is a start state. We need the corresponding LR(1) start state,
           and nothing else. *)
        Some (Lr0.start c)
    | Some symbol ->
        (* [c] is not a start state. For every edge of [b] to [c] in the LR(0)
           automaton, we must find out which LR(1) states whose core is [b]
           currently exist, and compute their successors along [symbol]. This
           yields a set of LR(1) states, all of which we merge together. *)
        P.big_union (Lr0.incoming_edges c) (fun b ->
          Option.map (Lr0.transition symbol) (get b)
        )
  )

(* -------------------------------------------------------------------------- *)

(* We now force the least fixed computation to take place. (This is implicit;
   it is done just by applying the function [mu] to every LR(0) state). *)

(* Since the LALR automaton has exactly the same states as the LR(0) automaton,
   up to lookahead information, we can use the same state numbers. *)

type node =
  int

let n =
  Lr0.n

let states : lr1state array =
  Array.init n (fun c ->
    Option.force (mu c)
  )

(* -------------------------------------------------------------------------- *)

(* Expose the mapping of nodes to LR(1) states. *)

let state : node -> lr1state =
  Array.get states

(* -------------------------------------------------------------------------- *)

(* Expose the entry nodes and transitions of the LALR automaton. *)

(* Because we re-use LR(0) node numbers, these are exactly the same as those
   of the LR(0) automaton! *)

let entry : node ProductionMap.t =
  Lr0.entry

let transitions : node -> node SymbolMap.t =
  Lr0.outgoing_edges

(* -------------------------------------------------------------------------- *)

(* Expose the bijection between nodes and numbers. *)

let number (i : node) : int =
  i

let node (i : int) : node =
  i

(* -------------------------------------------------------------------------- *)

end (* Run *)
