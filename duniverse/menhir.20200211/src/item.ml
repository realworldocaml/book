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

open Grammar

(* ------------------------------------------------------------------------ *)
(* Items. *)

(* An LR(0) item encodes a pair of integers, namely the index of the
   production and the index of the bullet in the production's
   right-hand side. *)

(* Both integers are packed into a single integer, using 10 bits for
   the bullet position and the rest (21 bits on a 32-bit architecture,
   53 bits on a 64-bit architecture) for the production index. This
   means that the length of a production must be at most 1023. This
   means that the number of productions must be at most:
   - 2^21, that is about 2 million, on a 32-bit architecture;
   - 2^53, that is practically unlimited, on a 64-bit architecture. *)

(* These static limits could be adjusted if necessary. It would also be
   possible to dynamically adjust the limits depending on the grammar
   at hand. In practice, the need for this has not been felt. *)

(* WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING *)

(* The constant [low_bits], [low_limits] and the function [export] are
   duplicated in [lib/InspectionTableInterpreter.ml]. Do not modify them,
   or modify them here and there in a consistent manner. *)

type t = int

let low_bits =
  10 (* have you read the above warning? *)

let low_limit =
  1 lsl low_bits

let export t =
  (Production.i2p (t lsr low_bits), t mod low_limit)

let import (prod, pos) =
  assert (pos < low_limit);
  (Production.p2i prod) lsl low_bits + pos

let marshal (item : t) : int =
  item

(* In order to guarantee that the assertion in [import] cannot fail,
   we check up front that every production is reasonably short. *)

let () =
  Production.iter (fun index ->
    let length = Production.length index in
    if low_limit <= length then
      Error.error
        (Production.positions index)
        "The length of this production is %d, which exceeds the limit of %d."
        length (low_limit - 1)
  )

(* Comparison. *)

let equal (item1 : t) (item2: t) =
  item1 = item2

(* Position. *)

let positions (item : t) =
  let prod, _ = export item in
  Production.positions prod

(* [def item] looks up the production associated with this item in the
   grammar and returns [prod, nt, rhs, pos, length], where [prod] is
   the production's index, [nt] and [rhs] represent the production,
   [pos] is the position of the bullet in the item, and [length] is
   the length of the production's right-hand side. *)

let def t =
  let prod, pos = export t in
  let nt, rhs = Production.def prod in
  let length = Array.length rhs in
  assert ((pos >= 0) && (pos <= length));
  prod, nt, rhs, pos, length

let startnt t =
  let _, _, rhs, pos, length = def t in
  assert (pos = 0 && length = 1);
  match rhs.(0) with
  | Symbol.N nt ->
      nt
  | Symbol.T _ ->
      assert false

(* Printing. *)

let print item =
  let _, nt, rhs, pos, _ = def item in
  Printf.sprintf "%s -> %s" (Nonterminal.print false nt) (Symbol.printaod 0 pos rhs)

(* Classifying items. *)

type kind =
  | Shift of Symbol.t * t
  | Reduce of Production.index

let classify item =
  let prod, _, rhs, pos, length = def item in
  if pos = length then
    Reduce prod
  else
    Shift (rhs.(pos), import (prod, pos + 1))

(* Sets of items and maps over items. Hashing these data structures is
   specifically allowed, so balanced trees (for instance) would not be
   applicable here. *)

module Map = Patricia.Big
module Set = Map.Domain

(* This functor performs precomputation that helps efficiently compute
   the closure of an LR(0) or LR(1) state. The precomputation requires
   time linear in the size of the grammar. The nature of the lookahead
   sets remains abstract. *)

(* The precomputation consists in building the LR(0) nondeterministic
   automaton. This is a graph whose nodes are items and whose edges
   are epsilon transitions. (We do not care about shift transitions
   here.) Lookahead information can be attached to nodes and is
   propagated through the graph during closure computations. *)

module Closure (L : Lookahead.S) = struct

  type state = L.t Map.t

  type node = {

      (* Nodes are sequentially numbered so as to allow applying
         Tarjan's algorithm (below). *)

      num: int;

      (* Each node is associated with an item. *)

      item: t;

      (* All of the epsilon transitions that leave a node have the
         same behavior with respect to lookahead information. *)

      (* The lookahead set transmitted along an epsilon transition is
         either a constant, or the union of a constant and the lookahead
         set at the source node. The former case corresponds to a source
         item whose trailer is not nullable, the latter to a source item
         whose trailer is nullable. *)

      epsilon_constant: L.t;
      epsilon_transmits: bool;

      (* Each node carries pointers to its successors through
         epsilon transitions. This field is never modified
         once initialization is over. *)

      mutable epsilon_transitions: node list;

      (* The following fields are transient, that is, only used
         temporarily during graph traversals. Marks are used to
         recognize which nodes have been traversed already. Lists
         of predecessors are used to record which edges have been
         traversed. Lookahead information is attached with each
         node. *)

      mutable mark: Mark.t;
      mutable predecessors: node list;
      mutable lookahead: L.t;
    }

  (* Allocate one graph node per item and build a mapping of
     items to nodes. *)

  let count =
    ref 0

  let mapping : node array array =
    Array.make Production.n [||]

  let item2node item =
    let prod, pos = export item in
    mapping.(Production.p2i prod).(pos)

  let () =
    Production.iter (fun prod ->
      let _nt, rhs = Production.def prod in
      let length = Array.length rhs in
      mapping.(Production.p2i prod) <- Array.init (length+1) (fun pos ->

        let item = import (prod, pos) in
        let num = !count in
        count := num + 1;

        (* The lookahead set transmitted through an epsilon
           transition is the FIRST set of the remainder of
           the source item, plus, if that is nullable, the
           lookahead set of the source item. *)

        let constant, transmits =
          if pos < length then
            let nullable, first = Analysis.nullable_first_prod prod (pos + 1) in
            L.constant first, nullable
          else
            (* No epsilon transitions leave this item. *)
            L.empty, false
        in

        {
          num = num;
          item = item;
          epsilon_constant = constant;
          epsilon_transmits = transmits;
          epsilon_transitions = []; (* temporary placeholder *)
          mark = Mark.none;
          predecessors = [];
          lookahead = L.empty;
        }

      )
    )

  (* At each node, compute transitions. *)

  let () =
    Production.iter (fun prod ->
      let _nt, rhs = Production.def prod in
      let length = Array.length rhs in
      Array.iteri (fun pos node ->

        node.epsilon_transitions <-
          if pos < length then
            match rhs.(pos) with
            | Symbol.N nt ->
                Production.foldnt nt [] (fun prod nodes ->
                  (item2node (import (prod, 0))) :: nodes
                )
            | Symbol.T _ ->
                []
          else
            []

      ) mapping.(Production.p2i prod)
    )

  (* Detect and reject cycles of transitions that transmit a lookahead
     set.

     We need to ensure that there are no such cycles in order to be
     able to traverse these transitions in topological order.

     Each such cycle corresponds to a set of productions of the form
     A1 -> A2, A2 -> A3, ..., An -> A1 (modulo nullable
     trailers). Such cycles are unlikely to occur in realistic
     grammars, so our current approach is to reject the grammar if
     such a cycle exists. Actually, according to DeRemer and Pennello
     (1982), such a cycle is exactly an includes cycle, and implies
     that the grammar is not LR(k) for any k, unless A1, ..., An are
     in fact uninhabited. In other words, this is a pathological
     case. *)

  (* Yes, indeed, this is called a cycle in Aho & Ullman's book,
     and a loop in Grune & Jacobs' book. It is not difficult to
     see that (provided all symbols are inhabited) the grammar
     is infinitely ambiguous if and only if there is a loop. *)

  module P = struct

    type foo = node
    type node = foo

    let n =
      !count

    let index node =
      node.num

    let iter f =
      Array.iter (fun nodes ->
        Array.iter f nodes
      ) mapping

    let successors f node =
      if node.epsilon_transmits then
        List.iter f node.epsilon_transitions

  end

  module T = Tarjan.Run (P)

  let cycle scc =
    let items = List.map (fun node -> node.item) scc in
    let positions = List.flatten (List.map positions items) in
    let names = String.concat "\n" (List.map print items) in
    Error.error
      positions
      "the grammar is ambiguous.\n\
       The following items participate in an epsilon-cycle:\n\
       %s" names

  let () =
     P.iter (fun node ->
       let scc = T.scc node in
       match scc with
       | [] ->
           ()
       | [ node ] ->

           (* This is a strongly connected component of one node. Check
              whether it carries a self-loop. Forbidding self-loops is not
              strictly required by the code that follows, but is consistent
              with the fact that we forbid cycles of length greater than 1. *)

           P.successors (fun successor ->
             if successor.num = node.num then
               cycle scc
           ) node

       | _ ->

           (* This is a strongly connected component of at least two
              elements. *)

           cycle scc
     )

  (* Closure computation. *)

  let closure (items : state) : state =

    (* Explore the graph forwards, starting from these items. Marks
       are used to tell which nodes have been visited. Build a list of
       all visited nodes; this is in fact the list of all items in the
       closure.

       At initial nodes and when reaching a node through a transition,
       record a lookahead set.

       When we reach a node through a transition that transmits the
       lookahead set found at its source, record its source, so as to
       allow re-traversing this transition backwards (below). *)

    let this = Mark.fresh() in
    let nodes = ref [] in

    let rec visit father transmits toks node =
      if Mark.same node.mark this then begin
        (* Node has been visited already. *)
        node.lookahead <- L.union toks node.lookahead;
        if transmits then
          node.predecessors <- father :: node.predecessors
      end
      else begin
        (* Node is new. *)
        node.predecessors <- if transmits then [ father ] else [];
        node.lookahead <- toks;
        follow node
      end

    and follow node =
      node.mark <- this;
      nodes := node :: !nodes;
      List.iter
        (visit node node.epsilon_transmits node.epsilon_constant)
        node.epsilon_transitions

    in

    Map.iter (fun item toks ->
      let node = item2node item in
      visit node (* dummy! *) false toks node
    ) items;

    let nodes =
      !nodes in

    (* Explore the graph of transmitting transitions backwards. By
       hypothesis, it is acyclic, so this is a topological
       walk. Lookahead sets are inherited through transitions. *)

    let this = Mark.fresh() in

    let rec walk node =
      if not (Mark.same node.mark this) then begin
        (* Node is new. *)
        node.mark <- this;
        (* Explore all predecessors and merge their lookahead
           sets into the current node's own lookahead set. *)
        List.iter (fun predecessor ->
          walk predecessor;
          node.lookahead <- L.union predecessor.lookahead node.lookahead
        ) node.predecessors
      end
    in

    List.iter walk nodes;

    (* Done. Produce a mapping of items to lookahead sets.
       Clear all transient fields so as to reduce pressure
       on the GC -- this does not make much difference. *)

    List.fold_left (fun closure node ->
      node.predecessors <- [];
      let closure = Map.add node.item node.lookahead closure in
      node.lookahead <- L.empty;
      closure
    ) Map.empty nodes

  (* End of closure computation *)

end
