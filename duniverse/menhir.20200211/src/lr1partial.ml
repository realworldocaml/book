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

exception Oops

module Run (X : sig

  (* A restricted set of tokens of interest. *)

  val tokens: TerminalSet.t

  (* A state of the (merged) LR(1) automaton that we're trying to
     simulate. *)

  val goal: Lr1.node

end) = struct

  (* First, let's restrict our interest to the nodes of the merged
     LR(1) automaton that can reach the goal node. Some experiments
     show that this can involve one tenth to one half of all nodes.
     This optimization seems minor, but is easy to implement. *)

  let relevant : Lr1.node -> bool =
    let module G = struct
      include Lr1.BackwardEdges
      let foreach_root f =
        f X.goal
    end in
    let module M = DFS.MarkArray(Lr1) in
    let module D = struct
      let discover _node = ()
      let traverse _source _label _target = ()
    end in
    let module R = DFS.Run(G)(M)(D) in
    M.is_marked

  (* Second, all of the states that we shall consider are restricted
     to the set of tokens of interest. This is an important idea: by
     abstracting away some information, we make the construction much
     faster. *)

  let restrict =
    Lr0.restrict X.tokens

  (* Constructing the automaton. The automaton is represented as a
     graph. States are never merged -- this is a canonical LR(1)
     construction!

     As we go, we record the correspondence between nodes in this
     automaton and nodes in the merged LR(1) automaton. This allows
     us to tell when we have reached the desired place.

     This also allows us not to follow transitions that have already
     been eliminated, in the merged automaton, via resolution of
     shift/reduce conflicts. Whenever we follow a transition in the
     canonical LR(1) automaton, we check that the corresponding
     transition is legal in the merged LR(1) automaton.

     The automaton is explored breadth-first and shortest paths from
     every node to one of the start nodes are recorded. *)

  type node = {
      state: Lr0.lr1state;
      ancestor: (Symbol.t * node) option;
      shadow: Lr1.node;
    }

  (* A queue of pending nodes, whose successors should be explored. *)

  let queue : node Queue.t =
    Queue.create()

  (* Mapping of LR(0) state numbers to lists of nodes. *)

  let map : node list array =
    Array.make Lr0.n []

  (* Exploring a state. This creates a new node, if necessary, and
     enqueues it for further exploration. *)

  exception Goal of node * Terminal.t

  let explore ancestor shadow (state : Lr0.lr1state) : unit =

    (* Find all existing nodes that share the same LR(0) core. *)

    let k = Lr0.core state in
    assert (k < Lr0.n);
    let similar = map.(k) in

    (* Check whether one of these nodes coincides with the candidate
       new node. If so, stop. This check requires comparing not only
       the states of the partial, canonical automaton, but also their
       shadows in the full, merged automaton. This is because a single
       state of the canonical automaton may be reached along several
       different paths, leading to distinct shadows in the merged
       automaton, and we must explore all of these paths in order to
       ensure that we eventually find a goal node. *)

    if not (List.exists (fun node ->
      Lr0.equal state node.state && shadow == node.shadow
    ) similar) then begin

      (* Otherwise, create a new node. *)

      let node = {
        state = state;
        ancestor = ancestor;
        shadow = shadow;
      } in

      map.(k) <- node :: similar;
      Queue.add node queue;

      (* Check whether this is a goal node. A node [N] is a goal node
         if (i) [N] has a conflict involving one of the tokens of
         interest and (ii) [N] corresponds to the goal node, that is,
         the path that leads to [N] in the canonical LR(1) automaton
         leads to the goal node in the merged LR(1) automaton. Note
         that these conditions do not uniquely define [N]. *)

      if shadow == X.goal then
        let can_reduce =
          ref TerminalSet.empty in
        let reductions1 : Production.index list TerminalMap.t =
          Lr1.reductions shadow in
        List.iter (fun (toks, prod) ->
          TerminalSet.iter (fun tok ->

            (* We are looking at a [(tok, prod)] pair -- a reduction
               in the canonical automaton state. *)

            (* Check that this reduction, which exists in the canonical
               automaton state, also exists in the merged automaton --
               that is, it wasn't suppressed by conflict resolution. *)

            if List.mem prod (TerminalMap.lookup tok reductions1) then

              try
                let (_ : Lr1.node) =
                  SymbolMap.find (Symbol.T tok) (Lr1.transitions shadow)
                in
                (* Shift/reduce conflict. *)
                raise (Goal (node, tok))
              with Not_found ->
                let toks = !can_reduce in
                (* We rely on the property that [TerminalSet.add tok toks]
                   preserves physical equality when [tok] is a member of
                   [toks]. *)
                let toks' = TerminalSet.add tok toks in
                if toks == toks' then
                  (* Reduce/reduce conflict. *)
                  raise (Goal (node, tok))
                else
                  (* No conflict so far. *)
                  can_reduce := toks'

          ) toks
        ) (Lr0.reductions state)

    end

  (* Populate the queue with the start nodes. Until we find a goal
     node, take a node out the queue, construct the nodes that
     correspond to its successors, and enqueue them. *)

  let goal, token =
    try

      ProductionMap.iter (fun (prod : Production.index) (k : Lr0.node) ->
        let shadow = try
            ProductionMap.find prod Lr1.entry
          with Not_found ->
            assert false
        in
        if relevant shadow then
          explore None shadow (restrict (Lr0.start k))
      ) Lr0.entry;

      Misc.qiter (fun node ->
        SymbolMap.iter (fun symbol state ->
          try
            let shadow =
              SymbolMap.find symbol (Lr1.transitions node.shadow) in
            if relevant shadow then
              explore (Some (symbol, node)) shadow (restrict state)
          with Not_found ->
            (* No shadow. This can happen if a shift/reduce conflict
               was resolved in favor in reduce. Ignore that transition. *)
            ()
        ) (Lr0.transitions node.state)
      ) queue;

      (* We didn't find a goal node. This shouldn't happen! If the
         goal node in the merged LR(1) automaton has a conflict,
         then there should exist a node with a conflict in the
         canonical automaton as well. Otherwise, Pager's construction
         is incorrect. *)

      raise Oops

    with Goal (node, tok) ->
      node, tok

  (* Query the goal node that was found about the shortest path from
     it to one of the entry nodes. *)

  let source, path =

    let rec follow path node =
      match node.ancestor with
      | None ->
          Lr1.start2item node.shadow, Array.of_list path
      | Some (symbol, node) ->
          follow (symbol :: path) node
    in
    follow [] goal

  let goal =
    Lr0.export goal.state

end
