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

(* This file contains experimental code that has been used in an attempt to
   explore the validation of the termination property for LR(1) automata. *)

(* This code is currently unused. It could be plugged in again (at the end of
   the module [Invariant]. *)

(* ------------------------------------------------------------------------ *)
(* Build a graph of all reductions. The vertices are the states of the automaton;
   there is an edge from [s1] to [s2] if a reduction (including the goto step)
   can take us from [s1] to [s2]. Every edge is labeled with its effect on the
   size of the stack. *)

(* This graph is built with respect to a fixed lookahead token [tok]. We
   consider only the reductions that are permitted with [tok] is the next
   token on the stream. *)

exception NoDanger

let clique_count =
  ref 0

let make_reduction_graph tok =

  (* Build the reduction graph for this token. The main part of the work
     is to define the edges; the rest is immediate. *)

  let module ReductionGraph = struct

    type node =
        Lr1.node

    let n =
      Lr1.n

    let index =
      Lr1.number

    (* This auxiliary function pops [n] cells off an abstract stack. The
       parameter [states] is a set of states that we might be in before
       popping. The function returns a set of states that we might be in
       after popping. *)

    let rec pop_word n states w =
      if n = 0 then
        states
      else
        match w with
        | [] ->
            (* the invariant is too weak to ensure that reduction is possible! *)
            assert false
        | (_, states) :: w ->
            pop_word (n-1) states w

    let pop_stack n states (w, _) =
      pop_word n states w

    (* The following function allows listing the successors of a node. Each
       edge is labeled with an integer value that represents the decrease in
       the size of the stack. *)

    let successors (action : int -> node -> unit) node : unit =
      (* Find which reductions are permitted at this node. *)
      let prods =
        match has_default_reduction node with
        | Some (prod, _) ->
            [ prod ]
        | None ->
            try
              TerminalMap.find tok (Lr1.reductions node)
            with Not_found ->
              []
      in
      (* Get a description of the stack at this node. *)
      let stack = lfp node in
      (* For each production [prod], ... *)
      List.iter (fun prod ->
        (* If this is a start production, ignore it. We are not interested in
           accept actions, only in true reduce actions. *)
        if not (Production.is_start prod) then begin
          (* Find out how many cells are popped. *)
          let decrease = Production.length prod in
          (* Find out what states we might be in after popping. *)
          let states = pop_stack decrease (Lr1.NodeSet.singleton node) stack in
          (* Now, the goto step pushes one cell... *)
          let increase = 1 in
          let net = decrease - increase in
          (* Find out which states we might be in after the goto step. *)
          let symbol = Symbol.N (Production.nt prod) in
          let goto (state : Lr1.node) : Lr1.node =
            try
              SymbolMap.find symbol (Lr1.transitions state)
            with Not_found ->
              (* the invariant is too weak to ensure that goto is possible! *)
              assert false
          in
          (* There is a transition, labelled [decrease - increase], from [node]
             to every state in the image through [goto] of the set [states]. *)
          Lr1.NodeSet.iter (fun state ->
            action net (goto state)
          ) states
        end
      ) prods

    let iter =
      Lr1.iter

    (* The [successors] function describes a multi-graph: there might be multiple
       edges with the same source and target nodes. In that case, we would like
       to keep only one, the one with minimum weight, as this is the most dangerous
       one. Do so (naively). *)

    let adjacency : (int * node) list Lr1.NodeMap.t ref =
      ref Lr1.NodeMap.empty

    let () =
      iter (fun source ->
        (* Compute a list of outgoing edges. *)
        let edges = ref [] in
        successors (fun weight target ->
          edges := (weight, target) :: !edges;
        ) source;
        let edges =
          List.sort (fun (weight1, _) (weight2, _) -> weight1 - weight2) !edges
        in
        (* Define a predicate that accepts an edge only the first time
           its target node is seen. *)
        let seen =
          ref Lr1.NodeSet.empty
        in
        let acceptable (_, node) =
          if Lr1.NodeSet.mem node !seen then
            false
          else begin
            seen := Lr1.NodeSet.add node !seen;
            true
          end
        in
        (* Filter the list of edges. This relies on [filter] evaluating the
           predicate left-to-right. *)
        let edges = List.filter acceptable edges in
        (* Augment the table. *)
        adjacency := Lr1.NodeMap.add source edges !adjacency
      )

    let successors (action : int -> node -> unit) source : unit =
      let edges =
        try
          Lr1.NodeMap.find source !adjacency
        with Not_found ->
          assert false
      in
      List.iter (fun (weight, target) ->
        action weight target
      ) edges

  end in

  (* We are interested in determining whether the reduction graph contains
     simple cycles of nonpositive weight. In order to answer this question,
     it is sufficient (and more tractable) to consider each strongly connected
     component separately. *)

  (* Compute the strongly connected components. *)

  let module SCC =
    Tarjan.Run (struct
      include ReductionGraph
      (* Forget the edge labels. *)
      let successors action node =
        successors (fun _ target -> action target) node
    end)
  in

  (* Examine the components, one at a time. *)

  SCC.iter (fun representative elements ->
    match elements with
    | [] ->
        assert false
    | [ _ ] ->
        ()
    | _ ->

        try

          (* We have a non-trivial component. [representative] is its
             representative, and [elements] is the list of its elements. *)

          (* This auxiliary function tests whether a node is a member of
             this component. *)

          let member node =
            Lr1.number (SCC.representative node) = Lr1.number representative
          in

          (* Build a description of this component. *)

          let module Clique = struct

            type node =
                Lr1.node

            (* Re-index the nodes. *)

            let n, node_to_new_index =
              List.fold_left (fun (n, map) node ->
                n + 1, Lr1.NodeMap.add node n map
              ) (0, Lr1.NodeMap.empty) elements

            let index node =
              try
                Lr1.NodeMap.find node node_to_new_index
              with Not_found ->
                assert false

            (* Restrict the edges to only those that remain within this component. *)

            let successors (action : int -> node -> unit) node : unit =
              ReductionGraph.successors (fun label successor ->
                if member successor then
                  action label successor
              ) node

            (* Restrict the vertices to only the elements of this component. *)

            let iter (action : node -> unit) : unit =
              List.iter action elements

          end in

          (* In the following, we perform several tests, of increasing strength and
             cost, to determine whether there is a dangerous cycle in the clique. *)

          (* Check whether at least one edge has nonpositive weight. If that is not
             the case, then there is clearly no dangerous cycle. *)

          let danger =
            ref false
          in

          Clique.iter (fun node ->
            Clique.successors (fun weight _ ->
              if weight <= 0 then
                danger := true
            ) node
          );

          if not !danger then
            raise NoDanger;

          (* Check whether there is at least one edge of negative weight. If not,
             look for a non-trivial strongly connected component among the edges
             of zero weight. *)

          let negative =
            ref false
          in

          Clique.iter (fun node ->
            Clique.successors (fun weight _ ->
              if weight < 0 then
                negative := true
            ) node
          );

          if not !negative then begin
            let module ZeroWeight = struct
              include Clique
              let successors action source =
                successors (fun weight target ->
                  if weight = 0 then
                    action target
                ) source
            end in
            let module ZeroWeightSCC =
              Tarjan.Run (ZeroWeight)
            in
            danger := false;
            ZeroWeightSCC.iter (fun _ elements ->
              if List.length elements > 1 then
                danger := true
            )
          end;

          if not !danger then
            raise NoDanger;

          (* Use Floyd and Warshall's algorithm to determine if there is a dangerous
             cycle. *)

          let module NC =
            NonpositiveCycles.Run(Clique)
          in

          if not NC.graph_has_nonpositive_simple_cycle then
            raise NoDanger;

          (* If there might be danger, then print this clique for manual examination. *)

          let module PrintableClique = struct

            include Clique

            type vertex =
                node

            let name node =
              Printf.sprintf "s%d" (Lr1.number node)

            let successors (action: ?style:Dot.style -> label:string -> vertex -> unit) node : unit =
              successors (fun label successor ->
                action ~label:(string_of_int label) successor
              ) node

            let iter (action: ?style:Dot.style -> label:string -> vertex -> unit) : unit =
              iter (fun node ->
                action ~label:(name node) node
              )

          end in

          let filename =
            Printf.sprintf "%s.%d.dot"
              (Terminal.print tok)
              (Misc.postincrement clique_count)
          in
          let c = open_out filename in
          let module P = Dot.Print(PrintableClique) in
          P.print ~orientation:Dot.Portrait ~size:(8.,5.) c;
          close_out c

        with NoDanger ->
          ()

  )

let () =
  (* The graphs are built, and printed to .dot files, only if requested by
     the user via [--make-reduction-graphs]. The reduction graphs are not
     used by Menhir itself. *)
  if Settings.make_reduction_graphs then begin
    Terminal.iter make_reduction_graph;
    Printf.fprintf stderr "Constructed %d potentially interesting reduction cliques.\n" !clique_count
  end

