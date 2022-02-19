(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar

(**The functors [Run] and [Dummy] assume that the height of the known suffix
   of the stack is known at every state. This height information must be
   consistent: the height at a state [s] must be no greater than the minimum
   of the height at the predecessors of [s], plus one. *)
module type STACK_HEIGHTS = sig

  (**[stack_height s] is the height of the known suffix of the stack
     at state [s]. *)
  val stack_height: Lr1.node -> int

  (**[production_height prod] is the height of the known suffix of the stack
     at a state where production [prod] can be reduced. *)
  val production_height: Production.index -> int

  (**[goto_height nt] is the height of the known suffix of the stack at a
     state where an edge labeled [nt] has just been followed. *)
  val goto_height: Nonterminal.t -> int

  (**The string [variant] should be "short" or "long" and is printed
     when --timings is enabled. *)
  val variant: string

end

(**This signature describes the output of the functors [Run] and [Dummy]. *)
module type STACK_STATES = sig

  (**A property is a description of the known suffix of the stack at state
     [s]. It is represented as an array. By convention, the top of the stack
     is the end of the array. Each array element is a set of states that may
     appear in this stack cell. *)
  type property =
    Lr1.NodeSet.t array

  (**[stack_states s] is the known suffix of the stack at state [s]. *)
  val stack_states: Lr1.node -> property

  (**[production_states prod] is the known suffix of the stack at a state
     where production [prod] can be reduced. In the short invariant, the
     length of this suffix is [Production.length prod]. In the long
     invariant, its length can be greater. *)
  val production_states: Production.index -> property

  (**[goto_states nt] is the known suffix of the stack at a state where an
     edge labeled [nt] has just been followed. If [long] is false, then the
     length of this suffix is [1]. If [long] is true, then its length can be
     greater. *)
  val goto_states: Nonterminal.t -> property

  (* At log level [-lc 3], the result of the analysis is logged, in an
     unspecified format. *)

end

module Run (S : sig

  (**[stack_height s] is the height of the known suffix of the stack
     at state [s]. *)
  val stack_height: Lr1.node -> int

  (**[production_height prod] is the height of the known suffix of the stack
     at a state where production [prod] can be reduced. *)
  val production_height: Production.index -> int

  (**[goto_height nt] is the height of the known suffix of the stack at a
     state where an edge labeled [nt] has just been followed. *)
  val goto_height: Nonterminal.t -> int

  (**The string [variant] should be "short" or "long" and is printed
     when --timings is enabled. *)
  val variant: string

end) = struct

open S

(* We now wish to compute, at each state [s], a vector of sets of states,
   whose length is [stack_height s].  *)

(* Define the data flow graph. *)

(* Its vertices are the stack cells of interest. (2021/10/15) Defining a
   data flow graph where vertices are individual stack cells and properties
   are sets of states is preferable to one where vertices are nodes and
   properties are vectors of sets of states. On large automata, the speed
   difference can be more than 2x. *)

module G = struct

  (* A stack cell is identified by a node [s] in the LR(1) automaton and
     an index [i] into the known suffix of the stack at state [s]. This
     index is comprised between 0 and [stack_height s], excluded. *)

  (* Unlike our usual convention, here, the top stack cell is numbered 0,
     the next cell is numbered 1, and so on. (That is, we count from the
     right towards the left.) *)

  type variable =
    Lr1.node * int

  (* To each cell, we wish to associate a set of states. *)

  type property =
    Lr1.NodeSet.t

  let leq_join =
    Lr1.NodeSet.leq_join

  (* For each transition in the automaton, the cell at index 0 in the target
     node is a root of the data flow analysis. *)

  let foreach_root contribute =
    Lr1.iter (fun source ->
      let property = Lr1.NodeSet.singleton source in
      Lr1.transitions source |> SymbolMap.iter (fun _symbol target ->
        assert (0 < stack_height target);
        contribute (target, 0) property
      )
    )

  (* The edges of the data flow graph are the transitions of the automaton.
     Along each transition, the cell at index [i] at the source node flows
     into the cell at index [i+1] at the target node, provided the latter
     cell exists. (The stack at the target is truncated so as to avoid
     obtaining a vector that is longer than expected/necessary.) *)

  (* It is interesting to note that the property flows, but is not
     transformed: this is a graph reachability problem in disguise.
     It is really just a matter of computing which PUSHes reach which
     stack cells. *)

  let foreach_successor (source, i) states contribute =
    Lr1.transitions source |> SymbolMap.iter (fun _symbol target ->
      if i + 1 < stack_height target then
        contribute (target, i + 1) states
    )

end

(* Maps. *)

module M =
  Fix.Glue.HashTablesAsImperativeMaps(struct
    type t = G.variable
    let equal (t1 : t) (t2 : t) = (t1 = t2)
    let hash (t : t) = Hashtbl.hash t
  end)

(* Compute the least fixed point. *)

let stack_states : G.variable -> G.property option =
  let module F = Fix.DataFlow.Run(M)(G)(G) in
  F.solution

(* If every state is reachable, then the least fixed point must be non-[None]
   everywhere, so we may view it as a function that produces a vector of sets
   of states. *)

let stack_states (cell : G.variable) : G.property =
  assert (let (s, i) = cell in 0 <= i && i < stack_height s);
  match stack_states cell with
  | None ->
      (* Apparently this node is unreachable. *)
      assert false
  | Some states ->
      states

(* To the end user, we want to propose an API that is based on vectors of
   sets of states. *)

type property =
  Lr1.NodeSet.t array

(* Adapt [stack_states] to the external API. *)

let stack_states : Lr1.node -> property =
  Lr1.tabulate (fun node ->
    let n = stack_height node in
    Array.init n (fun i ->
      let i = n - 1 - i in
      stack_states (node, i)
    )
  )

(* [truncate_join height f nodes] computes a join of the images through [f] of
   the nodes in the set [nodes], truncated at height [height]. *)

let bottom height : property =
  Array.make height Lr1.NodeSet.empty

let truncate k (v : property) : property =
  assert (k <= Array.length v);
  MArray.truncate k v

let leq_join (v1 : property) (v2 : property) : property =
  MArray.leq_join Lr1.NodeSet.leq_join v1 v2

let truncate_join height (f : Lr1.node -> property) nodes =
  Lr1.NodeSet.fold (fun node accu ->
    leq_join (truncate height (f node)) accu
  ) nodes (bottom height)

(* From the above information, deduce, for each production, the shape
   of the stack when this production is reduced. *)

(* We produce a vector of states whose length is [production_height prod].
   It is up to the user to provide an appropriate height oracle. *)

let production_states : Production.index -> property =
  Production.tabulate (fun prod ->
    let sites = Lr1.production_where prod in
    let height = production_height prod in
    truncate_join height stack_states sites
  )

(* Compute the shape of the stack when a transition on the nonterminal
   symbol [nt] is taken. *)

(* We produce a vector of states whose length is [goto_height nt].
   It is up to the user to provide an appropriate height oracle. *)

let goto_states : Nonterminal.t -> property =
  Nonterminal.tabulate (fun nt ->
    let symbol = Symbol.N nt in
    (* Compute the join of the stack shapes at every target of an edge
       labeled with [nt]. *)
    let targets = Lr1.all_targets symbol in
    let height = goto_height nt in
    truncate_join height stack_states targets
  )

(* Debugging output. *)

let print (v : property) =
  if Array.length v = 0 then
    "epsilon"
  else
    Misc.separated_list_to_string Lr1.NodeSet.print "; " (Array.to_list v)

(* Dump. *)

let () =
  Error.logC 3 (fun f ->
    Lr1.iter (fun node ->
      Printf.fprintf f "%sstack(%s) = %s\n"
        variant
        (Lr1.print node)
        (print (stack_states node))
    );
    Production.iterx (fun prod ->
      Printf.fprintf f "%sprodstack(%s) = %s\n"
        variant
        (Production.print prod)
        (print (production_states prod))
    )
  )

let () =
  Time.tick (Printf.sprintf "Computing stack states (%s)" variant)

end (* Run *)

(* -------------------------------------------------------------------------- *)

(* The following dummy module is used to skip the above computation. *)

module Dummy (S : STACK_HEIGHTS) = struct

  type property =
    Lr1.NodeSet.t array

  let bottom height : property =
    Array.make height Lr1.NodeSet.empty

  let dummy =
    bottom

  let stack_states node =
    dummy (S.stack_height node)

  let production_states prod =
    dummy (S.production_height prod)

  let goto_states nt =
    dummy (S.goto_height nt)

end
