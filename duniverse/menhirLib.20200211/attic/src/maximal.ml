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

(* The so-called maximal LR(1) automaton can be defined as the LR(1) automaton
   obtained by first building the canonical LR(1) automaton, then keeping only
   the states that are maximal with respect to set inclusion. That is, if two
   (core-compatible) states [s0] and [s1] are in a subset relationship, then
   [s0] is dropped and every edge that leads to [s0] is replaced with an edge
   that leads to [s1]. *)

(* Because every state of the maximal automaton is also a state of the
   canonical automaton, every conflict that exists in the maximal automaton
   also exists in the canonical automaton. (This holds for end-of-stream
   conflicts as well.) Thus, the maximal automaton does not have artificial
   conflicts. *)

(* Following the fix introduced in 20110124, we require error compatibility in
   addition to subsumption. This ensures that the maximal automaton does not
   have spurious reductions on the [error] token. *)

(* Based on 344 grammars that currently appear in Menhir's test suite, the
   maximal automaton typically can have as much as 10x more states than the
   LR(0) automaton, whereas the canonical automaton can have as much as 100x
   more states than the LR(0) automaton. *)

(* The maximal automaton can be constructed directly, without first building
   the canonical automaton. Furthermore, its construction can be expressed
   relatively easily as a least fixed point computation. The idea is to map
   each LR(0) core [c] to an irredundant set of LR(1) states, where a set of
   sets is irredundant iff it contains no two comparable elements. *)

type lr0state =
  Lr0.node

type lr1state =
  Lr0.lr1state

open Grammar

(* -------------------------------------------------------------------------- *)

(* A property is an irredundant set of core-compatible LR(1) states. In fact,
   for each LR(0) core [c], we have a different space of properties, namely
   the irredundant sets of LR(1) states whose core is [c]. *)

module P = struct

  (* We represent an irredundant set as an irredundant and sorted list. *)

  (* We could also use an OCaml set data structure, but that would seem
     wasteful, as OCaml sets have built-in tests for redundancy with respect
     to equality, whereas we need to test for redundancy with respect to
     inclusion. *)

  type property =
    lr1state list

  (* Because we represent a set as a list, this conversion is trivial. *)

  let export (p : property) : lr1state list =
    p

  (* The bottom property is the empty set. *)

  let bottom =
    []

  (* Because our lists are sorted, they can be easily tested for equality. *)

  let equal p1 p2 =
    Misc.ListExtras.equal Lr0.equal p1 p2

  (* This definition turns off an optimization in [Fix]. It has nothing to do
     with our use of the word "maximal" in this file. *)

  let is_maximal _p =
    false

  (* A possibly redundant set of LR(1) states is made irredundant as follows. *)

  let subsume (s1 : lr1state) (s2 : lr1state) =
    Lr0.subsume s1 s2 &&
    Lr0.error_compatible s1 s2

  let trim p =
    Misc.trim subsume p

  (* A list of LR(1) states is sorted as follows. (A fixed, arbitrary total
     order is used.) *)

  let sort p =
    List.sort Lr0.compare p

  (* An arbitrary list is made irredundant and sorted as follows. *)

  let import (ss : lr1state list) : property =
    sort (trim ss)

  (* A singleton set is sorted and irredundant, so can be imported as follows. *)

  let singleton (s : lr1state) : property =
    [ s ]

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
        P.singleton (Lr0.start c)
    | Some symbol ->
        (* [c] is not a start state. For every edge of [b] to [c] in the LR(0)
           automaton, we must find out which LR(1) states whose core is [b]
           currently exist, and compute their successors along [symbol]. This
           yields a list of LR(1) states, out of which we keep only the maximal
           elements. *)
        List.fold_left (fun (accu : lr1state list list) b ->
          let ss : lr1state list = P.export (get b) in
          List.map (Lr0.transition symbol) ss :: accu
        ) [] (Lr0.incoming_edges c)
        |> List.concat
        |> P.import

  )

(* -------------------------------------------------------------------------- *)

(* We now force the least fixed computation to take place. (This is implicit;
   it is done just by applying the function [mu] to every LR(0) state). At the
   same time, we count the states of the maximal automaton, and assign a
   unique number to each of them. We build the mapping of states to numbers
   and the reverse mapping of numbers to states. (It should in fact be possible
   to proceed without numbering states at all, but this makes my head hurt.) *)

(* For every LR(0) state [node], the indices [start.(node)] and [finish.(node)]
   delimit a semi-open interval. The indices within this interval correspond to
   the LR(1) states whose core is [node]. *)

type lr1index =
  int

let number, current =
  Lr0.new_numbering()

let start, finish =
  Array.make Lr0.n 0 (* dummy *),
  Array.make Lr0.n 0 (* dummy *)

let m =
  Misc.iteri Lr0.n (fun node ->
    start.(node) <- current();
    mu node
    |> P.export
    |> List.iter (fun s -> ignore (number s));
    finish.(node) <- current()
  );
  current()

(* Manufacture a dummy state so as to initialize the array. Ouch. *)
let dummy : lr1state =
  let _prod, node = ProductionMap.choose Lr0.entry in
  Lr0.start node

let state : lr1index -> lr1state =
  (* Initialize an array of states. *)
  let state = Array.make m dummy in
  (* Populate this array. *)
  Misc.iteri Lr0.n (fun node ->
    mu node
    |> P.export
    |> List.iter (fun s ->
         let i = number s in
         state.(i) <- s
       )
  );
  (* Provide read-only access to this array. *)
  Array.get state

let () =
  Error.logA 3 (fun f ->
    Printf.fprintf f "The maximal automaton has %d states.\n" m
  )

let () =
  Time.tick "Construction of the maximal automaton"

(* -------------------------------------------------------------------------- *)

(* The above code has constructed only the states of the maximal automaton.
   The edges of this automaton can be computed as follows. *)

let transition symbol (source : lr1state) : lr1state =
  (* Compute the successor of the [source] state, along the symbol [symbol],
     in the canonical automaton. *)
  let target = Lr0.transition symbol source in
  (* Because this is the maximal automaton, not the canonical automaton, the
     state [target] does not necessarily exist. Instead, there must exist at
     least one state that subsumes [target]. Find one. This may involve an
     arbitrary choice! *)
  try
    target
    |> Lr0.core
    |> mu
    |> P.export
    |> List.find (P.subsume target)
  with Not_found ->
    assert false (* should not happen *)

let transition symbol (i : lr1index) : lr1index =
  number (transition symbol (state i))

let outgoing_symbols (i : lr1index) =
  Lr0.outgoing_symbols (Lr0.core (state i))

(* -------------------------------------------------------------------------- *)

(* Even though the maximal automaton is usually much smaller than the
   canonical automaton, it can still be quite large. For this reason, we would
   like to merge some of its states, as aggressively as we can, without
   creating artificial conflicts. *)

(* A conflict is artificial if it exists in the automaton after merging but
   does not exist in the automaton before merging. It is worth noting that a
   shift/reduce conflict cannot be artificial. (Indeed, two states can be
   merged only if they have the same core, therefore the same outgoing
   transitions. Therefore, if a shift/reduce conflict exists in a merged
   state, it must have existed in one of the two states before merging.)
   Thus, all artificial conflicts are reduce/reduce conflicts. *)

(* [rrc_tokens state] computes the set of tokens that are involved in a
   reduce/reduce conflict in the LR(1) state [state]. *)

let rrc_tokens (state : lr1state) : TerminalSet.t =
  TerminalMap.fold (fun tok prods toks ->
    (* If several productions can be reduced on the token [tok], then
       it is a reduce/reduce conflict token. *)
    if List.length prods > 1 then
      TerminalSet.add tok toks
    else
      toks
  ) (Lr0.reductions_table state) TerminalSet.empty

(* [max_rrc_tokens node] computes the union of the sets [rrc_tokens state],
   where [state] ranges over all states in the maximal automaton whose LR(0)
   core is [node]. This tells us which tokens are involved in a reduce/reduce
   conflict in the maximal automaton. *)

let max_rrc_tokens (node : Lr0.node) : TerminalSet.t =
  Misc.foldij start.(node) finish.(node) (fun i toks ->
    TerminalSet.union
      (rrc_tokens (state i))
      toks
  ) TerminalSet.empty

(* For efficiency, tabulate the above function. *)

let max_rrc_tokens : Lr0.node -> TerminalSet.t =
  Misc.tabulate Lr0.n max_rrc_tokens

(* [max_eos_conflict node] computes the disjunction of [has_eos_conflict state]
   where [state] ranges over all states in the maximal automaton whose LR(0)
   core is [node]. *)

let max_eos_conflict (node : Lr0.node) : bool =
  Misc.foldij start.(node) finish.(node) (fun i accu ->
    accu || Lr0.has_eos_conflict_lr1state (state i)
  ) false

(* For efficiency, tabulate the above function. *)

let max_eos_conflict : Lr0.node -> bool =
  Misc.tabulate Lr0.n max_eos_conflict

(* Two states [s0] and [s1] can be merged if every reduce/reduce conflict
   token in the merged state [union s0 s1] is already a reduce/reduce conflict
   token *in the maximal automaton*. (and therefore also in the canonical
   automaton). This ensures that every conflict on token [tok] in the merged
   automaton can be explained in terms of a conflict on token [tok] in the
   canonical automaton. *)

(* It is worth noting that the above condition is *more relaxed* than the
   following variant, which may naturally come to mind: "Two states [s0] and
   [s1] can be merged if every reduce/reduce conflict token in the merged
   state [union s0 s1] is already a reduce/reduce conflict token *in [s0] or
   [s1]*". Our more relaxed condition allows us to merge more aggressively. *)

(* This condition is also more relaxed than Pager's weak compatibility
   criterion, which is conservative. See [Lr0.compatible]. *)

let implication (a : bool) (b : bool) : bool =
  not a || b

let can_merge (s0 : lr1state) (s1 : lr1state) : bool =
  assert (Lr0.core s0 = Lr0.core s1);
  let union = Lr0.union s0 s1 in
  let core = Lr0.core s0 in
  (* 1. Require merging to create no mysterious conflicts. *)
  TerminalSet.subset
    (rrc_tokens union)
    (max_rrc_tokens core)
  &&
  (* 2. Require merging to create no end-of-stream conflicts. *)
  implication
    (Lr0.has_eos_conflict_lr1state union)
    (max_eos_conflict core)
  &&
  (* 3. Allow merging error-compatible states only. *)
  Lr0.error_compatible s0 s1

(* -------------------------------------------------------------------------- *)

(* TEMPORARY *)

let weight : Lr0.node -> int =
  (* Compute the strongly connected components of the LR(0) automaton. *)
  let module SCC =
    Tarjan.Run(struct
    type node = Lr0.node
    let n = Lr0.n
    let index node = node
    let successors f node =
      SymbolMap.iter (fun _symbol target ->
        f target
      ) (Lr0.outgoing_edges node)
    let iter f =
      Misc.iteri Lr0.n f
  end) in
  (* Traverse the tree of the strongly connected components, assigning
     an integer weight to each component. The weight of a component is
     its size plus the weights of its children. *)
  let marked = Array.make Lr0.n false
  and w = Array.make Lr0.n 0 in
  let rec weight node : int =
    assert (SCC.representative node = node);
    if marked.(node) then
      w.(node)
    else begin
      marked.(node) <- true;
      let children =
        MenhirLib.General.weed compare (
          SymbolMap.fold (fun _symbol child children ->
            let child = SCC.representative child in
            child :: children
          ) (Lr0.outgoing_edges node) []
        )
      in
      List.length (SCC.scc node) +
      List.fold_left (+) 0 (List.map weight children)
    end
  in
  let weight node = weight (SCC.representative node) in
  weight

let priority node =
  Lr0.n - weight node

(* -------------------------------------------------------------------------- *)

(* Set up a propositional satisfiability problem that describes which of these
   states can or cannot be merged. *)

module H =
  MaxHorn.Make()

(* Set up a mapping of index pairs [(i, j)], where [i] is less than [j], to
   propositional variables. By symmetry, we never need to consider the case
   where [i] is greater than [j]. *)

let fusion : lr1index * lr1index -> H.variable =
  let module M = Memoize.ForType(struct type t = lr1index * lr1index end) in
  M.memoize (fun (i, _j) ->
    let node = Lr0.core (state i) in
    H.new_variable (priority node)
  )

let fusion i j =
  assert (i < j);
  fusion (i, j)

(* For each LR(0) state [node], there are one or more LR(1) states in the
   maximal automaton, whose numbers form the semi-open interval from
   [start.(node)] to [finish.(node)]. *)

(* We construct the propositional satisfiability problem as follows:

   0. For every pair [i < j] in this interval, allocate a variable [f_ij].

   1. If it appears that merging the states [i] and [j] would create a
      conflict, declare that [f_ij] implies [false]. (A unit clause.)

   2. For every triple [i < j < k] in this interval, encode transitivity:
      [f_ij] and [f_jk] imply [f_ik]. (A 3-clause.)
      In fact, because we cannot control whether the pivot state is [i],
      [j], or [k], we need two more clauses:
      [f_ij] and [f_ik] imply [f_jk]
      [f_ik] and [f_jk] imply [f_ij].

   3. For every pair [i < j] in this interval and for every relevant outgoing
      symbol, compute the successors [i'] and [j'] of [i] and [j] along [symbol]
      and declare that [f_ij] implies [f_i'j']. (A 2-clause.)

   It is worth noting that this problem is always satisfiable, by assigning
   the value [false] to every variable, that is, by merging no states at all.
   Our aim is to find a better solution, one that allows us to merge as many
   states as possible. (We won't aim for an optimal solution, as computing it
   could be costly.) *)

let _transition : int array =
  Array.make m 0 (* dummy *)
    (* This array is used below to tabulate the transition function. *)

let () =
  Misc.iteri Lr0.n (fun node ->
    let start = start.(node)
    and finish = finish.(node) in
    for i = start to finish - 1 do
      for j = i + 1 to finish - 1 do
        (* Rule 0. Make sure that the variable [fusion i j] is declared, even if
           (by chance) it is not mentioned in any clause. *)
        let fij = fusion i j in
        (* Rule 1. *)
        if not (can_merge (state i) (state j)) then
          H.declare [ (false, fij ) ];
        (* Rule 2. *)
        for k = j + 1 to finish - 1 do
          let fik = fusion i k
          and fjk = fusion j k in
          H.declare [ (false, fij); (false, fjk); (true, fik) ];
          H.declare [ (false, fij); (false, fik); (true, fjk) ];
          H.declare [ (false, fik); (false, fjk); (true, fij) ]
        done
      done
    done;
    (* Rule 3. *)
    Lr0.outgoing_symbols node |> List.iter (fun symbol ->
      (* Tabulate the function [transition symbol _], as it is
         used quite intensively in the following double loop. *)
      for i = start to finish - 1 do
        _transition.(i) <- transition symbol i
      done;
      for i = start to finish - 1 do
        let i' = _transition.(i) in
        for j = i + 1 to finish - 1 do
          let j' = _transition.(j) in
          if i' <> j' then
            let i', j' = if i' < j' then i', j' else j', i' in
            H.declare [ (false, fusion i j); (true, fusion i' j') ]
        done
      done
    )
  )

let () =
  Time.tick "Construction of the satisfiability problem"

let () =
  Error.logA 3 (fun f ->
    let v, c = H.stats() in
    Printf.fprintf f
      "The satisfiability problem has %d variables and %d clauses.\n"
      v c
  )

(* -------------------------------------------------------------------------- *)

(* Solve the satisfiability problem. *)

let solution : H.variable -> bool =
  H.solve()

let () =
  Time.tick "Solving the satisfiability problem"

(* -------------------------------------------------------------------------- *)

(* We now perform merging. To begin with, we assign consecutive numbers to the
   states of the merged automaton. *)

type node =
  int

let number (node : node) : int =
  node

let node (node : int) : node =
  node

(* The Boolean flag [represented.(i)], where [i] is a state of the original
   (unmerged) automaton, indicates whether a state that corresponds to
   [i] has been created in the merged automaton. *)

let represented : bool array =
  Array.make m false

(* [representative.(i)], where [i] is a state of the original automaton,
   is the number of the state that stands for [i] in the merged automaton. *)

let representative : node array =
  Array.make m (-1) (* dummy *)

(* [represent i s] records the fact that [s] represents [i]. *)

let represent (i : lr1index) (s : node) =
  assert (not represented.(i));
  represented.(i) <- true;
  representative.(i) <- s

(* A number allocator. *)

let (next : unit -> node), (current : unit -> node) =
  let n = ref 0 in
  (fun () -> Misc.postincrement n),
  (fun () -> !n)

(* The numbering pass. *)

let n =
  Misc.iteri Lr0.n (fun node ->
    let start = start.(node)
    and finish = finish.(node) in
    for i = start to finish - 1 do
      if not represented.(i) then begin
        let s = next() in
        represent i s;
        for j = i + 1 to finish - 1 do
          if solution (fusion i j) then
            represent j s
        done
      end
    done
  );
  current()

(* -------------------------------------------------------------------------- *)

(* The entry states. *)

let entry : node ProductionMap.t =
  ProductionMap.map (fun (node : Lr0.node) ->
    (* We know that exactly one node in the maximal automaton corresponds
       to this LR(0) node. Find it, then find the corresponding node in the
       merged automaton. *)
    assert (start.(node) + 1 = finish.(node));
    let i = start.(node) in
    representative.(i)
  ) Lr0.entry

(* -------------------------------------------------------------------------- *)

(* The function [state] maps a state number [s] in the merged automaton to
   the LR(1) state that [s] represents. *)

let state : node -> lr1state =
  (* Allocate an array of options. *)
  let state_ : lr1state option array = Array.make n None in
  (* Populate this array. *)
  Misc.iteri m (fun i ->
    let s = representative.(i) in
    state_.(s) <- Some (
      match state_.(s) with
      | None ->
          state i
      | Some u ->
          Lr0.union u (state i)
    )
  );
  (* Remove the [Some] constructors and provide read access. *)
  Array.get (Array.map Misc.unSome state_)

(* -------------------------------------------------------------------------- *)

(* The transitions of the merged automaton. *)

(* We know that two states [i] and [j] can be merged only if they have the
   same core (therefore the same set of outgoing transition labels) and only
   if their successor states are merged as well. Thus, in order to compute
   the transitions out of a state [s] in the merged automaton, it suffices
   to know the transitions out of [i] in the unmerged) automaton, where [i]
   is an *arbitrary* state such that [representative.(i)] is [s]. *)

let transitions : node -> node SymbolMap.t =
  (* Allocate an array of options. *)
  let transitions_ : node SymbolMap.t option array = Array.make n None in
  (* Populate this array. *)
  Misc.iteri m (fun i ->
    let s = representative.(i) in
    match transitions_.(s) with
    | Some _ ->
        ()
    | None ->
        transitions_.(s) <- Some (
          SymbolMap.init (fun symbol ->
            representative.(transition symbol i)
          ) (outgoing_symbols i)
        )
  );
  (* Remove the [Some] constructors and provide read access. *)
  Array.get (Array.map Misc.unSome transitions_)

(* -------------------------------------------------------------------------- *)

end (* Run *)
