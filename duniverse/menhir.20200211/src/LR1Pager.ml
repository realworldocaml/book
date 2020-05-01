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

(* This module implements the construction of an LR(1) automaton following a
   version of Pager's algorithm. *)

(* This is a complete re-implementation of Pager's algorithm, by François
   Pottier, on 2020/01/31, following a suggestion by Jacques-Henri Jourdan.
   Our previous implementation of Pager's algorithm exhibited a rare bug that
   could create artificial (unexplainable) conflicts; see issue #21 at
   https://gitlab.inria.fr/fpottier/menhir/issues/21 . *)

(* This code can be viewed as a variant of [LR1Canonical]. However, there, the
   algorithm is just a traversal of a well-defined graph, whereas here, the
   situation is more complex. Indeed, when the algorithm asks the question:

   * What state should be the target of the transition labeled [symbol] out of
   the state [state]?

   the answer *depends on which states have been discovered so far*. In fact,
   the answer to this question can involve an arbitrary choice, as there are
   sometimes several acceptable target states. So, the first phase of our
   algorithm resembles a graph traversal, but the graph's "edges" depend on
   the history of the traversal itself. *)

(* The algorithm is in two phases: a nonstandard graph traversal, followed
   with a standard graph traversal. *)

(* The first traversal is concerned solely with discovering states; it follows
   edges, but does not actually construct them in memory. This traversal can
   discover more states than will be present in the automaton at the end:
   indeed, some states that are discovered along the way can later become
   unreachable, as they are subsumed by larger states, and the graph's edges
   change over time. *)

(* The second traversal determines which states are actually reachable,
   therefore present in the final automaton. It numbers these states. *)

(* This implementation of Pager's algorithm is more flexible than our previous
   implementation in a subtle and interesting way. This explains why issue #21
   was present and is now fixed. Imagine that the state B is created as a
   successor of A along some symbol [symbol]. (The previous algorithm would
   install an edge of A to B.) Now, imagine that, for some reason, the state A
   is subsumed by a larger state A', and for some independent reason, the
   state B is subsumed by a larger state B'. (The previous algorithm would
   grow the two states in place.) Now, the transition out of A' along [symbol]
   must be examined, and it is quite possible that its target B'' is
   incompatible with B', that is, the union of B' and B'' has a conflict that
   does not exist in the canonical automaton. In that case, the previous
   algorithm had painted itself into a corner; there was no way of detecting
   or avoiding this artificial conflict. The new algorithm, on the other hand,
   simply decides that the transition out of A' along [symbol] cannot lead to
   B' and must instead lead to B'' (or to a state that subsumes B''). Indeed,
   the new algorithm does not commit early to which states or edges will exist
   once the dust settles down. *)

type lr0state =
  Lr0.node

type lr1state =
  Lr0.lr1state

open Grammar

module Run () = struct

(* -------------------------------------------------------------------------- *)

(* Give an implicit definition of the graph that we wish to traverse. *)

(* This section is identical to the one found in [LR1Canonical]. *)

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

(* Sets of LR(1) states. *)

(* We expect these sets to have few elements, most of the time, as of the
   automaton produced by Pager's algorithm will have only marginally more
   states than the LALR automaton. So, one could perhaps use lists instead
   of sets. But it's easy to do the right thing here, so let's do it. *)

module S = struct

  include Set.Make(Lr0.Lr1StateAsOrderedType)

  (* [select p s] returns an element of the set [s] that satisfies the
     predicate [p], if such an element exists. It is deterministic: the
     least element that satisfies [p], according to the user-defined
     ordering on elements, is selected. *)

  exception Found of elt

  let select (p : elt -> bool) (s : t) : elt option =
    try
      iter (fun x ->
        if p x then raise (Found x)
      ) s;
      None
    with Found x ->
      Some x

end

(* -------------------------------------------------------------------------- *)

(* Set up a mapping of LR(0) nodes to sets of LR(1) states. This allows us
   to efficiently find all existing LR(1) states that are core-compatible
   with a newly discovered LR(1) state. *)

(* Within each family [families.(c)], all states have the same core [c], no
   two states subsume each other, and no two states are compatible with each
   other. (Two states in the subsumption relation are also compatible, so the
   latter statement is stronger.) *)

let families : S.t array =
  Array.make Lr0.n S.empty

(* -------------------------------------------------------------------------- *)

(* The frontier of the first traversal. This is a set of states that are
   currently scheduled for later examination. *)

let frontier : lr1state Stack.t =
  Stack.create()

let schedule state =
  Stack.push state frontier

(* -------------------------------------------------------------------------- *)

(* [subsume candidate state] determines whether [candidate] is a subset of
   [state], in the sense of set-theoretic inclusion of sets of LR(1) items. *)

(* [compatible candidate state] determines whether [candidate] and [state] are
   compatible according to Pager's weak compatibility criterion, modified so
   as to take end-of-stream conflicts into account. *)

(* Since 2011/01/24, both criteria take error compatibility into account. *)

let subsume candidate state =
  Lr0.subsume candidate state &&
  Lr0.error_compatible candidate state

let compatible candidate state =
  Lr0.compatible candidate state &&
  Lr0.eos_compatible candidate state &&
  Lr0.error_compatible candidate state

(* In the construction mode [ModeInclusionOnly], the compatibility test is
   much weakened. In this mode, compatibility is defined as the symmetric
   closure of subsumption. This means that two states can be merged only if
   one subsumes the other. Thus, we get an LR(1) automaton where every state
   is a state that also exists in the canonical automaton. Thus, it is clear
   that no artificial conflicts can be created by this construction. *)

let compatible candidate state =
  Settings.(
    match construction_mode with
    | ModePager ->
        compatible candidate state
    | ModeInclusionOnly ->
        subsume candidate state || subsume state candidate
    | ModeCanonical
    | ModeLALR ->
        (* We cannot be here. *)
        assert false
  )

(* -------------------------------------------------------------------------- *)

(* Debugging code. *)

let debug =
  false

let rec no_two related xs =
  match xs with
  | [] ->
      true
  | x :: xs ->
      List.for_all (fun x' -> not (related x x')) xs &&
      no_two related xs

let well_formed (family : S.t) : bool =
  let members = S.elements family in
  no_two compatible members

(* -------------------------------------------------------------------------- *)

(* [examine candidate] is invoked whenever some state [state] has just been
   taken out of the frontier and the algorithm has determined that [candidate]
   should normally be its successor along a certain symbol. *)

(* In a standard graph traversal, we would test whether [candidate] has been
   discovered already: if so (1), do nothing; otherwise (2), schedule it. *)

(* Here, case (1) becomes more widely applicable, because we also test whether
   a state that subsumes [candidate] has been discovered already. Furthermore,
   a third case appears: if we find [candidate] is compatible with an existing
   state, then we construct the union of these two states, and schedule it. *)

let rec examine (candidate : lr1state) =

  (* Find out which already-discovered states are core-compatible with the
     candidate state. *)

  let c : lr0state = Lr0.core candidate in
  let family : S.t = families.(c) in
  if debug then assert (well_formed family);

  (* Is the candidate state a subset of an existing state? *)

  (* One might wish to first test [S.mem candidate family], because this test
     runs in logarithmic time, whereas the test [S.exists ..] below runs in
     linear time. However, in most cases, we expect the family to have size
     zero or one, rarely more, so adding such a redundant test does not seem
     to be a good idea. *)

  if S.exists (subsume candidate) family then

    (* Yes, it is. Then, the candidate state does not need to be created, and
       the existing state does not even need to be rescheduled. There is
       nothing to do. *)

    (* This covers the case where the candidate state has been discovered in
       the past. Therefore, a state is never scheduled twice. This implies
       that the algorithm terminates. *)

    ()

  else
    fuse c family candidate

and fuse (c : lr0state) (family : S.t) (candidate : lr1state) =

  (* Is the candidate state Pager-compatible with an existing state? *)

  (* (This covers the case where the candidate subsumes an existing state.) *)

  match S.select (compatible candidate) family with
  | Some state ->
      if debug then assert (not (subsume candidate state));
      if debug then assert (compatible candidate state);

      (* Yes, it is. (The candidate might be compatible with several existing
         states; in that case, an arbitrary choice is made.) Then, we form the
         union of the candidate state and the pre-existing state, and we
         regard it as a new candidate state. *)

      let candidate : lr1state = Lr0.union candidate state in

      (* The existing state [state] is subsumed by the new candidate, so it
         must be removed from the family, so as to maintain the invariant
         that the family consists of pairwise-incompatible states. *)

      (* If there were any transitions whose endpoint was [state], then these
         transitions can safely be redirected towards [candidate], so it is
         fine to remove [state] from consideration; it definitely will not
         be part of the final automaton. *)

      let family = S.remove state family in
      if debug then assert (well_formed family);

      (* There might still be more opportunities for fusion, as the
         candidate state can be compatible with several members of
         the family. *)

      fuse c family candidate

  | None ->

      (* No, it is not. *)

      (* The new candidate is added to the family and scheduled. *)

      let family = S.add candidate family in
      if debug then assert (well_formed family);
      families.(c) <- family;
      schedule candidate

(* -------------------------------------------------------------------------- *)

(* Carry out the first traversal. First schedule the roots, then repeatedly
   extract a node of out the frontier and examine its candidate successors. *)

let () =
  G.foreach_root (fun state ->
    let c = Lr0.core state in
    if debug then assert (S.is_empty families.(c));
    families.(c) <- S.singleton state;
    schedule state
  )

let () =
  while not (Stack.is_empty frontier) do
    let state = Stack.pop frontier in
    G.foreach_successor state examine
  done

(* The first phase is now over. *)

(* -------------------------------------------------------------------------- *)

(* We are now ready for the second phase. *)

(* The array [families] is now read-only. *)

(* The function [examine] can be replaced by [redirect], where we are assured
   that the candidate state must be subsumed by some member of the family. *)

let redirect candidate =
    let c = Lr0.core candidate in
    let family = families.(c) in
    match S.select (subsume candidate) family with
    | Some successor ->
        successor
    | None ->
        (* This cannot happen. Trust me, I have often been wrong before. *)
        assert false

(* The composition of [G.foreach_successor] and [redirect] defines the edges
   of a new graph [G'], whose vertices form a subset of the vertices that we
   have discovered during the first phase. *)

module G' = struct

  include G

  let foreach_successor (state : lr1state) f =
    G.foreach_successor state (fun candidate ->
      let successor = redirect candidate in
      f successor
    )

end

(* Traversing the graph [G'] yields a numbering of its vertices, which are
   the states of the final LR(1) automaton. *)

(* The remainder of this file is identical to [LR1Canonical], except for one
   use of [redirect] in the definition of [transition]. *)

type node =
  int

include Fix.GraphNumbering.ForOrderedType(Lr0.Lr1StateAsOrderedType)(G')
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
  encode (redirect (Lr0.transition symbol (state i)))
    (* note the use of [redirect] *)

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
