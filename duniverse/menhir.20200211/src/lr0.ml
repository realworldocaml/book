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

module InfiniteArray =
  MenhirLib.InfiniteArray

(* ------------------------------------------------------------------------ *)
(* Symbolic lookahead information. *)

(* A symbolic lookahead set consists of an actual concrete set of
   terminal symbols and of a number of set variables. Set variables as
   encoded as integers. *)

module VarSet = SparseBitSet

module SymbolicLookahead = struct

  type t =
    TerminalSet.t * VarSet.t

  let constant toks =
    (toks, VarSet.empty)

  let empty =
    constant TerminalSet.empty

  let union (toks1, vars1) ((toks2, vars2) as s2) =
    let toks = TerminalSet.union toks1 toks2
    and vars = VarSet.union vars1 vars2 in
    if toks2 == toks && vars2 == vars then
      s2
    else
      (toks, vars)

  let variable (var : int) : t =
    (TerminalSet.empty, VarSet.singleton var)

  let project (toks, vars) =
    assert (VarSet.is_empty vars);
    toks

end

(* We will perform closure operations over symbolic lookahead sets.
   This allows us to later represent LR(1) states as pairs of an
   LR(0) node number and an array of concrete lookahead sets. *)

module SymbolicClosure =
  Item.Closure(SymbolicLookahead)

(* Closure operations over concrete lookahead sets are also used (when
   explaining conflicts). One could take another instance of the
   functor. The approach below is somewhat less elegant and makes each
   call to [closure] somewhat slower, but saves the cost of
   instantiating the functor again -- which is linear in the size of
   the grammar. *)

type concretelr1state =
    TerminalSet.t Item.Map.t

let closure (state : concretelr1state) : concretelr1state =
   Item.Map.map SymbolicLookahead.project
     (SymbolicClosure.closure
       (Item.Map.map SymbolicLookahead.constant state))

(* ------------------------------------------------------------------------ *)
(* Finding which non-epsilon transitions leave a set of items. This
   code is parametric in the nature of lookahead sets. *)

let transitions (state : 'a Item.Map.t) : 'a Item.Map.t SymbolMap.t =

  Item.Map.fold (fun item toks transitions ->
    match Item.classify item with
    | Item.Shift (symbol, item') ->
        let items : 'a Item.Map.t =
          try
            SymbolMap.find symbol transitions
          with Not_found ->
            Item.Map.empty
        in
        SymbolMap.add symbol (Item.Map.add item' toks items) transitions
    | Item.Reduce _ ->
        transitions
  ) state SymbolMap.empty

(* ------------------------------------------------------------------------ *)
(* Determining the reduction opportunities at a (closed) state. They
   are represented as a list of pairs of a lookahead set and a
   production index. This code is again parametric in the nature of
   lookahead sets. *)

let reductions (state : 'a Item.Map.t) : ('a * Production.index) list =
  Item.Map.fold (fun item toks accu ->
    match Item.classify item with
    | Item.Reduce prod ->
        (toks, prod) :: accu
    | Item.Shift _ ->
        accu
  ) state []

(* ------------------------------------------------------------------------ *)
(* Construction of the the LR(0) automaton. *)

(* Nodes are numbered sequentially. *)

type node =
    int

(* A symbolic transition is a pair of the target state number and an
   array of symbolic lookahead sets. The variables in these sets are
   numbered in [0,g) where g is the number of items in the source
   LR(0) state. Items are numbered in the order of presentation by
   [Item.Set.fold]. *)

type symbolic_transition_target =
    node * SymbolicLookahead.t array

(* The automaton is represented by (growing) arrays of states (sets of
   items), symbolic transition information, and symbolic reduction
   information, indexed by node numbers. Conversely, a hash table maps
   states (sets of items) to node numbers. *)

let n =
  ref 0

let states : Item.Set.t InfiniteArray.t =
  InfiniteArray.make Item.Set.empty

let _transitions : symbolic_transition_target SymbolMap.t InfiniteArray.t =
  InfiniteArray.make SymbolMap.empty

let _reductions : (SymbolicLookahead.t * Production.index) list InfiniteArray.t =
  InfiniteArray.make []

let map : (Item.Set.t, node) Hashtbl.t =
  Hashtbl.create 50021

let incoming : Symbol.t option InfiniteArray.t =
  InfiniteArray.make None

(* The automaton is built depth-first. *)

let rec explore (symbol : Symbol.t option) (state : Item.Set.t) : node =

  (* Find out whether this state was already explored. *)

  try
    Hashtbl.find map state
  with Not_found ->

    (* If not, create a new node. *)

    let k = !n in
    n := k + 1;
    InfiniteArray.set states k state;
    Hashtbl.add map state k;

    (* Record its incoming symbol. *)

    InfiniteArray.set incoming k symbol;

    (* Build a symbolic version of the current state, where each item
       is associated with a distinct lookahead set variable, numbered
       consecutively. *)

    let (_ : int), (symbolic_state : SymbolicClosure.state) =
      Item.Set.fold (fun item (i, symbolic_state) ->
        i+1, Item.Map.add item (SymbolicLookahead.variable i) symbolic_state
      ) state (0, Item.Map.empty) in

    (* Compute the symbolic closure. *)

    let closure = SymbolicClosure.closure symbolic_state in

    (* Compute symbolic information about reductions. *)

    InfiniteArray.set _reductions k (reductions closure);

    (* Compute symbolic information about the transitions, and, by
       dropping the symbolic lookahead information, explore the
       transitions to further LR(0) states. *)

    InfiniteArray.set _transitions k (SymbolMap.mapi (fun symbol symbolic_state ->
      let (k : node) = explore (Some symbol) (Item.Map.domain symbolic_state) in
      let lookahead : SymbolicLookahead.t array =
        Array.make (Item.Map.cardinal symbolic_state) SymbolicLookahead.empty in
      let (_ : int) = Item.Map.fold (fun _ s i ->
        lookahead.(i) <- s;
        i+1
      ) symbolic_state 0 in
      ((k, lookahead) : symbolic_transition_target)
    ) (transitions closure));

    k

(* Creating a start state out of a start production. It contains a
   single item, consisting of the start production, at position 0. *)

let start prod : Item.Set.t =
   Item.Set.singleton (Item.import (prod, 0))

(* This starts the construction of the automaton and records the
   entry nodes in an array. *)

let entry : node ProductionMap.t =
  ProductionMap.start (fun prod ->
    explore None (start prod)
  )

let () =
  Hashtbl.clear map

let n =
  !n

let () =
  Error.logA 1 (fun f -> Printf.fprintf f "Built an LR(0) automaton with %d states.\n" n);
  Time.tick "Construction of the LR(0) automaton"

(* ------------------------------------------------------------------------ *)
(* Accessors. *)

let items node : Item.Set.t =
  InfiniteArray.get states node

let incoming_symbol node : Symbol.t option =
  InfiniteArray.get incoming node

let outgoing_edges node : node SymbolMap.t =
  SymbolMap.map
    (fun (target, _) -> target)
    (InfiniteArray.get _transitions node)

let outgoing_symbols node : Symbol.t list =
  SymbolMap.domain (InfiniteArray.get _transitions node)

(* Efficient access to the predecessors of an LR(0) state requires building
   a reversed graph. This is done on the first invocation of the function
   [predecessors]. Our measurements show that it typically takes less than
   0.01s anyway. *)

let predecessors : node list array Lazy.t =
  lazy (
    let predecessors = Array.make n [] in
    for source = 0 to n-1 do
      SymbolMap.iter (fun _symbol (target, _) ->
        predecessors.(target) <- source :: predecessors.(target)
      ) (InfiniteArray.get _transitions source)
    done;
    predecessors
  )

let incoming_edges (c : node) : node list =
  (Lazy.force predecessors).(c)

(* ------------------------------------------------------------------------ *)
(* Help for building the LR(1) automaton. *)

(* An LR(1) state is represented as a pair of an LR(0) state number
   and an array of concrete lookahead sets (whose length depends on
   the LR(0) state). *)

type lr1state =
    node * TerminalSet.t array

(* A view of the type [lr1state] as an ordered type. This can be used in
   conjuction with [Fix.Memoize], [Fix.Numbering], etc. E.g. a numbering
   facility based on this mechanism is able to number 10000 states in
   about 0.01s. *)

module Lr1StateAsOrderedType = struct
  type t = lr1state
  let compare (k1, toksr1) (k2, toksr2) =
    let c = k1 - k2 in
    if c <> 0 then c
    else Generic.compare toksr1 toksr2
      (* In principle, we should use [Array.compare TerminalSet.compare],
         but the function [Array.compare] does not exist, and we happen
         to know that [TerminalSet.compare] is OCaml's generic comparison,
         so the whole comparison can be carried using generic comparison. *)
end

(* An encoded LR(1) state can be turned into a concrete representation,
   that is, a mapping of items to concrete lookahead sets. *)

let export (k, toksr) =
  let (_ : int), items = Item.Set.fold (fun item (i, items) ->
    i+1, Item.Map.add item toksr.(i) items
  ) (InfiniteArray.get states k) (0, Item.Map.empty) in
  items

(* Displaying a concrete state. *)

let print_concrete leading (state : concretelr1state) =
  let buffer = Buffer.create 1024 in
  Item.Map.iter (fun item toks ->
    Printf.bprintf buffer "%s%s[ %s ]\n"
      leading
      (Item.print item)
      (TerminalSet.print toks)
  ) state;
  Buffer.contents buffer

(* Displaying a state. By default, only the kernel is displayed, not
   the closure. *)

let print leading state =
  print_concrete leading (export state)

let print_closure leading state =
  print_concrete leading (closure (export state))

(* The core of an LR(1) state is the underlying LR(0) state. *)

let core (k, _) =
  k

(* A sanity check. *)

let well_formed (k, toksr) =
  Array.length toksr = Item.Set.cardinal (InfiniteArray.get states k)

(* An LR(1) start state is the combination of an LR(0) start state
   (which consists of a single item) with a singleton lookahead set
   that consists of the end-of-file pseudo-token. *)

let start k =
  let state = (k, [| TerminalSet.singleton Terminal.sharp |]) in
  assert (well_formed state);
  state

(* Interpreting a symbolic lookahead set with respect to a source
   state. The variables in the symbolic lookahead set (which are
   integers) are interpreted as indices into the state's array of
   concrete lookahead sets. The result is a concrete lookahead set. *)

let interpret
    ((_, toksr) as state : lr1state)
    ((toks, vars) : SymbolicLookahead.t)
    : TerminalSet.t =

  assert (well_formed state);
  VarSet.fold (fun var toks ->
    assert (var >= 0 && var < Array.length toksr);
    TerminalSet.union toksr.(var) toks
  ) vars toks

(* Out of an LR(1) state, one produces information about reductions
   and transitions. This is done in an efficient way by interpreting
   the precomputed symbolic information with respect to that state. *)

let reductions
    ((k, _) as state : lr1state)
    : (TerminalSet.t * Production.index) list =

  List.map (fun (s, prod) ->
    interpret state s, prod
  ) (InfiniteArray.get _reductions k)

let transitions
    ((k, _) as state : lr1state)
    : lr1state SymbolMap.t =

  SymbolMap.map (fun ((k, sr) : symbolic_transition_target) ->
    ((k, Array.map (interpret state) sr) : lr1state)
  ) (InfiniteArray.get _transitions k)

let transition
    symbol
    ((k, _) as state : lr1state)
    : lr1state =

  let ((k, sr) : symbolic_transition_target) =
    try
      SymbolMap.find symbol (InfiniteArray.get _transitions k)
    with Not_found ->
      assert false (* no transition along this symbol *)
  in
  (k, Array.map (interpret state) sr)

(* [transition_tokens transitions] returns the set of tokens (terminal symbols)
   that are labels of outgoing transitions in the table [transitions]. *)

let transition_tokens transitions =
  SymbolMap.fold (fun symbol _target toks ->
    match symbol with
    | Symbol.T tok -> TerminalSet.add tok toks
    | Symbol.N _   -> toks
  ) transitions TerminalSet.empty

(* Equality of states. *)

let equal ((k1, toksr1) as state1) ((k2, toksr2) as state2) =
  assert (k1 = k2 && well_formed state1 && well_formed state2);
  let rec loop i =
    if i = 0 then
      true
    else
      let i = i - 1 in
      (TerminalSet.equal toksr1.(i) toksr2.(i)) && (loop i)
  in
  loop (Array.length toksr1)

(* Subsumption between states. *)

let subsume ((k1, toksr1) as state1) ((k2, toksr2) as state2) =
  assert (k1 = k2 && well_formed state1 && well_formed state2);
  let rec loop i =
    if i = 0 then
      true
    else
      let i = i - 1 in
      (TerminalSet.subset toksr1.(i) toksr2.(i)) && (loop i)
  in
  loop (Array.length toksr1)

(* This function determines whether two (core-equivalent) states are
   compatible, according to a criterion that is close to Pager's weak
   compatibility criterion.

   Pager's criterion guarantees that if a merged state has a potential
   conflict at [(i, j)] -- that is, some token [t] appears within the
   lookahead sets of both item [i] and item [j] -- then there exists a
   state in the canonical automaton that also has a potential conflict
   at [(i, j)] -- that is, some token [u] appears within the lookahead
   sets of both item [i] and item [j]. Note that [t] and [u] can be
   distinct.

   Pager has shown that his weak compatibility criterion is stable,
   that is, preserved by transitions and closure. This means that, if
   two states can be merged, then so can their successors. This is
   important, because merging two states means committing to merging
   their successors, even though we have not even built these
   successors yet.

   The criterion used here is a slightly more restrictive version of
   Pager's criterion, which guarantees equality of the tokens [t] and
   [u]. This is done essentially by applying Pager's original
   criterion on a token-wise basis. Pager's original criterion states
   that two states can be merged if the new state has no conflict or
   one of the original states has a conflict. Our more restrictive
   criterion states that two states can be merged if, for every token
   [t], the new state has no conflict at [t] or one of the original
   states has a conflict at [t].

   This modified criterion is also stable. My experiments show that it
   is almost as effective in practice: out of more than a hundred
   real-world sample grammars, only one automaton was affected, and
   only one extra state appeared as a result of using the modified
   criterion. Its advantage is to potentially make conflict
   explanations easier: if there appears to be a conflict at [t], then
   some conflict at [t] can be explained. This was not true when using
   Pager's original criterion. *)

(* A word of caution: reasoning about compatibility is tricky and often
   counter-intuitive. Here is a list of properties and non-properties:

   - Compatibility is reflexive and symmetric.

   - Compatibility is *not* transitive.

   - If two states A and B are in the subumption relation (i.e., one is a
     subset of the other), then A and B are compatible.

   - Compatibility is *not* monotonic. That is, it is *not* the case that if
     two states A and B are incompatible, then two larger states A' and B'
     must be incompatible as well. (The fact that the state A U B is
     compatible with itself shows that this is false.) In the contrapositive,
     it is *not* the case that if A and B are compatible, then two smaller
     states A' and B' must be compatible as well.

   - Compatibility is preserved by union of compatible states. That is, if
     A and B are compatible, then C is compatible with (A U B) if and only
     if C is compatible with both A and B. *)

let compatible (k1, toksr1) (k2, toksr2) =
  assert (k1 = k2);
  let n = Array.length toksr1 in
  (* Two states are compatible if and only if they are compatible
     at every pair (i, j), where i and j are distinct. *)
  let rec loopi i =
    if i = n then
      true
    else
      let toksr1i = toksr1.(i)
      and toksr2i = toksr2.(i) in
      let rec loopj j =
        if j = i then
          true
        else
          let toksr1j = toksr1.(j)
          and toksr2j = toksr2.(j) in

          (* The two states are compatible at (i, j) if every conflict
             token in the merged state already was a conflict token in
             one of the two original states. This could be written as
             follows:

            TerminalSet.subset
              (TerminalSet.inter (TerminalSet.union toksr1i toksr2i) (TerminalSet.union toksr1j toksr2j))
              (TerminalSet.union (TerminalSet.inter toksr1i toksr1j) (TerminalSet.inter toksr2i toksr2j))

             but is easily seen (on paper) to be equivalent to:

          *)

             TerminalSet.subset
               (TerminalSet.inter toksr2i toksr1j)
               (TerminalSet.union toksr1i toksr2j)
          &&
             TerminalSet.subset
               (TerminalSet.inter toksr1i toksr2j)
               (TerminalSet.union toksr2i toksr1j)
          &&
             loopj (j+1)
      in
      loopj 0 && loopi (i+1)
  in
  loopi 0

(* This function determines whether two (core-equivalent) states can
   be merged without creating an end-of-stream conflict, now or in the
   future.

   The rule is, if an item appears in one state with the singleton "#"
   as its lookahead set, then its lookahead set in the other state
   must contain "#".

   So, either the second lookahead set is also the singleton "#", and
   no end-of-stream conflict exists, or it is larger, and the second
   state already contains an end-of-stream conflict.

   Put another way, we do not want to merge two lookahead sets when one
   contains "#" alone and the other does not contain "#".

   I invented this rule to complement Pager's criterion. I believe,
   but I am not 100% sure, that it does indeed prevent end-of-stream
   conflicts and that it is stable.

   Thanks to Sébastien Hinderer for reporting the bug caused by the
   absence of this extra criterion. *)

let eos_compatible  (k1, toksr1) (k2, toksr2) =
  assert (k1 = k2);
  let n = Array.length toksr1 in
  let rec loop i =
    if i = n then
      true
    else
      let toks1 = toksr1.(i)
      and toks2 = toksr2.(i) in
      begin
        if TerminalSet.mem Terminal.sharp toks1 && TerminalSet.is_singleton toks1 then
          (* "#" is alone in one set: it must be a member of the other set. *)
          TerminalSet.mem Terminal.sharp toks2
        else if TerminalSet.mem Terminal.sharp toks2 && TerminalSet.is_singleton toks2 then
          (* Symmetric condition. *)
          TerminalSet.mem Terminal.sharp toks1
        else
          true
      end
      && loop (i+1)
  in
  loop 0

(* This function determines whether two (core-equivalent) states can
   be merged without creating spurious reductions on the [error]
   token.

   The rule is, we merge two states only if they agree on which
   reductions are permitted on the [error] token.

   Without this restriction, we might end up in a situation where we
   decide to introduce an [error] token into the input stream and
   perform a reduction, whereas a canonical LR(1) automaton,
   confronted with the same input string, would fail normally -- that
   is, it would introduce an [error] token into the input stream, but
   it would not be able to perform a reduction right away: the current
   state would be discarded.

   In the interest of more accurate (or sane, or predictable) error
   handling, I decided to introduce this restriction as of 20110124.
   This will cause an increase in the size of automata for grammars
   that use the [error] token. It might actually make the [error]
   token somewhat easier to use.

   Note that two sets can be in the subsumption relation and still
   be error-incompatible. Error-compatibility requires equality of
   the lookahead sets, restricted to [error].

   Thanks to Didier Rémy for reporting a bug caused by the absence
   of this extra criterion. *)

let error_compatible  (k1, toksr1) (k2, toksr2) =
  assert (k1 = k2);
  let n = Array.length toksr1 in
  let rec loop i =
    if i = n then
      true
    else
      let toks1 = toksr1.(i)
      and toks2 = toksr2.(i) in
      begin
        if TerminalSet.mem Terminal.error toks1 then
          (* [error] is a member of one set: it must be a member of the other set. *)
          TerminalSet.mem Terminal.error toks2
        else if TerminalSet.mem Terminal.error toks2 then
          (* Symmetric condition. *)
          TerminalSet.mem Terminal.error toks1
        else
          true
      end
      && loop (i+1)
  in
  loop 0

(* Union of two states. The two states must have the same core. The
   new state is obtained by pointwise union of the lookahead sets. *)

let union (k1, toksr1) (k2, toksr2) =
  assert (k1 = k2);
  k1, Array.init (Array.length toksr1) (fun i ->
    TerminalSet.union toksr1.(i) toksr2.(i)
  )

(* Restriction of a state to a set of tokens of interest. Every
   lookahead set is intersected with that set. *)

let restrict toks (k, toksr) =
  k, Array.map (fun toksri ->
    TerminalSet.inter toksri toks
  ) toksr

(* A (state-local, possibly nondeterministic) reduction table maps
   terminal symbols to lists of productions. *)

type reductions =
  Production.index list TerminalMap.t

(* [add_reduction prod tok reductions] adds a reduction of [prod] on [tok]
   to the table [reductions]. *)

let add_reduction prod tok reductions =
  let prods =
    try
      TerminalMap.lookup tok reductions
    with Not_found ->
      []
  in
  TerminalMap.add tok (prod :: prods) reductions

(* [add_reductions prod toks reductions] adds a reduction of [prod] on every
   token in the set [toks] to the table [reductions]. *)

let add_reductions prod toks reductions =
  TerminalSet.fold (add_reduction prod) toks reductions

let reductions_table state =
  List.fold_left (fun reductions (toks, prod) ->
    add_reductions prod toks reductions
  ) TerminalMap.empty (reductions state)

(* [reduction_tokens reductions] returns the domain of the reductions table
   [table], in the form of a set of tokens. *)

let reduction_tokens reductions =
  TerminalMap.fold (fun tok _prods toks ->
      TerminalSet.add tok toks
  ) reductions TerminalSet.empty

(* This inverts a mapping of tokens to productions into a mapping of
   productions to sets of tokens. *)

(* This is needed, in [CodeBackend], to avoid producing two (or more)
   separate branches that call the same [reduce] function. Instead,
   we generate just one branch, guarded by a [POr] pattern. *)

let invert reductions : TerminalSet.t ProductionMap.t =
  TerminalMap.fold (fun tok prods inverse ->
    let prod = Misc.single prods in
    let toks =
      try
        ProductionMap.lookup prod inverse
      with Not_found ->
        TerminalSet.empty
    in
    ProductionMap.add prod (TerminalSet.add tok toks) inverse
  ) reductions ProductionMap.empty

(* [has_eos_conflict transitions reductions] tells whether a state has
   an end-of-stream conflict, that is, a reduction action on [#] and
   at least one other (shift or reduce) action. *)

let has_eos_conflict transitions reductions =
  match TerminalMap.lookup_and_remove Terminal.sharp reductions with
  | exception Not_found ->
      (* There is no reduction action on [#], thus no conflict. *)
      false
  | prods, reductions ->
      (* There is at least one reduction action on [#]. *)
      (* If there are two reduction actions on [#], then we have a conflict. *)
      List.length prods > 1 ||
      (* If there only one reduction on [#], then we have a conflict if and
         only if either there exists another shift or reduce action. *)
      not (TerminalMap.is_empty reductions) ||
      SymbolMap.exists (fun symbol _ -> Symbol.is_terminal symbol) transitions

let has_eos_conflict_lr1state (state : lr1state) =
  has_eos_conflict
    (transitions state)
    (reductions_table state)
