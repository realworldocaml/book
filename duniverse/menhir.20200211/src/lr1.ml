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
open LR1Sigs

(* This module first constructs an LR(1) automaton by using an appropriate
   construction method (LALR, Pager, canonical).
   Then, this automaton is further transformed (in place), in three steps:

   - Silent conflict resolution (without warnings),
     following the user's precedence declarations.
     This is done immediately.
     This can remove transitions and reductions.

   - Default conflict resolution (with warnings),
     following a fixed default policy.
     This is done via an explicit call to [default_conflict_resolution()].
     This can remove reductions.

   - Addition of extra reductions,
     following the user's [%on_error_reduce] declarations.
     This is done via an explicit call to [extra_reductions()].

   Conflicts are explained after step 1, and before steps 2 and 3.
   This is the main reason why these steps are separate. *)

(* -------------------------------------------------------------------------- *)

(* Run the SLR(1) check first. *)

let () =
  Slr.check()

(* -------------------------------------------------------------------------- *)

(* Select a construction algorithm based on the command-line settings. *)

module type ALGORITHM = sig
  module Run () : LR1_AUTOMATON
end

let algo, mode =
  Settings.(match construction_mode with
  | ModeCanonical ->
      (module LR1Canonical : ALGORITHM),
      "canonical"
  | ModeInclusionOnly ->
      (module LR1Pager : ALGORITHM),
      "no-pager"
  | ModePager ->
      (module LR1Pager : ALGORITHM),
      "pager"
  | ModeLALR ->
      (module LALR : ALGORITHM),
      "lalr"
  )

module Algorithm =
  (val algo : ALGORITHM)

let () =
  Error.logA 1 (fun f ->
    Printf.fprintf f
      "The construction mode is %s.\n"
      mode
  )

(* -------------------------------------------------------------------------- *)

(* Run the construction algorithm. *)

module Raw =
  Algorithm.Run()

let () =
  Error.logA 1 (fun f ->
    Printf.fprintf f "Built an LR(1) automaton with %d states.\n" Raw.n
  )

(* -------------------------------------------------------------------------- *)

(* In the following, we perform a depth-first traversal of the raw automaton
   that was built above. As we go, we perform silent conflict resolution
   (which can remove some transitions and therefore make some raw nodes
   unreachable) and we assign consecutive numbers to the reachable nodes. *)

(* We define our own type [node] to be the number of a reachable node in this
   new numbering system. *)

type node =
  int

(* -------------------------------------------------------------------------- *)

(* All of the following mutable state is modified or initialized during the
   depth-first traversal below. *)

(* All of the arrays below are indexed by raw node numbers. *)

module M = struct
  let marked : bool array =
    Array.make Raw.n false
  let mark (node : Raw.node) =
    marked.(Raw.number node) <- true
  let is_marked (node : Raw.node) =
    marked.(Raw.number node)
end

(* This array is initialized during the traversal. We assign new consecutive
   numbers to the reachable nodes. *)

let unreachable =
  -1

let _number : node array =
  Array.make Raw.n unreachable

let transport (raw_node : Raw.node) : node =
  _number.(Raw.number raw_node)

(* This array of transitions is initialized here with the data supplied by
   [Raw.transitions]. Then, some transitions are *removed* (because of
   conflict resolution) during the traversal. *)

let transitions : Raw.node SymbolMap.t array =
  Array.init Raw.n (fun i ->
    Raw.transitions (Raw.node i)
  )

(* Transitions are also stored in reverse, so as to allow reverse traversals
   of the automaton. This array is populated during the traversal. *)

let predecessors : node list array =
  Array.make Raw.n []

(* This array is initialized during the traversal. *)

let reductions : Lr0.reductions array =
  Array.make Raw.n TerminalMap.empty (* dummy *)

(* Tokens for which there are several possible actions in the raw LR(1)
   automaton are known as conflict tokens. This array is populated during the
   traversal. *)

let conflict_tokens : TerminalSet.t array =
  Array.make Raw.n TerminalSet.empty

(* (New as of 2012/01/23.) This flag records whether a shift/reduce conflict
   in this node was solved in favor of neither (%nonassoc). This is later used
   to forbid a default reduction at this node. *)

let forbid_default_reduction : bool array =
  Array.make Raw.n false

(* A count of all reachable nodes. *)

let n =
  ref 0

(* A list of nodes with conflicts. *)

let conflict_nodes : node list ref =
  ref []

(* Counts of nodes with shift/reduce and reduce/reduce conflicts. *)

let shift_reduce =
  ref 0

let reduce_reduce =
  ref 0

(* Count of the shift/reduce conflicts that could be silently
   resolved. *)

let silently_solved =
  ref 0

(* -------------------------------------------------------------------------- *)

(* A view of the raw LR(1) automaton as a graph. *)

(* This view relies on the [transitions] array, as opposed to the function
   [Raw.transitions]. This means that, once an edge has been removed, it can
   no longer be followed. *)

module ForwardEdges = struct
  type node = Raw.node
  type label = Symbol.t
  let foreach_outgoing_edge node f =
    let i = Raw.number node in
    SymbolMap.iter f transitions.(i)
  let foreach_root f =
    ProductionMap.iter (fun _prod node -> f node) Raw.entry
end

(* -------------------------------------------------------------------------- *)

(* This function is invoked during the traversal when a node is discovered. *)

(* It is in charge of detecting and resolving conflicts at this node. *)

let discover (raw_node : Raw.node) =
  let i = Raw.number raw_node
  and state = Raw.state raw_node in

  (* Number this node. *)

  let node : node = Misc.postincrement n in
  _number.(i) <- node;

  (* Detect conflicts. We iterate over the table [Lr0.reductions_table state],
     which gives us all potential reductions. The code is written in such a
     way that we are aware of multi-way shift/reduce/reduce conflicts. We
     solve conflicts, when unambiguously allowed by priorities, by removing
     certain transitions and reductions. *)

  let has_shift_reduce = ref false
  and has_reduce_reduce = ref false in

  let foreach_reduction f =
    TerminalMap.fold f (Lr0.reductions_table state) TerminalMap.empty
  in

  reductions.(i) <- foreach_reduction begin fun tok prods reductions ->
    assert (prods <> []);
    if SymbolMap.mem (Symbol.T tok) transitions.(i) then begin

      (* There is a transition in addition to the reduction(s). We have (at
         least) a shift/reduce conflict. *)

      assert (not (Terminal.equal tok Terminal.sharp));
      if List.length prods = 1 then begin
        let prod = List.hd prods in

        (* This is a single shift/reduce conflict. If priorities tell
           us how to solve it, we follow that and modify the automaton. *)

        match Precedence.shift_reduce tok prod with

        | Precedence.ChooseShift ->

            (* Suppress the reduce action. *)

            incr silently_solved;
            reductions

        | Precedence.ChooseReduce ->

            (* Record the reduce action and suppress the shift transition.
               The automaton is modified in place. This can have the subtle
               effect of making some nodes unreachable. Any conflicts in these
               nodes are then ignored (as they should be). *)

            incr silently_solved;
            transitions.(i) <- SymbolMap.remove (Symbol.T tok) transitions.(i);
            TerminalMap.add tok prods reductions

        | Precedence.ChooseNeither ->

            (* Suppress both the reduce action and the shift transition. *)

            incr silently_solved;
            transitions.(i) <- SymbolMap.remove (Symbol.T tok) transitions.(i);
            forbid_default_reduction.(i) <- true;
            reductions

        | Precedence.DontKnow ->

            (* Priorities don't allow concluding. Record the existence of a
               shift/reduce conflict. *)

            conflict_tokens.(i) <- Grammar.TerminalSet.add tok conflict_tokens.(i);
            has_shift_reduce := true;
            TerminalMap.add tok prods reductions

      end
      else begin

        (* At least two reductions are enabled, so this is a
           shift/reduce/reduce conflict. If the priorities are such that
           each individual shift/reduce conflict is solved in favor of
           shifting or in favor of neither, then solve the entire composite
           conflict in the same way. Otherwise, report the conflict. *)

        let choices = List.map (Precedence.shift_reduce tok) prods in

        if List.for_all (fun choice ->
          match choice with
          | Precedence.ChooseShift -> true
          | _ -> false
        ) choices then begin

          (* Suppress the reduce action. *)

          silently_solved := !silently_solved + List.length prods;
          reductions

        end
        else if List.for_all ((=) Precedence.ChooseNeither) choices then begin

          (* Suppress the reduce action and the shift transition. *)

          silently_solved := !silently_solved + List.length prods;
          transitions.(i) <- SymbolMap.remove (Symbol.T tok) transitions.(i);
          reductions

        end
        else begin

          (* Record a shift/reduce/reduce conflict. Keep all reductions. *)

          conflict_tokens.(i) <- Grammar.TerminalSet.add tok conflict_tokens.(i);
          has_shift_reduce := true;
          has_reduce_reduce := true;
          TerminalMap.add tok prods reductions

        end

      end

    end
    else begin

      (* There is no transition in addition to the reduction(s). *)

      if List.length prods >= 2 then begin
        (* If there are multiple reductions, then we have a pure
           reduce/reduce conflict. Do nothing about it at this point. *)
        conflict_tokens.(i) <- Grammar.TerminalSet.add tok conflict_tokens.(i);
        has_reduce_reduce := true
      end;

      TerminalMap.add tok prods reductions

    end

  end;

  (* Record statistics about conflicts. *)

  if not (TerminalSet.is_empty conflict_tokens.(i)) then begin
    conflict_nodes := node :: !conflict_nodes;
    if !has_shift_reduce then
      incr shift_reduce;
    if !has_reduce_reduce then
      incr reduce_reduce
  end

(* -------------------------------------------------------------------------- *)

(* This function is invoked during the traversal when an edge is traversed. *)

(* It records an edge in the predecessor array. *)

let traverse (source : Raw.node) _symbol (target : Raw.node) =
  (* The source node has been discovered and numbered already, so it can be
     transported. (This is not necessarily true of the target node.) *)
  let j = Raw.number target in
  predecessors.(j) <- transport source :: predecessors.(j)

(* -------------------------------------------------------------------------- *)

(* Perform the depth-first traversal of the raw automaton. *)

let () =
  let module D = struct
    let traverse = traverse
    let discover = discover
  end in
  let module R = DFS.Run(ForwardEdges)(M)(D) in
  ()

let () =
  if !silently_solved = 1 then
    Error.logA 1 (fun f ->
      Printf.fprintf f "One shift/reduce conflict was silently solved.\n"
    )
  else if !silently_solved > 1 then
    Error.logA 1 (fun f ->
      Printf.fprintf f "%d shift/reduce conflicts were silently solved.\n"
        !silently_solved
    );
  if !n < Raw.n then
    Error.logA 1 (fun f ->
      Printf.fprintf f
        "Only %d states remain after resolving shift/reduce conflicts.\n"
        !n
    )

let () =
  Grammar.diagnostics()

(* -------------------------------------------------------------------------- *)

(* Most of our mutable state becomes frozen at this point. Some transitions
   and reductions can still be removed further on, when default conflict
   resolution is performed. Also, some reductions can still be added further
   on, when [%on_error_reduce] declarations are obeyed. *)

let n =
  !n

let conflict_nodes =
  !conflict_nodes

(* We need a mapping of nodes to raw node numbers -- the inverse of the array
   [number]. *)

(* While building this mapping, we must remember that [number.(i)] is
   [unreachable] if the raw node number [i] has never been reached by the
   traversal. *)

let raw : node -> int =
  let raw = Array.make n (-1) (* dummy *) in
  Array.iteri (fun i (* raw index *) (node : node) ->
    assert (0 <= i && i < Raw.n);
    if node <> unreachable then begin
      assert (0 <= node && node < n);
      raw.(node) <- i
    end
  ) _number;
  fun node ->
    assert (0 <= node && node < n);
    raw.(node)

(* The array [transitions] is re-constructed so as to map nodes to nodes
   (instead of raw nodes to raw nodes). This array is now frozen; it is
   no longer modified. *)

let transitions : node SymbolMap.t array =
  Array.init n (fun node ->
    SymbolMap.map transport transitions.(raw node)
  )

(* The array [predecessors] is now frozen. *)

(* The array [reductions] is *not* yet frozen. *)

(* The array [conflict_tokens] is now frozen. *)

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let number node =
  node

let entry =
  ProductionMap.map transport Raw.entry

let state node =
  Raw.state (Raw.node (raw node))

let transitions node =
  assert (0 <= node && node < n);
  transitions.(node)

let set_reductions node table =
  reductions.(raw node) <- table

let reductions node =
  reductions.(raw node)

let predecessors node =
  predecessors.(raw node)

module BackwardEdges = struct
  type nonrec node = node
  type label = unit
  let foreach_outgoing_edge node f =
    List.iter (fun node -> f () node) (predecessors node)
end

let conflict_tokens node =
  conflict_tokens.(raw node)

let conflicts f =
  List.iter (fun node ->
    f (conflict_tokens node) node
  ) conflict_nodes

let forbid_default_reduction node =
  forbid_default_reduction.(raw node)

(* -------------------------------------------------------------------------- *)

(* The incoming symbol of a node can be computed by going through its LR(0)
   core. For this reason, we do not need to explicitly record it here. *)

let incoming_symbol node =
  Lr0.incoming_symbol (Lr0.core (state node))

(* -------------------------------------------------------------------------- *)

(* Iteration over all nodes. *)

let fold f accu =
  let accu = ref accu in
  for node = 0 to n - 1 do
    accu := f !accu node
  done;
  !accu

let iter f =
  for node = 0 to n - 1 do
    f node
  done

let map f =
  List.rev (
    fold (fun accu node ->
      f node :: accu
    ) []
  )

let foldx f =
  fold (fun accu node ->
    match incoming_symbol node with
    | None -> accu
    | Some _ -> f accu node
  )

let iterx f =
  iter (fun node ->
    match incoming_symbol node with
    | None -> ()
    | Some _ -> f node
  )

let tabulate (f : node -> 'a) =
  Misc.tabulate n f

let sum (f : node -> int) =
  Misc.sum n f

(* -------------------------------------------------------------------------- *)

(* We build a map of each symbol to the (reachable) nodes that have this
   incoming symbol. *)

let lookup symbol index =
  try SymbolMap.find symbol index with Not_found -> []

let index : node list SymbolMap.t =
  fold (fun index node ->
    match incoming_symbol node with
    | None ->
        index
    | Some symbol ->
        SymbolMap.add symbol (node :: lookup symbol index) index
  ) SymbolMap.empty

(* This allows iterating over all nodes that are targets of edges carrying a
   certain symbol. The sources of the corresponding edges are also provided. *)

let targets f accu symbol =
  (* There are no incoming transitions on the start symbols. *)
  let targets = lookup symbol index in
  List.fold_left (fun accu target ->
    f accu (predecessors target) target
  ) accu targets

(* -------------------------------------------------------------------------- *)

(* Our output channel. *)

let out =
  lazy (open_out (Settings.base ^ ".automaton"))

(* -------------------------------------------------------------------------- *)

(* If requested, dump a verbose description of the automaton. *)

let describe out node =

  Printf.fprintf out "State %d%s:\n%s"
    (number node)
    (if Settings.follow then Printf.sprintf " (r%d)" (raw node) else "")
    (Lr0.print "" (state node));

  SymbolMap.iter (fun symbol node ->
    Printf.fprintf out "-- On %s shift to state %d\n"
      (Symbol.print symbol) (number node)
  ) (transitions node);

  (* TEMPORARY In the following, one might wish to group all symbols that
     lead to reducing a common production. *)

  TerminalMap.iter (fun tok prods ->
    List.iter (fun prod ->
      Printf.fprintf out "-- On %s " (Terminal.print tok);
      match Production.classify prod with
      | Some nt ->
          Printf.fprintf out "accept %s\n" (Nonterminal.print false nt)
      | None ->
          Printf.fprintf out "reduce production %s\n" (Production.print prod)
    ) prods
  ) (reductions node);

  if not (TerminalSet.is_empty (conflict_tokens node)) then
    Printf.fprintf out "** Conflict on %s\n"
      (TerminalSet.print (conflict_tokens node));

  Printf.fprintf out "\n%!"

let () =
  Time.tick "Construction of the LR(1) automaton";
  if Settings.dump then begin
    iter (describe (Lazy.force out));
    Time.tick "Dumping the LR(1) automaton"
  end

(* -------------------------------------------------------------------------- *)

(* Converting a start node into the single item that it contains. *)

let start2item node =
  let state : Lr0.lr1state = state node in
  let core : Lr0.node = Lr0.core state in
  let items : Item.Set.t = Lr0.items core in
  assert (Item.Set.cardinal items = 1);
  Item.Set.choose items

(* -------------------------------------------------------------------------- *)
(* [has_beforeend s] tests whether the state [s] can reduce a production
   whose semantic action uses [$endpos($0)]. Note that [$startpos] and
   [$endpos] have been expanded away already, so we need not worry about
   the fact that (in an epsilon production) they expand to [$endpos($0)]. *)

let has_beforeend node =
  TerminalMap.fold (fun _ prods accu ->
    accu ||
    let prod = Misc.single prods in
    not (Production.is_start prod) &&
    let action = Production.action prod in
    Action.has_beforeend action
  ) (reductions node) false

(* -------------------------------------------------------------------------- *)
(* Computing which terminal symbols a state is willing to act upon.

   One must keep in mind that, due to the merging of states, a state might be
   willing to perform a reduction on a certain token, yet the reduction can
   take us to another state where this token causes an error. In other words,
   the set of terminal symbols that is computed here is really an
   over-approximation of the set of symbols that will not cause an error. And
   there seems to be no way of performing an exact computation, as we would
   need to know not only the current state, but the contents of the stack as
   well. *)

let acceptable_tokens (s : node) =

  (* If this state is willing to act on the error token, ignore it -- we do
     not wish to report that an error would be accepted in this state :-) *)

  let transitions =
    SymbolMap.remove (Symbol.T Terminal.error) (transitions s)
  and reductions =
    TerminalMap.remove Terminal.error (reductions s)
  in

  (* Accumulate the tokens carried by outgoing transitions. *)

  let covered =
    SymbolMap.fold (fun symbol _ covered ->
      match symbol with
      | Symbol.T tok ->
          TerminalSet.add tok covered
      | Symbol.N _ ->
          covered
    ) transitions TerminalSet.empty
  in

  (* Accumulate the tokens that permit reduction. *)

  let covered =
    ProductionMap.fold (fun _ toks covered ->
      TerminalSet.union toks covered
    ) (Lr0.invert reductions) covered
  in

  (* That's it. *)

  covered

(* -------------------------------------------------------------------------- *)
(* Report statistics. *)

(* Produce the reports. *)

let () =
  if !shift_reduce = 1 then
    Error.grammar_warning [] "one state has shift/reduce conflicts."
  else if !shift_reduce > 1 then
    Error.grammar_warning [] "%d states have shift/reduce conflicts."
      !shift_reduce;
  if !reduce_reduce = 1 then
    Error.grammar_warning [] "one state has reduce/reduce conflicts."
  else if !reduce_reduce > 1 then
    Error.grammar_warning [] "%d states have reduce/reduce conflicts."
      !reduce_reduce

(* -------------------------------------------------------------------------- *)

(* Instantiate [Set] and [Map] on the type [node]. *)

module Node = struct
  type t = node
  let compare = (-)
end

module NodeSet =
  Set.Make(Node)

module NodeMap =
  Map.Make(Node)

(* -------------------------------------------------------------------------- *)

(* For each production, compute where (that is, in which states) this
   production can be reduced. This computation is done AFTER default conflict
   resolution (see below). It is an error to call the accessor function
   [production_where] before default conflict resolution has taken place. *)

let production_where : NodeSet.t ProductionMap.t option ref =
  ref None

let initialize_production_where () =
  production_where := Some (
    fold (fun accu node ->
      TerminalMap.fold (fun _ prods accu ->
        let prod = Misc.single prods in
        let nodes =
          try
            ProductionMap.lookup prod accu
          with Not_found ->
            NodeSet.empty
        in
        ProductionMap.add prod (NodeSet.add node nodes) accu
      ) (reductions node) accu
    ) ProductionMap.empty
  )

let production_where (prod : Production.index) : NodeSet.t =
  match !production_where with
  | None ->
      (* It is an error to call this function before conflict resolution. *)
      assert false
  | Some production_where ->
      try
        (* Production [prod] may be reduced at [nodes]. *)
        let nodes = ProductionMap.lookup prod production_where in
        assert (not (NodeSet.is_empty nodes));
        nodes
      with Not_found ->
        (* The production [prod] is never reduced. *)
        NodeSet.empty

(* -------------------------------------------------------------------------- *)
(* Warn about productions that are never reduced. *)

(* These are productions that can never, ever be reduced, because there is
   no state that is willing to reduce them. There could be other productions
   that are never reduced because the only states that are willing to reduce
   them are unreachable. We do not report those. In fact, through the use of
   the inspection API, it might be possible to bring the automaton into a
   state where one of those productions can be reduced. *)

let warn_about_productions_never_reduced () =
  let count = ref 0 in
  Production.iter (fun prod ->
    if NodeSet.is_empty (production_where prod) then
      match Production.classify prod with
      | Some nt ->
          incr count;
          Error.grammar_warning
            (Nonterminal.positions nt)
            "symbol %s is never accepted." (Nonterminal.print false nt)
      | None ->
          incr count;
          Error.grammar_warning
            (Production.positions prod)
            "production %sis never reduced." (Production.print prod)
  );
  if !count > 0 then
    let plural_mark, be = if !count > 1 then ("s", "are") else ("", "is") in
    Error.grammar_warning []
      "in total, %d production%s %s never reduced." !count plural_mark be

(* -------------------------------------------------------------------------- *)
(* When requested by the code generator, apply default conflict
   resolution to ensure that the automaton is deterministic. *)

(* [best prod prods] chooses which production should be reduced
   among the list [prod :: prods]. It fails if no best choice
   exists. *)

let rec best choice = function
  | [] ->
      choice
  | prod :: prods ->
      match Precedence.reduce_reduce choice prod with
      | Some choice ->
          best choice prods
      | None ->
          (* The cause for not knowing which production is best could be:
             1- the productions originate in different source files;
             2- they are derived, via inlining, from the same production. *)
          Error.signal Error.grammatical_error
            (Production.positions choice @ Production.positions prod)
               "do not know how to resolve a reduce/reduce conflict\n\
                between the following two productions:\n%s\n%s"
                  (Production.print choice)
                  (Production.print prod);
          choice (* dummy *)

(* Go ahead. *)

let default_conflict_resolution () =

  let shift_reduce =
    ref 0
  and reduce_reduce =
    ref 0
  in

  conflict_nodes |> List.iter (fun node ->
    set_reductions node (
      TerminalMap.fold (fun tok prods reductions ->

        try
          let (_ : node) =
            SymbolMap.find (Symbol.T tok) (transitions node)
          in
          (* There is a transition at this symbol, so this
             is a (possibly multiway) shift/reduce conflict.
             Resolve in favor of shifting by suppressing all
             reductions. *)
          shift_reduce := List.length prods + !shift_reduce;
          reductions
        with Not_found ->
          (* There is no transition at this symbol. Check
             whether we have multiple reductions. *)
          match prods with
          | [] ->
              assert false
          | [ _ ] ->
              TerminalMap.add tok prods reductions
          | prod :: ((_ :: _) as prods) ->
              (* We have a reduce/reduce conflict. Resolve, if
                 possible, in favor of a single reduction.
                 This reduction must be preferrable to each
                 of the others. *)
              reduce_reduce := List.length prods + !reduce_reduce;
              TerminalMap.add tok [ best prod prods ] reductions

      ) (reductions node) TerminalMap.empty
    )
  );

  if !shift_reduce = 1 then
    Error.warning [] "one shift/reduce conflict was arbitrarily resolved."
  else if !shift_reduce > 1 then
    Error.warning [] "%d shift/reduce conflicts were arbitrarily resolved."
      !shift_reduce;
  if !reduce_reduce = 1 then
    Error.warning [] "one reduce/reduce conflict was arbitrarily resolved."
  else if !reduce_reduce > 1 then
    Error.warning [] "%d reduce/reduce conflicts were arbitrarily resolved."
      !reduce_reduce;

  (* Now, detect and remove end-of-stream conflicts. If a state has both a
     reduce action at [#] and some other (shift or reduce) action, this is
     an end-of-stream conflict. This conflict is resolved by suppressing
     the reduce action at [#]. *)

  let ambiguities = ref 0 in

  iter begin fun node ->
    let transitions = transitions node
    and reductions = reductions node in

    if Lr0.has_eos_conflict transitions reductions then begin

      (* Suppress the reduce action at [#]. *)
      let prods, reductions =
        TerminalMap.lookup_and_remove Terminal.sharp reductions in
      set_reductions node reductions;
      (* We can assume that there is only one reduction on [#]. *)
      let prod = Misc.single prods in

      (* Count this end-of-stream conflict. *)
      incr ambiguities;

      (* Signal this end-of-stream conflict in the .automaton file. *)
      if Settings.dump then begin

        (* Compute the tokens involved in the transitions and remaining
           reductions. *)
        let toks =
          TerminalSet.union
            (Lr0.transition_tokens transitions)
            (Lr0.reduction_tokens reductions)
        in

        (* Emit a message. *)
        Printf.fprintf (Lazy.force out)
          "State %d has an end-of-stream conflict. There is a tension between\n\
           (1) %s\n\
           without even requesting a lookahead token, and\n\
           (2) checking whether the lookahead token is %s%s,\n\
           which would require some other action.\n\n"
          (number node)
          (match Production.classify prod with
          | Some nt ->
              Printf.sprintf "accepting %s" (Nonterminal.print false nt)
          | None ->
              Printf.sprintf "reducing production %s" (Production.print prod))
          (if TerminalSet.cardinal toks > 1 then "one of " else "")
          (TerminalSet.print toks)

      end

    end
  end;

  if !ambiguities = 1 then
    Error.grammar_warning [] "one state has an end-of-stream conflict."
  else if !ambiguities > 1 then
    Error.grammar_warning [] "%d states have an end-of-stream conflict."
      !ambiguities;

  (* We can now compute where productions are reduced. *)
  initialize_production_where();
  warn_about_productions_never_reduced()

(* -------------------------------------------------------------------------- *)
(* Extra reductions. *)

(* 2015/10/19 Original implementation. *)
(* 2016/07/13 Use priority levels to choose which productions to reduce
              when several productions are eligible. *)

(* If a state can reduce some productions whose left-hand symbol has been
   marked [%on_error_reduce], and if one such production [prod] is preferable
   to every other (according to the priority rules of [%on_error_reduce]
   declarations), then every error action in this state is replaced with a
   reduction of [prod]. This is done even though this state may have outgoing
   shift transitions: thus, we are forcing one interpretation of the past,
   among several possible interpretations. *)

(* The code below looks like the decision on a default reduction in
   [Default], except we do not impose the absence of outgoing terminal
   transitions. Also, we actually modify the automaton, so the back-ends, the
   reference interpreter, etc., need not be aware of this feature, whereas
   they are aware of default reductions. *)

(* This code can run before we decide on the default reductions; this does
   not affect which default reductions will be permitted. *)

(* This code does not affect which productions can be reduced where. Thus,
   it is OK for it to run after [initialize_production_where()]. *)

(* A count of how many states receive extra reductions through this mechanism. *)

let extra =
  ref 0

(* A count of how many states have more than one eligible production, but one
   is preferable to every other (so priority plays a role). *)

let prioritized =
  ref 0

(* The set of nonterminal symbols in the left-hand side of an extra reduction. *)

let extra_nts =
  ref NonterminalSet.empty

let extra_reductions_in_node node =
  (* Compute the productions which this node can reduce. *)
  let productions : _ ProductionMap.t = Lr0.invert (reductions node) in
  let prods : Production.index list =
    ProductionMap.fold (fun prod _ prods -> prod :: prods) productions []
  in
  (* Keep only those whose left-hand symbol is marked [%on_error_reduce]. *)
  let prods = List.filter OnErrorReduce.reduce prods in
  (* Check if one of them is preferable to every other one. *)
  match Misc.best OnErrorReduce.preferable prods with
  | None ->
      (* Either no production is marked [%on_error_reduce], or several of them
         are marked and none is preferable. *)
      ()
  | Some prod ->
      let acceptable = acceptable_tokens node in
      (* An extra reduction is possible. Replace every error action with
         a reduction of [prod]. If we replace at least one error action
         with a reduction, update [extra] and [extra_nts]. *)
      let triggered = lazy (
        incr extra;
        if List.length prods > 1 then incr prioritized;
        extra_nts := NonterminalSet.add (Production.nt prod) !extra_nts
      ) in
      Terminal.iter_real (fun tok ->
        if not (TerminalSet.mem tok acceptable) then begin
          set_reductions node (TerminalMap.add tok [ prod ] (reductions node));
          Lazy.force triggered
        end
      )

let extra_reductions () =
  (* Examine every node. *)
  iter (fun node ->
    (* Just like a default reduction, an extra reduction should be forbidden
       (it seems) if [forbid_default_reduction] is set. *)
    if not (forbid_default_reduction node) then
      extra_reductions_in_node node
  );
  (* Info message. *)
  if !extra > 0 then
    Error.logA 1 (fun f ->
      Printf.fprintf f "Extra reductions on error were added in %d states.\n"
        !extra;
      Printf.fprintf f "Priority played a role in %d of these states.\n"
        !prioritized
    );
  (* Warn about useless %on_error_reduce declarations. *)
  OnErrorReduce.iter (fun nt ->
    if not (NonterminalSet.mem nt !extra_nts) then
      Error.grammar_warning []
        "the declaration %%on_error_reduce %s is never useful."
        (Nonterminal.print false nt)
  )

(* -------------------------------------------------------------------------- *)
(* Define [fold_entry], which in some cases facilitates the use of [entry]. *)

let fold_entry f accu =
  ProductionMap.fold (fun prod state accu ->
    let nt : Nonterminal.t =
      match Production.classify prod with
      | Some nt ->
          nt
      | None ->
          assert false (* this is a start production *)
    in
    let t : Stretch.ocamltype =
      Nonterminal.ocamltype_of_start_symbol nt
    in
    f prod state nt t accu
  ) entry accu

let entry_of_nt nt =
  (* Find the entry state that corresponds to [nt]. *)
  try
    ProductionMap.find (Production.startsymbol2startprod nt) entry
  with Not_found ->
    assert false

exception Found of Nonterminal.t

let nt_of_entry s =
  (* [s] should be an initial state. *)
  assert (incoming_symbol s = None);
  try
    ProductionMap.iter (fun prod entry ->
      if Node.compare s entry = 0 then
        match Production.classify prod with
        | None ->
            assert false
        | Some nt ->
            raise (Found nt)
    ) entry;
    (* This should not happen if [s] is indeed an initial state. *)
    assert false
  with Found nt ->
    nt
