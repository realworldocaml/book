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

let () =
  if Settings.graph then
    DependencyGraph.print_dependency_graph()
  (* artificial dependency *)

(* -------------------------------------------------------------------------- *)
(* Explaining shift actions. *)

(* The existence of a shift action stems from the existence of a shift
   item in the LR(0) core that underlies the LR(1) state of interest.
   That is, lookahead sets are not relevant. The existence of a shift
   item in the LR(0) core is explained by finding a path from a start
   item to the shift item in the LR(0) nondeterministic automaton,
   such that the symbols read along this path form the (previously
   fixed) symbol string that leads to the conflict state in the LR(1)
   automaton. There may be several such paths: a shortest one is
   chosen. There may also be several shift items in the conflict
   state: an arbitrary one is chosen. I believe it would not be
   interesting to display traces for several shift items: they would
   be identical except in their last line (where the desired shift
   item actually appears). *)

(* Symbolic execution of the nondeterministic LR(0) automaton. *)

(* Configurations are pairs of an LR(0) item and an offset into
   the input string, which indicates how much has been read so
   far. *)

type configuration0 =
    Item.t * int

(* This function builds a derivation out of a (nonempty, reversed) sequence of
   configurations. The derivation is constructed from bottom to top, that is,
   beginning at the last configuration and moving back towards to the start
   configuration. *)

let rec follow derivation offset' = function
  | [] ->
      assert (offset' = 0);
      derivation
  | (item, offset) :: configs ->
      let _, _, rhs, pos, _ = Item.def item in
      let derivation =
        if offset = offset' then
          (* This is an epsilon transition. Put a new root node on top of
             the existing derivation. *)
          Derivation.build pos rhs derivation None
        else
          (* This was a shift transition. Tack symbol in front of the
             forest. *)
          Derivation.prepend rhs.(pos) derivation
      in
      follow derivation offset configs

(* Symbolic execution begins with a start item (corresponding
   to one of the automaton's entry nodes), a fixed string of
   input symbols, to be fully consumed, and a goal item. The
   objective is to find a path through the automaton that
   leads from the start configuration [(start, 0)] to the goal
   configuration [(stop, n)], where [n] is the length of the
   input string. The automaton is explored via breadth-first
   search. A hash table is used to record which configurations
   have been visited and to build a spanning tree of shortest
   paths. *)

exception Done

let explain_shift_item
    (start : Item.t)
    (input : Symbol.t array)
    (stop : Item.t)
    : Derivation.t =

  let n =
    Array.length input
  in

  let table : (configuration0, configuration0 option) Hashtbl.t =
    Hashtbl.create 1023
  in

  let queue : configuration0 Queue.t =
    Queue.create()
  in

  let enqueue ancestor config =
    try
      let _ = Hashtbl.find table config in
      ()
    with Not_found ->
      Hashtbl.add table config ancestor;
      Queue.add config queue
  in

  enqueue None (start, 0);
  try
    Misc.qiter (function (item, offset) as config ->

      (* If the item we're looking at is the goal item and if
         we have read all of the input symbols, stop. *)

      if (Item.equal item stop) && (offset = n) then
        raise Done;

      (* Otherwise, explore the transitions out of this item. *)

      let prod, _, rhs, pos, length = Item.def item in

      (* Shift transition, followed only if the symbol matches
         the symbol found in the input string. *)

      if (pos < length)
      && (offset < n)
      && (Symbol.equal rhs.(pos) input.(offset)) then begin
        let config' = (Item.import (prod, pos+1), offset+1) in
        enqueue (Some config) config'
      end;

      (* Epsilon transitions. *)

      if pos < length then
        match rhs.(pos) with
        | Symbol.N nt ->
            Production.iternt nt (fun prod ->
              let config' = (Item.import (prod, 0), offset) in
              enqueue (Some config) config'
            )
        | Symbol.T _ ->
            ()

    ) queue;
    assert false
  with Done ->

    (* We have found a (shortest) path from the start configuration to
       the goal configuration. Turn it into an explicit derivation. *)

    let configs = Misc.materialize table (stop, n) in
    let _, _, rhs, pos, _ = Item.def stop in
    let derivation = Derivation.tail pos rhs in
    let derivation = follow derivation n configs in
    derivation

(* -------------------------------------------------------------------------- *)
(* Explaining reduce actions. *)

(* The existence of a reduce action stems from the existence of a
   reduce item, whose lookahead set contains the token of interest, in
   the state of interest. Here, lookahead sets are relevant only
   insofar as they contain or do not contain the token of interest --
   in other words, lookahead sets can be abstracted by Boolean
   values. The existence of the reduce item is explained by finding a
   path from a start item to the reduce item in the LR(1)
   nondeterministic automaton, such that the symbols read along this
   path form the (previously fixed) symbol string that leads to the
   conflict state in the LR(1) automaton. There may be several such
   paths: a shortest one is chosen. *)

(* Symbolic execution of the nondeterministic LR(1) automaton. *)

(* Configurations are pairs of an LR(1) item and an offset into the
   input string, which indicates how much has been read so far. An
   LR(1) item is itself represented as the combination of an LR(0)
   item and a Boolean flag, telling whether the token of interest
   appears or does not appear in the lookahead set. *)

type configuration1 =
    Item.t * bool * int

(* This function builds a derivation out of a sequence of configurations. The
   end of the sequence is dealt with specially -- we want to explain how the
   lookahead symbol appears and is inherited. Once that is done, the rest
   (that is, the beginning) of the derivation is dealt with as above. *)

let config1toconfig0 (item, _, offset) =
  (item, offset)

let rec follow1 tok derivation offset' = function
  | [] ->
      assert (Terminal.equal tok Terminal.sharp);
      (* One could emit a comment saying that the lookahead token is
         initially [#]. That comment would have to be displayed above
         the derivation, though, and there is no support for that
         at the moment, so let's skip it. *)
      derivation
  | (item, _, offset) :: configs ->
      let prod, _, rhs, pos, length = Item.def item in
      if offset = offset' then

        (* This is an epsilon transition. Attack a new line and add
           a comment that explains why the lookahead symbol is
           produced or inherited. *)

        let nullable, first = Analysis.nullable_first_prod prod (pos + 1) in

        if TerminalSet.mem tok first then

          (* The lookahead symbol is produced (and perhaps also inherited,
             but let's ignore that). *)

          let e = Analysis.explain_first_rhs tok rhs (pos + 1) in
          let comment =
            "lookahead token appears" ^ (if e = "" then "" else " because " ^ e)
          in
          let derivation =
            Derivation.build pos rhs derivation (Some comment)
          in

          (* Print the rest of the derivation without paying attention to
             the lookahead symbols. *)

          follow derivation offset (List.map config1toconfig0 configs)

        else begin

          (* The lookahead symbol is not produced, so it is definitely inherited. *)

          assert nullable;

          let comment =
            "lookahead token is inherited" ^
            (if pos + 1 < length then Printf.sprintf " because %scan vanish" (Symbol.printao (pos + 1) rhs) else "")
          in
          let derivation =
            Derivation.build pos rhs derivation (Some comment)
          in

          follow1 tok derivation offset configs

        end

      else

        (* This is a shift transition. Tack symbol in front of forest. *)

        let derivation =
          Derivation.prepend rhs.(pos) derivation
        in

        follow1 tok derivation offset configs

(* Symbolic execution is performed in the same manner as above. *)

let explain_reduce_item
    (tok : Terminal.t)
    (start : Item.t)
    (input : Symbol.t array)
    (stop : Item.t)
    : Derivation.t =

  let n =
    Array.length input
  in

  let table : (configuration1, configuration1 option) Hashtbl.t =
    Hashtbl.create 1023
  in

  let queue : configuration1 Queue.t =
    Queue.create()
  in

  let enqueue ancestor config =
    try
      let _ = Hashtbl.find table config in
      ()
    with Not_found ->
      Hashtbl.add table config ancestor;
      Queue.add config queue
  in

  (* If the lookahead token is #, then it initially appear in the lookahead
     set, otherwise it doesn't. *)

  enqueue None (start, Terminal.equal tok Terminal.sharp, 0);
  try
    Misc.qiter (function (item, lookahead, offset) as config ->

      (* If the item we're looking at is the goal item and if
         we have read all of the input symbols, stop. *)

      if (Item.equal item stop) && lookahead && (offset = n) then
        raise Done;

      (* Otherwise, explore the transitions out of this item. *)

      let prod, _nt, rhs, pos, length = Item.def item in

      (* Shift transition, followed only if the symbol matches
         the symbol found in the input string. *)

      if (pos < length)
      && (offset < n)
      && (Symbol.equal rhs.(pos) input.(offset)) then begin
        let config' = (Item.import (prod, pos+1), lookahead, offset+1) in
        enqueue (Some config) config'
      end;

      (* Epsilon transitions. *)

      if pos < length then
        match rhs.(pos) with
        | Symbol.N nt ->
            let nullable, first = Analysis.nullable_first_prod prod (pos + 1) in
            let first : bool = TerminalSet.mem tok first in
            let lookahead' =
              if nullable then first || lookahead else first
            in
            Production.iternt nt (fun prod ->
              let config' = (Item.import (prod, 0), lookahead', offset) in
              enqueue (Some config) config'
            )
        | Symbol.T _ ->
            ()

    ) queue;
    assert false
  with Done ->

    (* We have found a (shortest) path from the start configuration to
       the goal configuration. Turn it into an explicit derivation. *)

    let configs = Misc.materialize table (stop, true, n) in
    let derivation = Derivation.empty in
    let derivation = follow1 tok derivation n configs in
    derivation

(* -------------------------------------------------------------------------- *)
(* A counter of how many conflicts could *not* be explained. *)

(* When this counter is nonzero, we display a message on the standard output
   channel. This can help us detect regressions via [make test]. *)

let unexplainable =
  ref 0

let log_unexplainable () =
  if !unexplainable > 0 then
    Error.logA 2 (fun f ->
      Printf.fprintf f
        "%d conflict%s could not be explained.\n"
        !unexplainable
        (if !unexplainable > 1 then "s" else "")
    )

(* -------------------------------------------------------------------------- *)
(* Putting it all together. *)

let () =
  if Settings.explain then begin

    (* 2018/09/05: when [--explain] is enabled, always create a fresh
       .conflicts file (wiping out any pre-existing file), even if
       there are in fact no conflicts. This should avoid confusion with
       outdated .conflicts files. *)

    let out =
      open_out (Settings.base ^ ".conflicts")
    in

    Lr1.conflicts (fun toks node ->
    try

      (* Construct a partial LR(1) automaton, looking for a conflict
         in a state that corresponds to this node. Because Pager's
         algorithm can merge two states as soon as one of them has a
         conflict, we can't be too specific about the conflict that we
         expect to find in the canonical automaton. So, we must supply
         a set of conflict tokens and accept any kind of conflict that
         involves one of them. *)

      (* TEMPORARY with the new compatibility criterion, we can be
         sure that every conflict token is indeed involved in a
         conflict. Exploit that? Avoid focusing on a single token? *)

      let module P = Lr1partial.Run (struct
        let tokens = toks
        let goal = node
      end) in

      let closure =
        Lr0.closure P.goal in

      (* Determine what kind of conflict was found. *)

      let shift, reduce = Item.Map.fold (fun item toks (shift, reduce) ->
        match Item.classify item with
        | Item.Shift (Symbol.T tok, _)
          when Terminal.equal tok P.token ->
              shift + 1, reduce
        | Item.Reduce _
          when TerminalSet.mem P.token toks ->
            shift, reduce + 1
        | _ ->
            shift, reduce
      ) closure (0, 0) in

      let kind =
        if (shift > 0) && (reduce > 1) then
          "shift/reduce/reduce"
        else if (shift > 0) then
          "shift/reduce"
        else
          "reduce/reduce"
      in

      (* Explain how the conflict state is reached. *)

      Printf.fprintf out "\n\
        ** Conflict (%s) in state %d.\n\
        ** Token%s involved: %s\n%s\
        ** This state is reached from %s after reading:\n\n%s\n"
      kind (Lr1.number node)
      (if TerminalSet.cardinal toks > 1 then "s" else "")
      (TerminalSet.print toks)
      (if TerminalSet.cardinal toks > 1 then
        Printf.sprintf "** The following explanations concentrate on token %s.\n" (Terminal.print P.token)
      else "")
      (Nonterminal.print false (Item.startnt P.source))
      (Symbol.printa P.path);

      (* Examine the items in that state, focusing on one particular
         token. Out of the shift items, we explain just one -- this
         seems enough. We explain each of the reduce items. *)

      (* First, build a mapping of items to derivations. *)

      let (_ : bool), derivations =
        Item.Map.fold (fun item toks (still_looking_for_shift_item, derivations) ->
          match Item.classify item with

          | Item.Shift (Symbol.T tok, _)
            when still_looking_for_shift_item && (Terminal.equal tok P.token) ->

              false,
              let derivation = explain_shift_item P.source P.path item in
              Item.Map.add item derivation derivations

          | Item.Reduce _
            when TerminalSet.mem P.token toks ->

              still_looking_for_shift_item,
              let derivation = explain_reduce_item P.token P.source P.path item in
              Item.Map.add item derivation derivations

          | _ ->

              still_looking_for_shift_item,
              derivations

        ) closure (true, Item.Map.empty)
      in

      (* Factor out the common context among all derivations, so as to avoid
         repeating it. This helps prevent derivation trees from drifting too
         far away towards the right. It also helps produce sub-derivations
         that are quite compact. *)

      let context, derivations =
        Derivation.factor derivations
      in

      (* Display the common context. *)

      Printf.fprintf out
        "\n** The derivations that appear below have the following common factor:\
         \n** (The question mark symbol (?) represents the spot where the derivations begin to differ.)\n\n";
      Derivation.printc out context;

      (* Then, display the sub-derivations. *)

      Item.Map.iter (fun item derivation ->

        Printf.fprintf out
          "\n** In state %d, looking ahead at %s, "
          (Lr1.number node)
          (Terminal.print P.token);

        begin match Item.classify item with
        | Item.Shift _ ->
            Printf.fprintf out "shifting is permitted\n** because of the following sub-derivation:\n\n"
        | Item.Reduce prod ->
            Printf.fprintf out
              "reducing production\n** %s\n** is permitted because of the following sub-derivation:\n\n"
              (Production.print prod)
        end;

        Derivation.print out derivation

      ) derivations;

      flush out

    with Lr1partial.Oops ->

      (* Ha ha! We were unable to explain this conflict. This could happen
         because the automaton was butchered by conflict resolution directives,
         or because [--lalr] was enabled and we have unexplainable LALR conflicts.
         Anyway, send the error message to the .conflicts file and continue. *)

      incr unexplainable;
      Printf.fprintf out "\n\
        ** Conflict (unexplainable) in state %d.\n\
        ** Token%s involved: %s\n\
        ** %s.\n%!"
      (Lr1.number node)
      (if TerminalSet.cardinal toks > 1 then "s" else "")
      (TerminalSet.print toks)
      (match Settings.construction_mode with
      | Settings.ModeLALR ->
          "This may be an artificial conflict caused by your use of --lalr"
      | Settings.ModeCanonical
      | Settings.ModeInclusionOnly
      | Settings.ModePager ->
          "Please send your grammar to Menhir's developers"
      )

    );
    log_unexplainable();
    Time.tick "Explaining conflicts"

  end

(* ------------------------------------------------------------------------ *)
(* Resolve the conflicts that remain in the automaton. *)

let () =
  Lr1.default_conflict_resolution();
  Time.tick "Resolving remaining conflicts"

(* ------------------------------------------------------------------------ *)
(* Now is as good a time as any to add extra reductions, if requested by the
   user. This must be done after conflicts have been resolved. *)

let () =
  Lr1.extra_reductions();
  Time.tick "Adding extra reductions"

(* ------------------------------------------------------------------------ *)
(* If any warnings about the grammar have been emitted up to this point,
   and if [--strict] is enabled, now is the time to stop, before going
   into the back-end. *)

let () =
  Error.exit_if Error.grammatical_error
