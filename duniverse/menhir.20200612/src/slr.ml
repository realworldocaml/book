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

(* This module extends the LR(0) automaton with lookahead information in order
   to construct an SLR(1) automaton. The lookahead information is obtained by
   considering the FOLLOW sets. *)

(* This construction is not used by Menhir, but can be used to check whether
   the grammar is in the class SLR(1). This check is performed when the log
   level [lg] is at least 1. *)

open Grammar

(* This flag, which is reserved for internal use, causes more information
   about SLR(1) conflict states to be printed. *)

let tell_me_everything =
  false

(* The following function turns an LR(0) state into an SLR(1) state. *)

let make_slr_state (s : Lr0.node) : Lr0.concretelr1state =

  (* Obtain the set of LR(0) items associated with the state [s]. *)

  let items = Lr0.items s in

  (* Unfortunately, this set is not closed. We do not have a function that
     computes the closure of a set of LR(0) items -- we could build one using
     [Item.Closure], but that would be overkill.  So, we first convert this
     set to a set of LR(1) items, then compute the closure at this level, and
     finally we turn this LR(1) state into an SLR(1) state by letting the
     lookahead sets be the FOLLOW sets. This is somewhat ugly and naïve, but
     seems to work. *)

  (* Convert this set to a set of LR(1) items. Here, we can use any set of
     tokens as the lookahead set. We use the empty set. *)

  let s = Item.Map.lift (fun _item -> TerminalSet.empty) items in

  (* Compute the LR(1) closure. *)

  let s = Lr0.closure s in

  (* We now have an LR(1) state that has the correct set of LR(0) items but
     phony lookahead information. We convert it into an SLR(1) state by
     deciding that, for each item, the lookahead set is the FOLLOW set of the
     symbol that appears on the left-hand side of the item. *)

  Item.Map.fold (fun item toks accu ->
    let _, nt, _, _, _ = Item.def item in
    let follow_nt = Analysis.follow nt in
    assert (TerminalSet.subset toks follow_nt); (* sanity check *)
    Item.Map.add item follow_nt accu
  ) s Item.Map.empty

(* The following function turns a closed LR(1) state into a map of terminal
   symbols to reduction actions. Copied from a related function in [Lr0]. *)

let reductions (s : Lr0.concretelr1state) : Production.index list TerminalMap.t =
  Item.Map.fold (fun item toks reductions ->
    match Item.classify item with
    | Item.Reduce prod ->
        Lr0.add_reductions prod toks reductions
    | Item.Shift _ ->
        reductions
  ) s TerminalMap.empty

(* The following function turns a closed LR(1) state into a set of shift
   actions. *)

let transitions (s : Lr0.concretelr1state) : TerminalSet.t =
  Item.Map.fold (fun item _ transitions ->
    match Item.classify item with
    | Item.Shift (Symbol.T tok, _) ->
        TerminalSet.add tok transitions
    | Item.Shift (Symbol.N _, _)
    | Item.Reduce _ ->
        transitions
  ) s TerminalSet.empty

(* This function computes the domain of a terminal map, producing a terminal
   set. *)

let domain (m : 'a TerminalMap.t) : TerminalSet.t =
  TerminalMap.fold (fun tok _ accu ->
    TerminalSet.add tok accu
  ) m TerminalSet.empty

(* The following function checks whether a closed LR(1) state is free of
   conflicts. *)

let state_is_ok (s : Lr0.concretelr1state) : bool =

  let reductions = reductions s
  and transitions = transitions s in

  (* Check for shift/reduce conflicts. *)

  TerminalSet.disjoint transitions (domain reductions) &&

  (* Check for reduce/reduce conflicts. *)

  TerminalMap.fold (fun _ prods ok ->
    ok && match prods with
    | []
    | [ _ ] ->
        true
    | _ :: _ :: _ ->
        false
  ) reductions true

(* The following function counts the number of states in the SLR(1) automaton
   that have a conflict. *)

let count_slr_violations () : int =

  let count = ref 0 in

  for s = 0 to Lr0.n - 1 do
    let s = make_slr_state s in
    if not (state_is_ok s) then begin
      incr count;
      if tell_me_everything then
        Printf.fprintf
          stderr
          "The following SLR(1) state has a conflict:\n%s"
          (Lr0.print_concrete "" s)
    end
  done;

  !count

(* At log level 1, indicate whether the grammar is SLR(1). *)

let check () =
  Error.logG 1 (fun f ->
    let count = count_slr_violations() in
    if count = 0 then
      Printf.fprintf f "The grammar is SLR(1).\n"
    else
      Printf.fprintf f "The grammar is not SLR(1) -- %d states have a conflict.\n" count
  )
