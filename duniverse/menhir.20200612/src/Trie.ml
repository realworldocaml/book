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

(* -------------------------------------------------------------------------- *)

(* We begin with a number of auxiliary functions that provide information
   about the LR(1) automaton. These functions could perhaps be moved
   elsewhere, e.g., inside [Default]. We keep them here, for now, because
   they are not used anywhere else. *)

(* [can_reduce s prod] indicates whether state [s] is able to reduce
   production [prod] (either as a default reduction, or as a normal
   reduction). *)

let can_reduce s prod =
  match Default.has_default_reduction s with
  | Some (prod', _) when prod = prod' ->
      true
  | _ ->
      TerminalMap.fold (fun z prods accu ->
        (* A reduction on [#] is always a default reduction. (See [lr1.ml].) *)
        assert (not (Terminal.equal z Terminal.sharp));
        accu || Terminal.non_error z && List.mem prod prods
      ) (Lr1.reductions s) false

(* [reduction_path_exists s w prod] tests whether the path determined by the
   sequence of symbols [w] out of the state [s] exists in the automaton and
   leads to a state where [prod] can be reduced. It further requires [w] to
   not contain the [error] token. *)

let rec reduction_path_exists s (w : Symbol.t list) prod : bool =
  match w with
  | [] ->
      can_reduce s prod
  | a :: w ->
      Symbol.non_error a &&
      match SymbolMap.find a (Lr1.transitions s) with
      | s ->
          reduction_path_exists s w prod
      | exception Not_found ->
          false

(* -------------------------------------------------------------------------- *)

(* Tries. *)

module Make (X : sig end) = struct

  (* A trie has the following structure. *)

  type trie = {
    (* A unique identity, used by [compare]. The trie construction code
       ensures that these numbers are indeed unique: see [fresh], [insert],
       [star]. *)
    identity: int;
    (* The root state of this star: "where we come from". *)
    source: Lr1.node;
    (* The current state, i.e., the root of this sub-trie: "where we are". *)
    current: Lr1.node;
    (* The productions that we can reduce in the current state. In other
       words, if this list is nonempty, then the current state is the end
       of one (or several) branches. It can nonetheless have children. *)
    mutable productions: Production.index list;
    (* The children, or sub-tries. *)
    mutable transitions: trie SymbolMap.t
    (* The two fields above are written only during the construction of a
       trie. Once every trie has been constructed, they are frozen. *)
  }

  (* This counter is used by [mktrie] to produce unique identities. *)
  let c = ref 0

  (* We keep a mapping of integer identities to tries. Whenever a new
     identity is assigned, this mapping must be updated. *)
  let tries =
    let s : Lr1.node = Obj.magic () in (* yes, this hurts *)
    let dummy = { identity = -1; source = s; current = s;
                  productions = []; transitions = SymbolMap.empty } in
    MenhirLib.InfiniteArray.make dummy

  (* This smart constructor creates a new trie with a unique identity. *)
  let mktrie source current productions transitions =
    let identity = Misc.postincrement c in
    let t = { identity; source; current; productions; transitions } in
    MenhirLib.InfiniteArray.set tries identity t;
    t

  (* [insert t w prod] updates the trie (in place) by adding a new branch,
     corresponding to the sequence of symbols [w], and ending with a reduction
     of production [prod]. We assume [reduction_path_exists w prod t.current]
     holds, so we need not worry about this being a dead branch, and we can
     use destructive updates without having to set up an undo mechanism. *)

  let rec insert (t : trie) (w : Symbol.t list) prod : unit =
    match w with
    | [] ->
        assert (can_reduce t.current prod);
        t.productions <- prod :: t.productions
    | a :: w ->
        match SymbolMap.find a (Lr1.transitions t.current) with
        | exception Not_found ->
            assert false
        | successor ->
            (* Find our child at [a], or create it. *)
            let t' =
              try
                SymbolMap.find a t.transitions
              with Not_found ->
                let t' = mktrie t.source successor [] SymbolMap.empty in
                t.transitions <- SymbolMap.add a t' t.transitions;
                t'
            in
            (* Update our child. *)
            insert t' w prod

  (* [insert t prod] inserts a new branch, corresponding to production
     [prod], into the trie [t], which is updated in place. *)
  let insert t prod : unit =
    let w = Array.to_list (Production.rhs prod) in
    (* Check whether the path [w] leads to a state where [prod] can be
       reduced. If not, then some transition or reduction action must
       have been suppressed by conflict resolution; or the path [w]
       involves the [error] token. In that case, the branch is dead,
       and is not added. This test is superfluous (i.e., it would
       be OK to add a dead branch) but allows us to build a slightly
       smaller star in some cases. *)
    if reduction_path_exists t.current w prod then
      insert t w prod

  (* [fresh s] creates a new empty trie whose source is [s]. *)
  let fresh source =
    mktrie source source [] SymbolMap.empty

  (* The star at [s] is obtained by starting with a fresh empty trie and
     inserting into it every production [prod] whose left-hand side [nt]
     is the label of an outgoing edge at [s]. *)
  let star s =
    let t = fresh s in
    SymbolMap.iter (fun sym _ ->
      match sym with
      | Symbol.T _ ->
          ()
      | Symbol.N nt ->
          Production.iternt nt (insert t)
    ) (Lr1.transitions s);
    t

  (* A trie [t] is nontrivial if it has at least one branch, i.e., contains at
     least one sub-trie whose [productions] field is nonempty. Trivia: a trie
     of size greater than 1 is necessarily nontrivial, but the converse is not
     true: a nontrivial trie can have size 1. (This occurs if all productions
     have zero length.) *)
  let trivial t =
    t.productions = [] && SymbolMap.is_empty t.transitions

  (* Redefine [star] to record the size of the newly built trie. *)

  let size =
    Array.make Lr1.n (-1)

  let star s =
    let initial = !c in
    let t = star s in
    let final = !c in
    size.(Lr1.number s) <- final - initial;
    t

  (* Define [stars] to build all stars and pass all nontrivial ones to [f]. *)

  let stars f =
    (* For every state [s]... *)
    Lr1.iter (fun s ->
      (* Build the trie rooted at [s]. If it is nontrivial, invoke [f]. *)
      let t = star s in
      if not (trivial t) then
        f s t
    )

  let size s =
    assert (size.(s) >= 0);
    size.(s)

  let total_size () =
    !c

  let compare t1 t2 =
    Generic.compare t1.identity t2.identity

  let source t =
    t.source

  let current t =
    t.current

  let accepts prod t =
    List.mem prod t.productions

  let step a t =
    SymbolMap.find a t.transitions (* careful: may raise [Not_found] *)

  let verbose () =
    Printf.eprintf "Total star size: %d\n%!" (total_size())

  let decode i =
    let t = MenhirLib.InfiniteArray.get tries i in
    assert (t.identity = i); (* ensure we do not get the [dummy] trie *)
    t

  let encode t =
    assert (decode t.identity == t); (* round-trip property *)
    t.identity

end
