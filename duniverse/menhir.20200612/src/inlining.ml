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

let position = Positions.position
open Keyword
type sw = Action.sw
open BasicSyntax
open ListMonad
let drop = MenhirLib.General.drop
let take = MenhirLib.General.take

(* -------------------------------------------------------------------------- *)

(* Throughout this file, branches (productions) are represented as lists of
   producers. We consider it acceptable to perform operations whose cost is
   linear in the length of a production, even when (with more complicated
   code) it would be possible to eliminate this cost. *)

(* -------------------------------------------------------------------------- *)

(* [search p i xs] searches the list [xs] for an element [x] that satisfies [p].
   If successful, then it returns a pair of: - [i] plus the offset of [x] in the
   list, and - the element [x]. *)

let rec search (p : 'a -> bool) (i : int) (xs : 'a list) : (int * 'a) option =
  match xs with
  | [] ->
      None
  | x :: xs ->
      if p x then Some (i, x) else search p (i+1) xs

(* [search_at p i xs] searches the list [xs] for an element [x] that satisfies
   [p]. The search begins at index [i] in the list. If successful, then it
   returns a pair of: - the offset of [x] in the list, and - the element [x]. *)

let search_at p i xs =
  search p i (drop i xs)

(* -------------------------------------------------------------------------- *)

(* [find grammar symbol] looks up the definition of [symbol], which must be
   a valid nonterminal symbol, in the grammar [grammar]. *)

let find grammar symbol : rule =
  try
    StringMap.find symbol grammar.rules
  with Not_found ->
    (* This cannot happen. *)
    assert false

(* -------------------------------------------------------------------------- *)

(* [check_no_producer_attributes] checks that a producer, which represents a
   use site of an %inline symbol, does not carry any attributes. This ensures
   that we need not worry about propagating attributes through inlining. *)

let check_no_producer_attributes producer =
  match producer_attributes producer with
  | [] ->
      ()
  | (id, _payload) :: _attributes ->
      Error.error
        [position id]
        "the nonterminal symbol %s is declared %%inline.\n\
         A use of it cannot carry an attribute."
        (producer_symbol producer)

(* -------------------------------------------------------------------------- *)

(* 2015/11/18. The interaction of %prec and %inline is not documented.
   It used to be the case that we would disallow marking a production
   both %inline and %prec. Now, we allow it, but we check that (1) it
   is inlined at the last position of the host production and (2) the
   host production does not already have a %prec annotation. *)

let check_prec_inline caller producer nsuffix callee =
  callee.branch_prec_annotation |> Option.iter (fun callee_prec ->
    (* The callee has a %prec annotation. *)
    (* Check condition 1. *)
    if nsuffix > 0 then begin
      let symbol = producer_symbol producer in
      Error.error [ position callee_prec; caller.branch_position ]
        "this production carries a %%prec annotation,\n\
         and the nonterminal symbol %s is marked %%inline.\n\
         For this reason, %s can be used only in tail position."
        symbol symbol
    end;
    (* Check condition 2. *)
    caller.branch_prec_annotation |> Option.iter (fun caller_prec ->
      let symbol = producer_symbol producer in
      Error.error [ position callee_prec; position caller_prec ]
        "this production carries a %%prec annotation,\n\
         and the nonterminal symbol %s is marked %%inline.\n\
         For this reason, %s cannot be used in a production\n\
         which itself carries a %%prec annotation."
        symbol symbol
    )
  )

(* -------------------------------------------------------------------------- *)

(* 2015/11/18. If the callee has a %prec annotation (which implies that the
   caller does not have one, and that the callee appears in tail position in
   the caller) then the annotation is inherited. This seems reasonable, but
   remains undocumented. *)

let propagate_prec_annotation caller callee =
  match callee.branch_prec_annotation with
  | (Some _) as annotation ->
      assert (caller.branch_prec_annotation = None);
      annotation
  | None ->
      caller.branch_prec_annotation

(* -------------------------------------------------------------------------- *)

(* [new_candidate x] is a candidate fresh name, which is based on [x] in an
   unspecified way. A fairly arbitrary construction can be used here; we just
   need it to produce an infinite sequence of names, so that eventually we are
   certain to be able to escape any finite set of unavailable names. We also
   need this construction to produce reasonably concise names, as it can be
   iterated several times in practice; I have observed up to 9 iterations in
   real-world grammars. *)

(* Here, the idea is to add a suffix of the form _inlined['0'-'9']+ to the
   name [x], if it does not already include such a suffix. If [x] already
   carries such a suffix, then we increment the integer number. *)

let new_candidate x =
  let x, n = ChopInlined.chop (Lexing.from_string x) in
  Printf.sprintf "%s_inlined%d" x (n + 1)

(* [fresh names x] returns a fresh name that is not in the set [names].
   The new name is obtained by iterating [new_candidate] until we fall
   outside the set [names]. *)

let rec fresh names x =
  if StringSet.mem x names then fresh names (new_candidate x) else x

(* -------------------------------------------------------------------------- *)

(* [rename used producers] renames the producers [producers] of the inlined
   branch (the callee) if necessary to avoid a clash with the set [used] of
   the names used by the producers of the host branch (the caller). This set
   need not contain the name of the producer that is inlined away. *)

(* This function produces a pair of: 1. a substitution [phi], which represents
   the renaming that we have performed, and which must be applied to the
   semantic action of the callee; 2. the renamed [producers]. *)

let rename (used : StringSet.t) producers: Action.subst * producers =
  let phi, _used, producers =
    List.fold_left (fun (phi, used, producers) producer ->
      let id = producer_identifier_located producer in
      let x = Positions.value id in
      if StringSet.mem x used then
        let x' = fresh used x in
        let id' = Positions.map (fun _x -> x') id in
        (x, x') :: phi,
        StringSet.add x' used,
        { producer with producer_identifier = id' } :: producers
      else
        (phi, StringSet.add x used, producer :: producers)
    ) ([], used, []) producers
  in
  phi, List.rev producers

(* -------------------------------------------------------------------------- *)

(* [define_positions] defines how the start and end positions of the callee
   should be computed once it is inlined into the caller. This information is
   used to transform [$startpos] and [$endpos] in the callee and to transform
   [$startpos(x)] and [$endpos(x)] in the caller. *)

(* 2015/11/04. We ensure that positions are computed in the same manner,
   regardless of whether inlining is performed. *)

(* The arguments of this function are as follows:

   [name]       an array of the names of the producers of the new branch
   [nprefix]    the length of the prefix of the caller, up to the inlining site
   [ncallee]    the length of the callee

   The results are as follows:

   [startp]     how to transform $startpos in the callee
   [endp]       how to transform $endpos in the callee
   [beforeendp] how to transform $endpos($0) in the callee

 *)

let define_positions (name : string array) nprefix ncallee : sw * sw * sw =

  let startp =
    if ncallee > 0 then
      (* If the inner production is non-epsilon, things are easy. The start
         position of the inner production is the start position of its first
         element. *)
      RightNamed name.(nprefix), WhereStart
    else if nprefix > 0 then
      (* If the inner production is epsilon, we are supposed to compute the
         end position of whatever comes in front of it. If the prefix is
         nonempty, then this is the end position of the last symbol in the
         prefix. *)
      RightNamed (name.(nprefix - 1)), WhereEnd
    else
      (* If the inner production is epsilon and the prefix is empty, then
         we need to look up the end position stored in the top stack cell.
         This is the reason why we need the keyword [$endpos($0)]. It is
         required in this case to preserve the semantics of $startpos and
         $endpos. *)
      Before, WhereEnd

    (* Note that, to contrary to intuition perhaps, we do NOT have that
       if the prefix is empty, then the start position of the inner
       production is the start production of the outer production.
       This is true only if the inner production is non-epsilon. *)

  in

  let endp =
    if ncallee > 0 then
      (* If the inner production is non-epsilon, things are easy: its end
         position is the end position of its last element. *)
      RightNamed (name.(nprefix + ncallee - 1)), WhereEnd
    else
      (* If the inner production is epsilon, then its end position is equal
         to its start position. *)
      startp

  (* We must also transform [$endpos($0)] if it used by the inner
     production. It refers to the end position of the stack cell
     that comes before the inner production. So, if the prefix is
     non-empty, then it translates to the end position of the last
     element of the prefix. Otherwise, it translates to [$endpos($0)]. *)

  and beforeendp =
    if nprefix > 0 then
      RightNamed (name.(nprefix - 1)), WhereEnd
    else
      Before, WhereEnd

  in
  startp, endp, beforeendp

(* -------------------------------------------------------------------------- *)

(* [rename_sw_outer] transforms the keywords in the outer production (the
   caller) during inlining. It replaces [$startpos(x)] and [$endpos(x)], where
   [x] is the name of the callee, with [startpx] and [endpx], respectively. *)

let rename_sw_outer (x, startpx, endpx) (sw : sw) : sw option =
  match sw with
  | Before, _ ->
      None
  | RightNamed x', where ->
      if x' = x then
        match where with
        | WhereStart -> Some startpx
        | WhereEnd   -> Some endpx
        | WhereSymbolStart -> assert false (* has been expanded away *)
      else
        None
  | Left, _ ->
      (* [$startpos], [$endpos], and [$symbolstartpos] have been expanded away
         earlier; see [KeywordExpansion]. *)
      assert false

(* -------------------------------------------------------------------------- *)

(* [rename_sw_inner] transforms the keywords in the inner production (the callee)
   during inlining. It replaces [$endpos($0)] with [beforeendp]. *)

let rename_sw_inner beforeendp (sw : sw) : sw option =
  match sw with
  | Before, where ->
      assert (where = WhereEnd);
      Some beforeendp
  | RightNamed _, _ ->
      None
  | Left, _ ->
      (* [$startpos] and [$endpos] have been expanded away earlier; see
         [KeywordExpansion]. *)
      assert false

(* -------------------------------------------------------------------------- *)

(* [inline_branch caller site callee] inlines the branch [callee] into the
   branch [caller] at the site [site]. By convention, a site is a pair of an
   integer index -- the index [i] of the producer that must be inlined away --
   and a producer [producer] -- the producer itself. This is redundant, as
   [producer] can be recovered based on [caller] and [i], but convenient. *)

type site =
  int * producer

let inline_branch caller (i, producer : site) (callee : branch) : branch =

  (* The host branch (the caller) is divided into three sections: a prefix
     of length [nprefix], the producer that we wish to inline away, and a
     suffix of length [nsuffix]. *)

  (* Compute the length of the prefix and suffix. *)

  let nprefix = i in
  let nsuffix = List.length caller.producers - (i + 1) in

  (* Construct the prefix and suffix. *)

  let prefix = take nprefix caller.producers
  and suffix = drop (nprefix + 1) caller.producers in

  (* Apply the (undocumented) restrictions that concern the interaction
     between %prec and %inline. Then, (possibly) propagate a %prec
     annotation. *)
  check_prec_inline caller producer nsuffix callee;
  let branch_prec_annotation = propagate_prec_annotation caller callee in

  (* Compute the names of the producers in the host branch (the caller), minus
     the one that is being inlined away. Rename the producers of the inlined
     branch (the callee), if necessary, so as to avoid a clash with this set.
     The goal is to guarantee that, after inlining, all producers in the newly
     constructed branch have unique names. *)
  let used = StringSet.union (names prefix) (names suffix) in
  let phi, inlined_producers = rename used callee.producers in

  (* Construct (the producers of) the new branch. The prefix and suffix of the
     caller are preserved. In the middle, [producer] disappears and is replaced
     with [inlined_producers]. For debugging purposes, check that each producer
     in the new branch carries a unique name. *)
  let producers = prefix @ inlined_producers @ suffix in
  let (_ : StringSet.t) = names producers in

  (* Find out how the start and end positions of the callee should be computed
     once it is inlined into the caller. *)

  let startp, endp, beforeendp =
    let name = producers |> Array.of_list |> Array.map producer_identifier in
    let ncallee = List.length callee.producers in
    define_positions name nprefix ncallee
  in

  (* Apply appropriate renamings to the semantic actions of the caller and
     callee, then compose them using a [let] binding. If [x] is the name of
     the producer that we wish to inline away, then the variable [x] in the
     caller's semantic action should refer to the semantic value produced by
     the callee's semantic action. *)

  let x = producer_identifier producer in
  let caller_action, callee_action =
    Action.rename (rename_sw_outer (x, startp, endp)) [] caller.action,
    Action.rename (rename_sw_inner beforeendp) phi callee.action
  in
  let action = Action.compose x callee_action caller_action in

  (* We are done! Build a new branch. *)

  let { branch_position; branch_production_level; _ } = caller in
  {
    branch_position;
    producers;
    action;
    branch_prec_annotation;
    branch_production_level;
  }

(* -------------------------------------------------------------------------- *)

(* Inlining a list of branches [callees] into the branch [caller] at [site]. *)

let inline_branches caller site (callees : branches) : branches =
  List.map (inline_branch caller site) callees

(* -------------------------------------------------------------------------- *)

(* For greater syntactic convenience, the main function is written as a
   functor, and re-packaged as a function at the very end. *)

(* Roughly speaking, the transformation is implemented by two mutually
   recursive functions. [expand_branches] transforms a list of branches into a
   list of (expanded) branches; [expand_symbol] maps a nonterminal symbol
   (which may or may not be marked %inline) to its definition in the
   transformed grammar, an (expanded) rule. In order to avoid duplication of
   work, we memoize [expand_symbol]. Thus, the expansion of each symbol is
   computed at most once. (Expansions are demanded top-down, but are computed
   bottom-up.) Memoization is implemented without pain by using a ready-made
   fixed point combinator, [Memoize.defensive_fix]. Furthermore, this find
   point combinator dynamically detects cycles of %inline nonterminal symbols,
   allowing us to avoid divergence and display a nice error message. *)

module Inline (G : sig val grammar: grammar end) = struct

  open G

  let is_inline_symbol =
    is_inline_symbol grammar

  let is_inline_producer =
    is_inline_producer grammar

  let find =
    find grammar

  (* In [--coq] mode, %inline is forbidden. There are two reasons for this.
     One technical reason is that inlining requires constructing composite
     semantic actions (using [Action.compose], etc.) and this construction is
     currently OCaml-specific. (This could be rather easily changed, though.)
     A more philosophical reason is that we don't want to have a large gap
     between the grammar written by the user in the .mly file and the grammar
     written by Menhir in the .v file. The latter grammar is the reference
     grammar, the one with respect to which the generated parser is proved
     correct. *)

  let () =
    if Settings.coq then
      StringMap.iter (fun _ rule ->
        if rule.inline_flag then
          Error.error rule.positions
            "%%inline is not supported by the Coq back-end."
      ) grammar.rules

  (* This is [expand_branches], parameterized by its companion function,
     [expand_symbol]. The parameter [i], an integer, is used to perform
     a left-to-right sweep: the precondition of [expand_branches] is that
     there are no inlining sites at indices less than [i] in [branches].
     Thus, we can begin searching at index [i]. (Beginning to search at
     index 0 would work, too, but would cause redundant searches.) *)

  let rec expand_branches expand_symbol i branches : branches =
    (* For each branch [caller] in the list [branches], *)
    branches >>= fun (caller : branch) ->
      (* Search for an inlining site in the branch [caller]. We begin the
         search at position [i], as we know that every inlining site left
         of this position has been dealt with already. *)
      match search_at is_inline_producer i caller.producers with
      | None ->
          (* There is none; we are done. *)
          return caller
      | Some ((i, producer) as site) ->
          (* There is one. This is an occurrence of a nonterminal symbol
             [symbol] that is marked %inline. We look up its (expanded)
             definition (via a recursive call to [expand_symbol]), yielding
             a set of branches, which we inline into the branch [caller].
             Then, we continue looking for inlining sites. *)
          check_no_producer_attributes producer;
          let symbol = producer_symbol producer in
          expand_symbol symbol
          |> get_branches
          |> inline_branches caller site
          |> expand_branches expand_symbol i

  (* This is [expand_symbol], parameterized by itself. *)

  let expand_symbol expand_symbol symbol : rule =
    (* Find the rule that defines this symbol. Then, transform this rule
       by applying [expand_branches] to its branches. The left-to-right
       sweep begins at index 0. *)
    find symbol
    |> transform_branches (expand_branches expand_symbol 0)

  (* Apply [defensive_fix] to obtain a closed function [expand_symbol]. *)

  let expand_symbol : Syntax.symbol -> rule =
    Fix.Memoize.String.defensive_fix expand_symbol

  (* Wrap [expand_symbol] in an exception handler, so that, when a cycle
     of %inline nonterminal symbols is detected, a good error message is
     displayed. *)

  let expand_symbol symbol =
    try
      expand_symbol symbol
    with Fix.Memoize.String.Cycle (symbols, symbol) ->
      let rule = find symbol in
      let b = Buffer.create 128 in
      Printf.bprintf b "there is a cycle of %%inline nonterminal symbols:\n";
      begin match symbols with
      | [] ->
          assert false
      | head :: [] ->
          assert (head = symbol);
          Printf.bprintf b "  %s refers to itself." symbol
      | head :: next :: symbols ->
          assert (head = symbol);
          Printf.bprintf b "  %s refers to %s,\n" head next;
          List.iter (fun symbol ->
            Printf.bprintf b "  which refers to %s,\n" symbol
          ) symbols;
          Printf.bprintf b "  which refers back to %s." symbol
      end;
      Error.error rule.positions "%s" (Buffer.contents b)

  (* The rules of the transformed grammar are obtained by keeping only
     non-%inline symbols and expanding their rules. *)

  let rules =
    grammar.rules
    |> StringMap.filter (fun _ rule -> not rule.inline_flag)
    |> StringMap.mapi (fun symbol _rule -> expand_symbol symbol)

  (* Drop %type declarations that concern %inline symbols. *)

  let keep symbol _rule : bool =
    not (is_inline_symbol symbol)

  let types =
    StringMap.filter keep grammar.types

  (* Drop %on_error_reduce declarations that concern %inline symbols. At the
     same time, display a warning, as this seems strange: these declarations
     are useless. *)

  let keep_or_warn (symbol : string) _rule : bool =
    let keep = keep symbol _rule in
    if not keep then
      Error.grammar_warning []
        "the declaration %%on_error_reduce %s\n\
         has no effect: this symbol is marked %%inline and is expanded away." symbol;
    keep

  let on_error_reduce =
    StringMap.filter keep_or_warn grammar.on_error_reduce

  (* We are done. *)

  let grammar =
    { grammar with rules; types; on_error_reduce }

end

(* -------------------------------------------------------------------------- *)

(* Re-package the above functor as a function. *)

let inline grammar =
  let module I = Inline(struct let grammar = grammar end) in
  I.grammar
