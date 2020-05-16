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

open BasicSyntax
open Keyword
open IL
open CodeBits

(* [posvar_ keyword] constructs the conventional name of the variable
   that stands for the position keyword [keyword]. *)

let posvar_ = function
  | Position (subject, where, flavor) ->
      posvar subject where flavor
  | _ ->
      assert false (* [posvar_] should be applied to a position keyword *)

(* [symbolstartpos producers i n] constructs an expression which, beginning at
   index [i], looks for the first non-empty producer and returns its start
   position. If none is found, this expression returns the end position of the
   right-hand side. This computation is modeled after the function
   [Parsing.symbol_start_pos] in OCaml's standard library. *)

(* This cascade of [if] constructs could be quite big, and this could be a
   problem in terms of code size. Fortunately, we can optimize this code by
   computing, ahead of time, the outcome of certain comparisons. We assume that
   the lexer never produces a token whose start and end positions are the same.
   There follows that a non-nullable symbol cannot have the same start and end
   positions. Conversely, a symbol that generates (a subset of) the language
   {epsilon} must have the same start and end positions. *)

(* Although this code is modeled after [Parsing.symbol_start_pos], we compare
   positions using physical equality, whereas they use structural equality. If
   for some reason a symbol has start and end positions that are structurally
   equal but physically different, then a difference will be observable.
   However, this is very unlikely. It would mean that a token has the same start
   and end positions (and furthermore, this position has been re-allocated). *)

(* The reason why we expand [$symbolstartpos] away prior to inlining is that we
   want its meaning to be preserved by inlining. If we tried to preserve this
   keyword through the inlining phase, then (I suppose) we would have to introduce
   a family of keywords [$symbolstartpos(i, j)], computing over the interval from
   [i] to [j], and the preservation would not be exact -- because a nonempty
   symbol, once inlined, can be seen to be a sequence of empty and nonempty
   symbols. *)

let rec symbolstartpos ((nullable, epsilon) as analysis) producers i n
: IL.expr * KeywordSet.t =
  if i = n then
    (* Return [$endpos]. *)
    let keyword = Position (Left, WhereEnd, FlavorPosition) in
    EVar (posvar_ keyword), KeywordSet.singleton keyword
  else
    (* [symbol] is the symbol that appears in the right-hand side at position i.
       [x] is the identifier that is bound to it. We generate code that compares
       [$startpos($i)] and [$endpos($i)]. If they differ, we return
       [$startpos($i)]. Otherwise, we continue. Furthermore, as noted above, if
       [symbol] is not nullable, then we know that the start and end positions
       must differ, so we optimize this case. *)
    let producer = List.nth producers i in
    let symbol = producer_symbol producer
    and x = producer_identifier producer in
    let startp = Position (RightNamed x, WhereStart, FlavorPosition)
    and   endp = Position (RightNamed x, WhereEnd,   FlavorPosition) in
    if not (nullable symbol) then
      (* The start and end positions must differ. *)
      EVar (posvar_ startp),
      KeywordSet.singleton startp
    else
      let continue, keywords = symbolstartpos analysis producers (i + 1) n in
      if epsilon symbol then
        (* The start and end positions must be the same. *)
        continue,
        keywords
      else
        (* In the general case, a runtime test is required. *)
        EIfThenElse (
          EApp (EVar "(!=)", [ EVar (posvar_ startp); EVar (posvar_ endp) ]),
          EVar (posvar_ startp),
          continue
        ),
        KeywordSet.add startp (KeywordSet.add endp keywords)

(* [define keyword1 f keyword2] macro-expands [keyword1] as [f(keyword2)],
   where [f] is a function of expressions to expressions. *)

let define keyword1 f keyword2 =
  Action.define
    keyword1
    (KeywordSet.singleton keyword2)
    (mlet
       [ PVar (posvar_ keyword1) ]
       [ f (EVar (posvar_ keyword2)) ])

(* A [loc] keyword is expanded away. *)

(* Since a location is represented as a pair of positions, $loc is sugar for
   the pair ($startpos, $endpos). (Similarly for $loc(x).) Furthermore, $sloc
   is sugar for the pair ($symbolstartpos, $endpos). *)

let define_as_tuple keyword keywords =
  Action.define
    keyword
    (List.fold_right KeywordSet.add keywords KeywordSet.empty)
    (mlet
       [ PVar (posvar_ keyword) ]
       [ ETuple (List.map (fun keyword -> EVar (posvar_ keyword)) keywords) ])

let expand_loc keyword action =
  match keyword with
  | Position (Left, WhereSymbolStart, FlavorLocation) -> (* $sloc *)
      define_as_tuple keyword
        [ Position (Left, WhereSymbolStart, FlavorPosition);
          Position (Left, WhereEnd, FlavorPosition) ]
        action
  | Position (subject, WhereStart, FlavorLocation) -> (* $loc, $loc(x) *)
      define_as_tuple keyword
        [ Position (subject, WhereStart, FlavorPosition);
          Position (subject, WhereEnd, FlavorPosition) ]
        action
  | _ ->
      action

(* An [ofs] keyword is expanded away. It is defined in terms of the
   corresponding [pos] keyword. *)

let expand_ofs keyword action =
  match keyword with
  | Position (subject, where, FlavorOffset) ->
      define keyword
        (fun e -> ERecordAccess (e, "Lexing.pos_cnum"))
        (Position (subject, where, FlavorPosition))
        action
  | _ ->
      action

(* [$symbolstartpos] is expanded into a cascade of [if] constructs, modeled
   after [Parsing.symbol_start_pos]. *)

let expand_symbolstartpos analysis producers n keyword action =
  match keyword with
  | Position (Left, WhereSymbolStart, FlavorPosition) ->
      let expansion, keywords = symbolstartpos analysis producers 0 n in
      Action.define keyword keywords
        (mlet [ PVar (posvar_ keyword) ] [ expansion ])
        action
  | Position (RightNamed _, WhereSymbolStart, FlavorPosition) ->
      (* [$symbolstartpos(x)] does not exist. *)
      assert false
  | _ ->
      action

(* [$startpos] and [$endpos] are expanded away.  *)

let expand_startend producers n keyword action =
  match keyword with
  | Position (Left, WhereStart, flavor) ->

      (* [$startpos] is defined as [$startpos($1)] if this production has
         nonzero length and [$endpos($0)] otherwise. *)
      define keyword (fun e -> e) (
        if n > 0 then
          let x = producer_identifier (List.hd producers) in
          Position (RightNamed x, WhereStart, flavor)
        else
          Position (Before, WhereEnd, flavor)
      ) action

  | Position (Left, WhereEnd, flavor) ->

      (* [$endpos] is defined as [$endpos($n)] if this production has
         nonzero length and [$endpos($0)] otherwise. *)
      define keyword (fun e -> e) (
        if n > 0 then
          let x = producer_identifier (List.hd (List.rev producers)) in
          Position (RightNamed x, WhereEnd, flavor)
        else
          Position (Before, WhereEnd, flavor)
      ) action

  | _ ->
      action

(* [expand_round] performs one round of expansion on [action], using [f] as a
   rewriting rule. *)

let expand_round f action =
  KeywordSet.fold f (Action.keywords action) action

(* [expand_action] performs macro-expansion in [action]. We do this in several
   rounds: first, expand the [loc] keywords away; then, expand the [ofs]
   keywords away; then, expand [symbolstart] away; then, expand the rest. We
   do this in this order because each round can cause new keywords to appear,
   which must eliminated by the following rounds. *)

let expand_action analysis producers action =
  let n = List.length producers in

  (* Expand [loc] keywords away first. *)

  let action = expand_round expand_loc action in

  (* The [ofs] keyword family is defined in terms of the [pos] family by
     accessing the [pos_cnum] field. Expand these keywords away first. *)

  let action = expand_round expand_ofs action in

  (* Expand [$symbolstartpos] away. *)

  let action = expand_round (expand_symbolstartpos analysis producers n) action in

  (* Then, expand away the non-[ofs] keywords. *)

  let action = expand_round (expand_startend producers n) action in

  action

(* Silently analyze the grammar so as to find out which symbols are
   nullable and which symbols generate a subset of {epsilon}. This
   is used to optimize the expansion of $symbolstartpos. *)

let analysis grammar =
  let module G = GrammarFunctor.Make(struct
    let grammar = grammar
    let verbose = false
  end)() in
  let lookup (nt : Syntax.symbol) : G.Symbol.t =
    try G.Symbol.lookup nt with Not_found -> assert false
  in
  let nullable nt : bool =
    G.Analysis.nullable_symbol (lookup nt)
  and epsilon nt : bool =
    G.TerminalSet.is_empty (G.Analysis.first_symbol (lookup nt))
  in
  nullable, epsilon

(* Put everything together. *)

let expand_branch analysis branch =
  { branch with action = expand_action analysis branch.producers branch.action }

let expand_rule analysis rule =
  { rule with branches = List.map (expand_branch analysis) rule.branches }

let expand_grammar grammar =
  let analysis = analysis grammar in
  { grammar with rules = StringMap.map (expand_rule analysis) grammar.rules }
