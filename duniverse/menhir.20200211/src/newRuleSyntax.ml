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

open Syntax

(* Because the main function, [NewRuleSyntax.rule], is called by the stage 2
   parser (fancy-parser) and nowhere else, this file is type-checked and
   compiled only at stage 2, not at stage 1. Be sure to run [make bootstrap]. *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Function composition. *)

let (>>) f g x =
  g (f x)

(* -------------------------------------------------------------------------- *)

(* Constructors for the new rule syntax. *)

(* [return pos x] is the semantic action [{x}]. *)

let return pos x : seq_expression =
  let action = Action.from_il_expr (IL.EVar x) in
  let raw_action _ _ = action in
  Positions.with_pos pos (EAction (XATraditional raw_action, None))

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Converting the new syntax to the old syntax. *)

(* The new syntax is organized in several levels: choice expressions, sequence
   expressions, symbol expressions, action expressions. The code below reflects
   this organization. *)

(* -------------------------------------------------------------------------- *)

(* When a [~] pattern appears at the top level in [~ = e1; e2] and furthermore
   the expression [e1] is a symbol [x1], then this is considered a pun -- that
   is, [~] is sugar for [x1]. We resolve these puns in a first pass, before we
   check that patterns are linear, so a linearity violation that involves [~]
   will be correctly caught. *)

(* There can still remain [~] patterns after puns are resolved, but they stand
   for fresh variables and cannot cause a linearity violation. *)

let rec resolve_puns (e : seq_expression) : seq_expression =
  Positions.map (fun e ->
    match e with
    | ECons (SemPatTilde pos, (ESymbol (x1, [], _) as e1), e2)
      when ParserAux.valid_ocaml_identifier x1 ->
        (* This is a pun. Resolve it. *)
        let x1 = Positions.with_pos pos (Positions.value x1) in (* optional *)
        ECons (SemPatVar x1, e1, resolve_puns e2)
    | ECons (p1, e1, e2) ->
        ECons (p1, e1, resolve_puns e2)
    | ESingleton _
    | EAction _ ->
        e
  ) e

(* -------------------------------------------------------------------------- *)

(* Checking that a new-syntax pattern is linear,
   i.e., that no variable is bound twice. *)

(* We first build a mapping of variables to the places where they are bound,
   then check that every list in the image of this mapping is a singleton
   list. *)

let check_linearity (ps : pattern list) =

  let rec build (m : Positions.positions StringMap.t) (p : pattern) =
    match p with
    | SemPatVar x ->
        let x, pos = Positions.decompose x in
        StringMap.multiple_add x pos m
    | SemPatWildcard
    | SemPatTilde _ ->
        m
    | SemPatTuple ps ->
        List.fold_left build m ps
  in

  let m = List.fold_left build StringMap.empty ps in
  StringMap.iter (fun x positions ->
    if List.length positions > 1 then
      Error.error positions "The variable %s is bound several times." x
  ) m

let rec patterns (e : seq_expression) : pattern list =
  let e = Positions.value e in
  match e with
  | ECons (p, _, e) ->
      p :: patterns e
  | ESingleton _
  | EAction _ ->
      []

let check_linearity : seq_expression -> unit =
  patterns >> check_linearity

(* -------------------------------------------------------------------------- *)

(* Determining whether a pattern contains a [~] subpattern. *)

let rec tilde_used positions (p : pattern) =
  match p with
  | SemPatVar _
  | SemPatWildcard ->
      positions
  | SemPatTilde pos ->
      pos :: positions
  | SemPatTuple ps ->
      List.fold_left tilde_used positions ps

(* Emitting a warning when a [~] subpattern has been used but the sequence
   expression ends in something other than a point-free semantic action. *)

let tilde_used_warning positions =
  let n = List.length positions in
  if n > 0 then
    let variables_have, tpatterns, wpatterns =
      if n = 1 then "variable has", "a ~ pattern", "a wildcard pattern _"
      else "variables have", "~ patterns", "wildcard patterns _"
    in
    Error.warning positions
      "%s nameless %s been introduced by %s,\n\
       yet this sequence does not end in a point-free semantic action <...>.\n\
       Perhaps %s should be used instead."
      (Misc.count n) variables_have tpatterns wpatterns

(* -------------------------------------------------------------------------- *)

(* Converting a new-syntax pattern to an IL pattern. *)

(* Here, [x1] is the variable that holds the semantic value; it is typically
   named [_1], [_2], etc. When we encounter a [~] pattern, we convert it to a
   fresh name, using [x1] as a basis in the generation of this fresh name. *)

let pattern (x1 : identifier) (p : pattern) : IL.pattern =

  let c = ref 1 in
  let fresh () = Printf.sprintf "%s_%d" x1 (Misc.postincrement c) in

  let rec pattern p =
    match p with
    | SemPatVar x ->
        IL.PVar (Positions.value x)
    | SemPatWildcard ->
        IL.PWildcard
    | SemPatTilde _ ->
        IL.PVar (fresh())
    | SemPatTuple [] ->
        IL.PUnit
    | SemPatTuple [p] ->
        pattern p
    | SemPatTuple ps ->
        IL.PTuple (List.map pattern ps)

  in
  pattern p

(* [bv accu p] accumulates the bound variables of a pattern [p] produced by
   the above function. The ordering is significant; variables must be
   accumulated left to right (so we get a reversed list). *)

let rec bv accu p =
  match p with
  | IL.PVar x ->
      x :: accu
  | IL.PWildcard ->
      accu
  | IL.PUnit ->
      accu
  | IL.PTuple ps ->
      List.fold_left bv accu ps
  | _ ->
      (* Impossible; not produced above. *)
      assert false

(* -------------------------------------------------------------------------- *)

(* Extracting the attributes of a symbol expression. *)

let attributes (e : symbol_expression) : attributes =
  match e with
  | ESymbol (_, _, attrs) ->
      attrs

(* -------------------------------------------------------------------------- *)

(* As we descend into a sequence expression and prepare to build a production,
   we maintain a context of the following form. *)

type context = {

  (* The position of the complete sequence expression. *)
  pos: Positions.t;

  (* The prefix of the production's right-hand side that has been built so far.
     This is reversed list of producers. Every producer carries an identifier,
     which is either user-supplied or auto-generated. *)
  producers: producer list;

  (* The user-supplied names under which the semantic values are known. Not
     every semantic value has such a name, as the user can supply no pattern,
     a wildcard pattern, or a tuple pattern; in either of these cases, there
     is no name for the semantic value. This is a reversed list. Its length
     is equal to the length of the list [producers] above. *)
  uxs: identifier option list;

  (* A set of (independent) bindings that must be wrapped around the semantic
     action. These are typically bindings of the form [let p = x in ...]. *)
  bindings: action -> action;

  (* A tuple of variables that the user has bound, either explicitly or via
     the [~] notation. This is a reversed list. Its length is unrelated to
     the length of the above lists, because one semantic value can be matched
     against a pattern that binds zero, one, or more variables. Once complete,
     this tuple becomes the argument to a point-free semantic action. *)
  tuple: identifier list;

  (* A list of positions indicating where [~] patterns appear. This flag is
     maintained as we descend into a [seq_expression] whose puns have been
     resolved already. Thus, when this list is nonempty, we expect that this
     [seq_expression] ends with a point-free semantic action; otherwise, there
     is no point in using [~], and the user could have used [_] instead. We
     issue a warning if the [seq_expression] does not end with a point-free
     semantic action. *)
  tilde_used: Positions.positions;

}

(* The empty context. *)

let empty pos : context =
  {
    pos;
    producers  = [];
    uxs        = [];
    bindings   = (fun a -> a);
    tuple      = [];
    tilde_used = [];
  }

(* Recording a user-supplied identifier. *)

let user (x : identifier located) : identifier option =
  Some (Positions.value x)

let auto : identifier option =
  None

(* Accessing the producers. *)

let producers context : producer list =
  List.rev context.producers

(* Accessing the user-supplied identifiers. *)

let uxs context : identifier option array =
  Array.of_list (List.rev context.uxs)

(* Accessing the tuple. *)

let tuple context : identifier list =
  List.rev context.tuple

(* -------------------------------------------------------------------------- *)

(* OCaml variables for semantic values. *)

(* We do not use a fresh name generator. Instead, we use our [context]
   to generate names of the form [_1], [_2], etc., corresponding to the
   current index in the production that we are building. *)

let semvar context : identifier =
  let i = List.length context.producers + 1 in
  Printf.sprintf "_%d" i

(* -------------------------------------------------------------------------- *)

(* Converting a symbol expression to a parameter. *)

let rec parameter (e : symbol_expression) : parameter =
  match e with
  | ESymbol (sym, args, _attrs) ->
      (* Converting a symbol expression is easy. Note, however, that the
         arguments [args] are arbitrary expressions. *)
      Parameters.app sym (List.map nested_parameter args)

(* Converting an arbitrary expression to a parameter. *)

and nested_parameter (e : expression) : parameter =
  match Positions.value e with
  | EChoice [ Branch ({ Positions.value = ESingleton e }, _) ] ->
     (* A choice with only one branch, whose body is a trivial sequence
        consisting of just a symbol, is simplified on the fly. This is
        important, as it allows us to avoid falling into the default case
        below, where an anonymous rule is generated. E.g., when we have an
        application [f(x)] of a parameterized symbol [f] to a symbol [x], we
        don't want an anonymous rule to be generated for [x]. That would be
        wasteful and (more seriously) could cause the
        grammar-expansion-termination check to fail. *)
      parameter e
  | EChoice _ ->
      (* A choice expression is converted to an anonymous rule. *)
      let pos = Positions.position e in
      ParameterAnonymous (Positions.with_pos pos (productions e))

(* Converting the head of a sequence, a pair [x = e1] of a variable [x] and
   a symbol expression [e1], to a producer. *)

and producer x (e1 : symbol_expression) : producer =
  x, parameter e1, attributes e1

(* Converting the head of a sequence, a pair [p = e1] of a pattern [p] and
   a symbol expression [e1], to a context extension. *)

and extend (p : pattern) (e1 : symbol_expression) (context : context) : context =
  match p with
  | SemPatVar x1 ->
      (* The variable [x1] is bound to the semantic value of [e1]. *)
      (* Puns have been resolved already, so they are handled by this code. *)
      { pos        =                       context.pos;
        producers  = producer x1 e1     :: context.producers;
        uxs        = user x1            :: context.uxs;
        bindings   =                       context.bindings;
        tuple      = Positions.value x1 :: context.tuple;
        tilde_used = context.tilde_used }
  | _ ->
      (* An arbitrary pattern [p] is used. We bind a variable [x1] to the
         semantic value of [e1], and wrap the semantic action in a binding
         [let p = x1 in ...]. Any [~] subpatterns within [p] are translated to
         fresh identifiers. *)
      let x1 = semvar context in
      let tilde_used = tilde_used context.tilde_used p in
      let p : IL.pattern = pattern x1 p in
      let binding = Action.bind p x1 in
      let x1 = Positions.unknown_pos x1 in
      { pos        =                   context.pos;
        producers  = producer x1 e1 :: context.producers;
        uxs        = auto           :: context.uxs;
        bindings   = binding        >> context.bindings;
        tuple      = bv context.tuple p;
        tilde_used }

(* Converting a sequence expression to a production. *)

and production_aux
  (context : context)
  (e : seq_expression)
  (level : branch_production_level)
: parameterized_branch =
  let e, pos = Positions.decompose e in
  match e with

  | ECons (p, e1, e2) ->
      (* A sequence expression [p = e1; e2]. Based on [p] and [e1], extend the
         context, then continue with [e2]. *)
      production_aux (extend p e1 context) e2 level

  | EAction (XATraditional raw_action, prec) ->
      (* An action expression. This is the end of the sequence. *)
      tilde_used_warning context.tilde_used;
      (* Check that the semantic action seems well-formed. *)
      let action = raw_action Settings.DollarsDisallowed (uxs context) in
      (* Build and return a complete production. *)
      {
        pr_branch_position         = context.pos;
        pr_producers               = producers context;
        pr_action                  = context.bindings action;
        pr_branch_prec_annotation  = prec;
        pr_branch_production_level = level;
      }

  | EAction (XAPointFree oid, prec) ->
      (* A point-free semantic action, containing an OCaml identifier [id]
         between angle brackets. This is syntactic sugar for a traditional
         semantic action containing an application of [id] to a tuple of the
         semantic values that have been assigned a name by the user. *)

      (* As a special case, if [oid] is [None], then we must not build
         an application node -- we simply build a tuple. *)

     (* [id] is actually a stretch, not just a string, and this matters when
        there is an OCaml error (e.g., [id] is undeclared, or ill-typed).
        The stretch contains source code location information which allows
        the error to be reported in the source file. *)

      (* Build the tuple as an IL expression. *)
      let evar x = IL.EVar x in
      let evars xs = List.map evar xs in
      let tuple = CodeBits.etuple (evars (tuple context)) in
      (* Build an application of [id] to this tuple. *)
      (* We abuse the abstract syntax of IL and build an application node,
         regardless of whether [id] a (possibly qualified) value, a (possibly
         qualified) data constructor, a polymorphic variant constructor, etc. *)
      let e =
        match oid with
        | Some id ->
            IL.EApp (IL.ETextual id, [tuple])
        | None ->
            tuple
      in
      (* Build a traditional semantic action. *)
      let action = Action.from_il_expr e in
      let raw_action _ _ = action in
      let e = EAction (XATraditional raw_action, prec) in
      let e = Positions.with_pos pos e in
      (* Reset [tilde_used], to avoid triggering the warning
         via our recursive call. *)
      let context = { context with tilde_used = [] } in
      (* Done. *)
      production_aux context e level

  | ESingleton e ->
      tilde_used_warning context.tilde_used;
      (* When a symbol expression [e] appears as the last element of a sequence,
         this is understood as syntactic sugar for [x = e; {x}], where [x] is a
         fresh variable. *)
      (* Another option would be to view it as sugar for [~ = e; <>]. That would
         also make sense, but would depart from the lambda-calculus convention
         that in a sequence [e1; e2; e3] only the value of the last expression
         is returned. *)
      (* No %prec annotation can be supplied when this sugar is used. *)
      let x = semvar context in
      let e = ECons (SemPatVar (Positions.with_pos pos x), e, return pos x) in
      let e = Positions.with_pos pos e in
      production_aux context e level

and production (Branch (e, level) : branch) =
  let e = resolve_puns e in
  check_linearity e;
  let pos = Positions.position e in
  production_aux (empty pos) e level

and productions (e : expression) : parameterized_branch list =
  match Positions.value e with
  | EChoice branches ->
      List.map production branches

(* -------------------------------------------------------------------------- *)

(* Converting a new rule to an old rule. *)

let rule (rule : rule) : parameterized_rule =
  {
    pr_public_flag = rule.rule_public;
    pr_inline_flag = rule.rule_inline;
    pr_nt          = Positions.value rule.rule_lhs;
    pr_positions   = [ Positions.position rule.rule_lhs ];
    pr_attributes  = rule.rule_attributes;
    pr_parameters  = List.map Positions.value rule.rule_formals;
    pr_branches    = productions rule.rule_rhs
  }
