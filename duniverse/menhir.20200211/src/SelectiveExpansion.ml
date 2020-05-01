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

let value    = Positions.value
let unknown  = Positions.unknown_pos
open Syntax
open GroundSort

(* -------------------------------------------------------------------------- *)

(* Expansion modes. *)

type mode =
  | ExpandHigherSort
  | ExpandAll

(* -------------------------------------------------------------------------- *)

(* Expansion can be understood as traversing a graph where every vertex is
   labeled with a pair of a nonterminal symbol [nt] and an instantiation of
   the formal parameters of [nt]. *)

(* We allow partial instantiations, where some of the formal parameters of
   [nt] are instantiated, while others remain present. For this reason, we
   represent an instantation as a list of *optional* actual parameters. *)

(* The actual parameters that appear in an instantiation make sense *in the
   source namespace* (at the toplevel). That is, they refer to (terminal and
   nonterminal) symbols that exist (at the toplevel) in the original
   grammar. *)

type instantiation =
  parameter option list

type label =
  nonterminal * instantiation

(* Equality and hashing for labels. *)

module Label = struct
  type t = label
  let equal (nt1, inst1) (nt2, inst2) =
    nt1 = nt2 &&
    List.for_all2 (Option.equal Parameters.equal) inst1 inst2
  let hash (nt, inst) =
    Hashtbl.hash (nt, Misc.ListExtras.hash (Option.hash Parameters.hash) inst)
end

(* -------------------------------------------------------------------------- *)

(* [mangle label] chooses a concrete name for the new nonterminal symbol that
   corresponds to the label [label]. *)

(* We include parentheses and commas in this name, because that is readable
   and acceptable in many situations. We replace them with underscores in
   situations where these characters are not valid; see [Misc.normalize]. *)

let mangle_po (po : parameter option) =
  match po with
  | None ->
      (* When a parameter remains uninstantiated,
         we put an underscore in its place. *)
      "_"
  | Some p ->
      Parameters.print false p

let mangle ((nt, pos) : label) : nonterminal =
  if pos = [] then nt else
    Printf.sprintf "%s(%s)"
      nt
      (Misc.separated_list_to_string mangle_po "," pos)

(* -------------------------------------------------------------------------- *)

(* An environment maps all of the formal parameters of a rule to actual
   parameters, which make sense in the source namespace. *)

module Env =
  StringMap

type env =
  parameter Env.t

let subst_symbol env sym : parameter =
  try
    Env.find (value sym) env
  with Not_found ->
    (* [x] is not a formal parameter. It is a toplevel symbol. *)
    ParameterVar sym

let apply (param : parameter) (params : parameter list) : parameter =
  match param with
  | ParameterVar sym ->
      assert (params <> []);
      ParameterApp (sym, params)
  | ParameterApp _ ->
      (* In a well-sorted grammar, only a variable can have higher
         sort. Here, [param] must have higher sort, so [param] must
         be a variable. This case cannot arise. *)
      assert false
  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false

let rec subst_parameter env param : parameter =
  match param with
  | ParameterVar sym ->
      subst_symbol env sym
  | ParameterApp (sym, params) ->
      assert (params <> []);
      apply (subst_symbol env sym) (subst_parameters env params)
  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false

and subst_parameters env params =
  List.map (subst_parameter env) params

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* For syntactic convenience, the rest of this file is a functor. *)

module Run (G : sig
  (* Expansion mode. *)
  val mode: mode
  (* Sort information. *)
  val sorts: SortInference.sorts
  (* The grammar [g] whose expansion is desired. *)
  val g : grammar
end) = struct

open G

(* -------------------------------------------------------------------------- *)

(* Determining the sort of a symbol or parameter. *)

(* Be careful: these functions use the toplevel sort environment [sorts],
   so they must not be used within a rule. (The sort environment would have
   to be extended with information about the formal parameters.) *)

let sort symbol =
  try
    StringMap.find (value symbol) sorts
  with Not_found ->
    assert false

let sort param =
  match param with
  | ParameterVar sym ->
      sort sym
  | ParameterApp (_, params) ->
      assert (params <> []);
      (* An application always has sort [*]. *)
      star
  | ParameterAnonymous _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* Looking up the [%attribute] declarations, looking for attributes attached
   with a nonterminal symbol [nt]. This is used when we create a specialized
   version of this symbol. *)

(* We use an inefficient linear search, but that shouldn't be a problem. *)

let global_attributes (nt : symbol) : attribute list =
  let param = ParameterVar (unknown nt) in
  List.concat (List.map (fun (params, attrs) ->
    if List.exists (Parameters.equal param) params then attrs else []
  ) g.p_symbol_attributes)

(* -------------------------------------------------------------------------- *)

(* A queue keeps track of the graph vertices that have been discovered but
   not yet visited. *)

let enqueue, repeatedly =
  let queue = Queue.create() in
  let enqueue label =
    Queue.add label queue
  and repeatedly visit =
    Misc.qiter visit queue
  in
  enqueue, repeatedly

(* -------------------------------------------------------------------------- *)

(* A hash table is used to mark the graph vertices that have been discovered. *)

let mark, marked =
  let module H = Hashtbl.Make(Label) in
  let table = H.create 128 in
  let mark label =
    H.add table label ()
  and marked label =
    H.mem table label
  in
  mark, marked

(* -------------------------------------------------------------------------- *)

(* The rules of the expanded grammar are gradually collected. *)

let emit, rules =
  let rules = ref StringMap.empty in
  let emit rule =
    assert (not (StringMap.mem rule.pr_nt !rules));
    rules := StringMap.add rule.pr_nt rule !rules
  and rules() =
    !rules
  in
  emit, rules

(* -------------------------------------------------------------------------- *)

(* On top of the function [mangle], we set up a mechanism that checks that
   every (normalized) mangled name is unique. (Indeed, in principle, there
   could be clashes, although in practice this is unlikely.) We must check
   that every application of [mangle] to a *new* argument yields a *new*
   (normalized) result. This is succinctly expressed by combining a claim
   and a memoizer. *)

let mangle : label -> nonterminal =
  let ensure_fresh = Misc.new_claim() in
  let module M = Fix.Memoize.ForHashedType(Label) in
  M.memoize (fun label ->
    let name = mangle label in
    ensure_fresh (Misc.normalize name);
    name
  )

(* -------------------------------------------------------------------------- *)

(* [recognize] receives an actual parameter [param] that makes sense in the
   source namespace and transforms it into a parameter that makes sense in the
   target namespace. This involves examining each application and
   "recognizing" it as an application of a label to a sequence of residual
   actual parameters, as explained next. All labels thus recognized are
   enqueued. *)

(* [recognize] governs how much specialization is performed. For instance,
   [F(X, Y, Z)] could be recognized as:

   - an application of the symbol [F] to the residual arguments [X, Y, Z].
     Then, no specialization at all takes place.

   - an application of the symbol [F(X,Y,Z)] to no residual arguments.
     Then, [F] is fully specialized for [X, Y, Z].

   - in between these extremes, say,
     an application of the symbol [F(X,_,Z)] to the residual argument [Y].
     Then, [F] is partially specialized.

   If there are any residual arguments, then they must be recursively
   recognized. For instance, [F(X,G(Y),Z)] could be recognized as an
   application of the symbol [F(X,_,Z)] to [G(Y)], which itself could
   be recognized as an application of the symbol [G(Y)] to no residual
   arguments. *)

let rec recognize (param : parameter) : parameter =
  (* [param] must have sort [star], in an appropriate sort environment. *)
  match param with
  | ParameterAnonymous _ ->
      assert false
  | ParameterVar _ ->
      param
  | ParameterApp (sym, ps) ->
      assert (ps <> []);
      let x = value sym in
      (* This symbol is applied to at least one argument, so cannot be
         a terminal symbol. It must be either a nonterminal symbol or
         an (uninstantiated) formal parameter of the current rule. *)
      (* Actually, in both modes, formal parameters of higher sort are
         expanded away, so [sym] cannot be an uninstantiated parameter
         of the current rule. It must be a nonterminal symbol. We can
         therefore look up its sort in the toplevel environment [sorts]. *)
      let inst, residuals =
        match mode with
        | ExpandAll ->
            (* Expansion of all parameters. *)
            let inst = List.map (fun p -> Some p) ps
            and residuals = [] in
            inst, residuals
        | ExpandHigherSort ->
            (* Expansion of only the parameters of higher sort. *)
            let ss : sort list = domain (sort (ParameterVar sym)) in
            assert (List.length ps = List.length ss);
            let pss = List.combine ps ss in
            let inst =
              pss
              |> List.map (fun (param, sort) ->
                   if sort = star then None else Some param)
            in
            let residuals =
              pss
              |> List.filter (fun (_, sort) -> sort = star)
              |> List.map (fun (param, _) -> recognize param)
            in
            inst, residuals
      in
      let label = (x, inst) in
      enqueue label;
      let sym = mangle label in
      Parameters.app (unknown sym) residuals

(* -------------------------------------------------------------------------- *)

(* The following functions take arguments in the source namespace and produce
   results in the target namespace. *)

let subst_parameter env param =
  (* [param] must have sort [star], in an appropriate sort environment. *)
  recognize (subst_parameter env param)

let subst_producer env (id, param, attrs) =
  let param = subst_parameter env param in
  (id, param, attrs)

let subst_producers env producers =
  List.map (subst_producer env) producers

let subst_branch env branch =
  { branch with pr_producers = subst_producers env branch.pr_producers }

let subst_branches env branches =
  List.map (subst_branch env) branches

(* -------------------------------------------------------------------------- *)

(* A quick and dirty way of mapping a name to a fresh name. *)

let freshen : string -> string =
  let c = ref 0 in
  fun x ->
    Printf.sprintf "%s__menhir__%d" x (Misc.postincrement c)

(* -------------------------------------------------------------------------- *)

(* [instantiation_env] expects the formal parameters of a rule, [formals], and
   an instantiation [inst] that dictates how this rule must be specialized. It
   returns an environment [env] that can be used to perform specialization and
   a list of residual formal parameters (those that are not specialized). *)

let instantiation_env formals inst : env * symbol list =
  assert (List.length formals = List.length inst);
  let env, residuals =
    List.fold_right2 (fun formal po (env, residuals) ->
      let param, residuals =
        match po with
        | Some param ->
            (* This formal parameter is instantiated. *)
            param, residuals
        | None ->
            (* This formal parameter is not instantiated. *)
            (* We would like to map it to itself. *)
            (* However, we must in principle be a bit careful: if a toplevel
               symbol by the same name as [formal] appears free in the codomain
               of the environment that we are building, then we will run intro
               trouble. We avoid this problem by systematically renaming every
               formal parameter to a fresh unlikely name. *)
            let formal = freshen formal in
            ParameterVar (unknown formal),
            formal :: residuals
      in
      Env.add formal param env, residuals
    ) formals inst (Env.empty, [])
  in
  env, residuals

(* -------------------------------------------------------------------------- *)

(* [visit label] visits a vertex labeled [label] in the graph. This label is a
   pair of a nonterminal symbol [nt] and an instantiation [inst]. Unless this
   vertex has been visited already, we create a specialized copy of [nt] for
   this instantiation. This involves a call to [subst_branches], which can
   cause more vertices to be discovered and enqueued. *)

(* The specialized symbol retains any attributes carried by the original
   parameterized symbol. These attributes could be either attached with this
   rule ([rule.pr_attributes]) or specified via an [%attribute] declaration.
   We have to look up [%attribute] declarations now (as opposed to letting
   [Drop] handle them) if this is a parameterized symbol, as the connection
   between the original parameterized symbol and its specialized version is
   evident here but is lost afterwards. *)

let visit label =
  if not (marked label) then begin
    mark label;
    let (nt, inst) = label in
    let rule = StringMap.find nt g.p_rules in
    let formals = rule.pr_parameters in
    let env, residuals = instantiation_env formals inst in
    emit {
      rule with
      pr_nt = mangle label;
      pr_parameters = residuals;
      pr_branches = subst_branches env rule.pr_branches;
      pr_attributes =
        (if formals = [] then [] else global_attributes nt) @
        rule.pr_attributes
    }
  end

(* -------------------------------------------------------------------------- *)

(* The entry points of the graph traversal include the nonterminal symbols of
   sort [*]. (Not just the start symbols, as we haven't run the reachability
   analysis, and the grammar may contain unreachable parts, which we still
   want to expand.) Because a start symbol must have sort [*], this includes
   the start symbols. *)

let () =
  StringMap.iter (fun nt prule ->
    if prule.pr_parameters = [] then
      let label = (nt, []) in
      enqueue label
  ) g.p_rules

(* -------------------------------------------------------------------------- *)

(* The parameters that appear in [%type] declarations and [%on_error_reduce]
   declarations are also considered entry points. They have sort [*]. *)

let subst_parameter param =
  subst_parameter Env.empty param

let subst_declaration (param, info) =
  assert (sort param = star);
  (subst_parameter param, info)

let subst_declarations decls =
  List.map subst_declaration decls

(* -------------------------------------------------------------------------- *)

(* An [%attribute] declaration for a parameter of sort [*] is treated as an
   entry point. An [%attribute] declaration for a symbol of higher sort is not
   regarded as an entry point, and at the end, is kept only if this symbol
   still appears in the expanded grammar. *)

(* This is done in two passes over the list of [%attribute] declarations,
   named [thingify] and [unthingify]. The first pass runs as part of the
   discovery of entry points, before the graph traversal. The second pass runs
   after the graph traversal is complete. *)

type thing =
  | TargetParameterOfSortStar of parameter
  | SourceParameterOfHigherSort of parameter

let thingify_parameter param : thing =
  if sort param = star then
    TargetParameterOfSortStar (subst_parameter param)
  else
    SourceParameterOfHigherSort param

let thingify_attribute_declaration (params, attrs) =
  (List.map thingify_parameter params, attrs)

let thingify_attribute_declarations decls =
  List.map thingify_attribute_declaration decls

let unthingify_parameter rules thing =
  match thing with
  | TargetParameterOfSortStar param ->
      (* This parameter has sort [star]. Keep it. *)
     Some param
  | SourceParameterOfHigherSort param ->
      (* This parameter has higher sort. It must be a symbol.
         Keep it if it still appears in the expanded grammar. *)
     let symbol = value (Parameters.unvar param) in
     if StringMap.mem symbol rules then Some param else None

let unthingify_attribute_declaration rules (params, attrs) =
  (Misc.map_opt (unthingify_parameter rules) params, attrs)

let unthingify_attribute_declarations rules decls =
  List.map (unthingify_attribute_declaration rules) decls

(* -------------------------------------------------------------------------- *)

(* Put everything together a construct a new grammar. *)

let g =
  (* Discovery of entry points. *)
  let p_types = subst_declarations g.p_types
  and p_on_error_reduce = subst_declarations g.p_on_error_reduce
  and things = thingify_attribute_declarations g.p_symbol_attributes in
  (* Graph traversal. *)
  repeatedly visit;
  (* Construction of the new grammar. *)
  let p_rules = rules() in
  let p_symbol_attributes = unthingify_attribute_declarations p_rules things in
  { g with
    p_types;
    p_on_error_reduce;
    p_symbol_attributes;
    p_rules }

end (* of the functor *)

(* -------------------------------------------------------------------------- *)

(* Re-package the above functor as a function. *)

let expand mode sorts g =
  let module G = Run(struct
    let mode = mode
    let sorts = sorts
    let g = g
  end) in
  G.g
