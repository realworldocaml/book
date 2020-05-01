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
open Grammar
open Cmly_format

let raw_content stretch =
  stretch.Stretch.stretch_raw_content

let ocamltype (typ : Stretch.ocamltype) : ocamltype =
  match typ with
  | Stretch.Declared stretch ->
      raw_content stretch
  | Stretch.Inferred typ ->
      typ

let ocamltype (typo : Stretch.ocamltype option) : ocamltype option =
  match typo with
  | None ->
      None
  | Some typ ->
      Some (ocamltype typ)

let range (pos : Positions.t) : range =
  {
    r_start = Positions.start_of_position pos;
    r_end   = Positions.end_of_position pos;
  }

let ranges =
  List.map range

let attribute (label, payload : Syntax.attribute) : attribute =
  {
    a_label    = Positions.value label;
    a_payload  = raw_content payload;
    a_position = range (Positions.position label);
  }

let attributes : Syntax.attributes -> attributes =
  List.map attribute

let terminal (t : Terminal.t) : terminal_def =
  {
    t_kind = (
      if Terminal.equal t Terminal.error then
        `ERROR
      else if
        (match Terminal.eof with
         | None -> false
         | Some eof -> Terminal.equal t eof) then
        `EOF
      else if Terminal.pseudo t then
        `PSEUDO
      else
        `REGULAR
    );
    t_name = Terminal.print t;
    t_type = ocamltype (Terminal.ocamltype t);
    t_attributes = attributes (Terminal.attributes t);
  }

let nonterminal (nt : Nonterminal.t) : nonterminal_def =
  let is_start = Nonterminal.is_start nt in
  {
    n_kind = if is_start then `START else `REGULAR;
    n_name = Nonterminal.print false nt;
    n_mangled_name = Nonterminal.print true nt;
    n_type = if is_start then None else ocamltype (Nonterminal.ocamltype nt);
    n_positions = if is_start then [] else ranges (Nonterminal.positions nt);
    n_nullable = Analysis.nullable nt;
    n_first = List.map Terminal.t2i (TerminalSet.elements (Analysis.first nt));
    n_attributes = if is_start then [] else attributes (Nonterminal.attributes nt);
  }

let symbol (sym : Symbol.t) : symbol =
  match sym with
  | Symbol.N n -> N (Nonterminal.n2i n)
  | Symbol.T t -> T (Terminal.t2i t)

let action (a : Action.t) : action =
  {
    a_expr = Printer.string_of_expr (Action.to_il_expr a);
    a_keywords = Keyword.KeywordSet.elements (Action.keywords a);
  }

let rhs (prod : Production.index) : producer_def array =
  match Production.classify prod with
  | Some n ->
      [| (N (Nonterminal.n2i n), "", []) |]
  | None ->
      Array.mapi (fun i sym ->
        let id = (Production.identifiers prod).(i) in
        let attrs = attributes (Production.rhs_attributes prod).(i) in
        symbol sym, id, attrs
      ) (Production.rhs prod)

let production (prod : Production.index) : production_def =
  {
    p_kind = if Production.is_start prod then `START else `REGULAR;
    p_lhs = Nonterminal.n2i (Production.nt prod);
    p_rhs = rhs prod;
    p_positions = ranges (Production.positions prod);
    p_action = if Production.is_start prod then None
               else Some (action (Production.action prod));
    p_attributes = attributes (Production.lhs_attributes prod);
  }

let item (i : Item.t) : production * int =
  let p, i = Item.export i in
  (Production.p2i p, i)

let itemset (is : Item.Set.t) : (production * int) list =
  List.map item (Item.Set.elements is)

let lr0_state (node : Lr0.node) : lr0_state_def =
  {
    lr0_incoming = Option.map symbol (Lr0.incoming_symbol node);
    lr0_items = itemset (Lr0.items node)
  }

let transition (sym, node) : symbol * lr1 =
  (symbol sym, Lr1.number node)

let lr1_state (node : Lr1.node) : lr1_state_def =
  {
    lr1_lr0 = Lr0.core (Lr1.state node);
    lr1_transitions =
      List.map transition (SymbolMap.bindings (Lr1.transitions node));
    lr1_reductions =
      let add t ps rs = (Terminal.t2i t, List.map Production.p2i ps) :: rs in
      TerminalMap.fold_rev add (Lr1.reductions node) []
  }

let entry_point prod node nt _typ accu : (nonterminal * production * lr1) list =
  (Nonterminal.n2i nt, Production.p2i prod, Lr1.number node) :: accu

let encode () : grammar =
  {
    g_basename     = Settings.base;
    g_preludes     = List.map raw_content Front.grammar.preludes;
    g_postludes    = List.map raw_content Front.grammar.postludes;
    g_terminals    = Terminal.init terminal;
    g_nonterminals = Nonterminal.init nonterminal;
    g_productions  = Production.init production;
    g_lr0_states   = Array.init Lr0.n lr0_state;
    g_lr1_states   = Array.of_list (Lr1.map lr1_state);
    g_entry_points = Lr1.fold_entry entry_point [];
    g_attributes   = attributes Analysis.attributes;
    g_parameters   = List.map raw_content Front.grammar.parameters;
  }

let write oc t =
  (* .cmly file format: CMLY ++ version string ++ grammar *)
  let magic = "CMLY" ^ Version.version in
  output_string oc magic;
  output_value oc (t : grammar)

let write filename =
  (* Opening in binary mode is required. This is not a text file;
     we write to it using [output_value]. *)
  let oc = open_out_bin filename in
  write oc (encode());
  close_out oc
