/*********************************************************************************/
/*                OCaml-RDF                                                      */
/*                                                                               */
/*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     */
/*    et en Automatique. All rights reserved.                                    */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU Lesser General Public License version        */
/*    3 as published by the Free Software Foundation.                            */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU General Public License for more details.                               */
/*                                                                               */
/*    You should have received a copy of the GNU General Public License          */
/*    along with this program; if not, write to the Free Software                */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*********************************************************************************/

(** *)

%{
open Rdf_sparql_types;;

let mk_loc start stop =
  { loc_start = start ;
    loc_end = stop ;
  }
;;

let mk_lit = Rdf_term.mk_literal ;;
let xsd_integer = Rdf_rdf.xsd_integer;;
let xsd_double = Rdf_rdf.xsd_double;;
let xsd_decimal = Rdf_rdf.xsd_decimal;;
let xsd_boolean = Rdf_rdf.xsd_boolean;;

let mk_integer = mk_lit ~typ: xsd_integer;;
let mk_decimal = mk_lit ~typ: xsd_decimal;;
let mk_double = mk_lit ~typ: xsd_double;;
let mk_boolean = mk_lit ~typ: xsd_boolean;;

%}

%token <Rdf_sparql_types.rel_iri>Iriref_
%token <string>Var1
%token <string>Var2
%token <Rdf_sparql_types.prefixed_name> Pname_ln
%token <Rdf_sparql_types.pname_ns> Pname_ns
%token <string> Blank_node_label

%token EOF
%token A
%token ANON
%token AS
%token BASE PREFIX
%token SELECT CONSTRUCT DESCRIBE ASK
%token DISTINCT REDUCED
%token VALUES FROM NAMED GROUP BY HAVING ORDER ASC DESC LIMIT OFFSET WHERE
%token NIL COMMA DOT PIPE HAT HATHAT QM SEMICOLON
%token LPAR RPAR
%token LBRACE RBRACE
%token LBRACKET RBRACKET

%token PLUS MINUS STAR SLASH BANG


%token UNDEF
%token UNION OPTIONAL GRAPH SERVICE SILENT BIND FILTER
%token AMPAMP PIPEPIPE
%token EQUAL NOTEQUAL LT GT LTE GTE NOT IN

%left AMPAMP
%left PIPEPIPE
%nonassoc EQUAL NOTEQUAL LT GT LTE GTE NOT IN
%left PLUS MINUS        /* lowest precedence */
%left STAR SLASH         /* medium precedence */
%nonassoc BANG      /* highest precedence */

%token ABS AVG BNODE BOUND CEIL COALESCE CONCAT CONTAINS COUNT
%token DATATYPE DAY ENCODE_FOR_URI EXISTS FLOOR GROUP_CONCAT HOURS
%token IF IRI ISBLANK ISIRI ISLITERAL ISNUMERIC ISURI LANG LANGMATCHES
%token LCASE MAX MD5 MIN MINUTES MONTH NOW RAND REGEX REPLACE ROUND
%token SAMETERM SAMPLE SECONDS SEPARATOR SHA1 SHA256 SHA384 SHA512
%token STR STRAFTER STRBEFORE STRDT STRENDS STRLANG STRLEN STRSTARTS
%token STRUUID SUBSTR SUM TIMEZONE TZ UCASE URI UUID YEAR

%token <string>Integer
%token <string>Decimal
%token <string>Double
%token <string>Integer_positive
%token <string>Decimal_positive
%token <string>Double_positive
%token <string>Integer_negative
%token <string>Decimal_negative
%token <string>Double_negative
%token <string>Boolean
%token <string>String_literal
%token <string>Langtag

%start <Rdf_sparql_types.query> query

%%

%public query:
  p=prologue k=query_kind v=values_clause EOF
  {
    { q_prolog = p ;
      q_kind = k ;
      q_values = v ;
    }
  }
;

prologue: list(prologue_item) { $1 };

prologue_item:
| BASE Iriref_ { BaseDecl $2 }
| PREFIX name=Pname_ns ir=Iriref_ { PrefixDecl (name, ir) }
;

query_kind:
| s=select_clause ds=list(dataset_clause) w=where_clause m=solution_modifier
  {
    Select {
      select_select = s ; select_dataset = ds ;
      select_where = w ; select_modifier = m ;
    }
  }
| CONSTRUCT t=construct_template ds=list(dataset_clause) w=where_clause m=solution_modifier
  {
    Construct
      { constr_template = Some t ;
        constr_dataset = ds ;
        constr_where = Constr_ggp w ;
        constr_modifier = m ;
      }
  }
| CONSTRUCT ds=list(dataset_clause) WHERE LBRACE l=triples_template RBRACE m=solution_modifier
  {
    Construct
      { constr_template = None ;
        constr_dataset = ds ;
        constr_where = Constr_template l ;
        constr_modifier = m ;
      }
  }
| DESCRIBE l=nonempty_list(var_or_iri) ds=list(dataset_clause) w=option(where_clause) m=solution_modifier
  {
    Describe {
      desc_sel = l;
      desc_dataset = ds ;
      desc_where = w ;
      desc_modifier = m ;
    }
  }
| DESCRIBE STAR ds=list(dataset_clause) w=option(where_clause) m=solution_modifier
  {
    Describe {
      desc_sel = [] ;
      desc_dataset = ds ;
      desc_where = w ;
      desc_modifier = m ;
    }
  }

| ASK ds=list(dataset_clause) w=where_clause m=solution_modifier
  {
    Ask {
      ask_dataset = ds ;
      ask_where = w ;
      ask_modifier = m ;
    }
  }
;

select_clause:
| SELECT f=option(select_flag) v=select_vars
   {
    { sel_flag = f ;
      sel_vars = v ;
    }
  }
;

select_flag:
| DISTINCT { Distinct }
| REDUCED { Reduced }
;

select_vars:
| STAR { SelectAll }
| nonempty_list(select_var) { SelectVars $1 }
;

select_var:
| var {
    { sel_var_loc = mk_loc $startpos($1) $endpos($1) ;
      sel_var_expr = None ;
      sel_var = $1 ;
    }
  }
| LPAR e=expression AS v=var RPAR {
    { sel_var_loc = mk_loc $startpos(e) $endpos(v) ;
      sel_var_expr = Some e ;
      sel_var = v ;
    }
  }
;

construct_template:
 LBRACE option(construct_triples) RBRACE {
   match $2 with None -> [] | Some l -> l
  }
;

construct_triples:
| triples_same_subject { [ $1 ] }
| triples_same_subject DOT { [ $1 ] }
| triples_same_subject DOT construct_triples { $1 :: $3 }
;

triples_template: construct_triples { $1 }
;

triples_same_subject:
| v=var_or_term l=property_list_not_empty
  {
    TriplesVar (v, l)
  }
| t=triples_node l=property_list
  {
    TriplesNode (t, l)
  }
;

dataset_clause:
| FROM source_selector { DefaultGraphClause $2 }
| FROM NAMED source_selector { NamedGraphClause $3 }
;

source_selector: iri { $1 }
;

iri:
| Iriref_ { Rdf_sparql_types.Reliri $1 }
| prefixed_name { Rdf_sparql_types.PrefixedName $1 }
;

prefixed_name:
| Pname_ns {
    { pname_loc = mk_loc $startpos($1) $endpos($1) ;
      pname_ns = $1 ;
      pname_local = None ;
    }
  }
| Pname_ln {
    $1
  }
;

values_clause: option(values_clause_) { $1 }
;

values_clause_:
| VALUES d=datablock { d }
;

datablock:
| inline_data_one_var { InLineDataOneVar $1 }
| inline_data_full { InLineDataFull $1 }
;

inline_data_one_var:
v=var LBRACE l=list(data_block_value) RBRACE
  {
    let loc = mk_loc $startpos(v) $endpos($4) in
    { idov_loc = loc ;
      idov_var = v ;
      idov_data = l ;
    }
  }
;

inline_data_full:
  vars=nil_or_var_list LBRACE l=list(data_block_values_or_nil) RBRACE
  {
    let loc = mk_loc $startpos(vars) $endpos($4) in
    {
      idf_loc = loc ;
      idf_vars = vars ;
      idf_values = l ;
    }
  }
;

nil_or_var_list:
| NIL { [] }
| LPAR l=list(var) RPAR { l }
;

data_block_values_or_nil:
| NIL { Nil }
| LPAR l=list(data_block_value) RPAR { Value l }
;

data_block_value:
| iri { DataBlockValueIri $1 }
| rdf_literal { DataBlockValueRdf $1 }
| numeric_literal { DataBlockValueNumeric $1 }
| boolean_literal { DataBlockValueBoolean $1 }
| UNDEF { DataBlockValueUndef }
;

solution_modifier:
| g=option(group_clause) h=option(having_clause) o=option(order_clause) lo=option(limit_offset_clause)
  {
    let loc = mk_loc $startpos(g) $endpos(lo) in
    { solmod_loc = loc ;
      solmod_group = (match g with None -> [] | Some l -> l) ;
      solmod_having = (match h with None -> [] | Some l -> l) ;
      solmod_order = o ;
      solmod_limoff = lo ;
    }
  }
;

group_clause:
| GROUP BY l=nonempty_list(group_condition)
  { l }
;

group_condition:
| built_in_call { GroupBuiltInCall $1 }
| function_call { GroupFunctionCall $1 }
| group_var { GroupVar $1 }
;

group_var:
| v=var {
    let loc = mk_loc $startpos(v) $endpos(v) in
    { grpvar_loc = loc ; grpvar_expr = None ; grpvar = Some v }
  }
| e=expression AS v=var {
    let loc = mk_loc $startpos(e) $endpos(v) in
    { grpvar_loc = loc ; grpvar_expr = Some e ; grpvar = Some v }
  }
| e=expression {
    let loc = mk_loc $startpos(e) $endpos(e) in
    { grpvar_loc = loc ; grpvar_expr = Some e ; grpvar = None }
  }
;

limit_offset_clause:
| l=limit_clause o=option(offset_clause)
  {
    let loc = mk_loc $startpos(l) $endpos(o) in
    { limoff_loc = loc ; limoff_limit = Some l ; limoff_offset = o }
  }
| o=offset_clause l=option(limit_clause)
  {
    let loc = mk_loc $startpos(o) $endpos(l) in
    { limoff_loc = loc ; limoff_limit = l ; limoff_offset = Some o }
  }
;

limit_clause: LIMIT Integer { int_of_string $2 };
offset_clause: OFFSET Integer { int_of_string $2 };

order_clause:
| ORDER BY l=nonempty_list(order_condition) { l }
;

order_condition:
| ASC bracketted_expression { OrderAsc $2 }
| DESC bracketted_expression { OrderDesc $2 }
| constraint_ { OrderConstr $1 }
| var { OrderVar $1 }
;

having_clause:
| HAVING l=nonempty_list(having_condition) { l }
;

having_condition: constraint_ { $1 }
;

bracketted_expression: LPAR expression RPAR { $2 };

constraint_:
| bracketted_expression { ConstrExpr $1 }
| built_in_call { ConstrBuiltInCall $1 }
| function_call { ConstrFunctionCall $1 }
;

where_clause:
| option(WHERE) group_graph_pattern { $2 }
;

group_graph_pattern:
| LBRACE subselect RBRACE { SubSelect $2 }
| LBRACE group_graph_pattern_sub RBRACE { GGPSub $2 }
;

subselect:
| sel=select_clause w=where_clause sol=solution_modifier v=values_clause
  {
   let loc = mk_loc $startpos(sel) $endpos(v) in
   {
    subsel_loc = loc ;
    subsel_select = sel ;
    subsel_where = w ;
    subsel_modifier = sol ;
    subsel_values = v ;
   }
  }
;

group_graph_pattern_sub:
| t=option(triples_block) l=list(gp_triples)
  {
    let loc = mk_loc $startpos(t) $endpos(l) in
    let l =
      List.fold_left
        (fun acc (g, t) ->
          match t with
            None -> g :: acc
          | Some t -> t :: g :: acc)
        []
        l
    in
    let l = List.rev l in
    let elts = match t with None -> l | Some t -> t :: l in
    { ggp_sub_loc = loc ;
      ggp_sub_elts = elts ;
    }
  }
;

gp_triples:
| gp=graph_pattern_not_triples option(DOT) t=option(triples_block)
  { (gp, t) }
;

triples_block:
| l=triples_block_details
  {
    let loc = mk_loc $startpos(l) $endpos(l) in
    Triples
      { triples_loc = loc ;
        triples = l ;
      }
  }
;
triples_block_details:
| s=triples_same_subject_path rest=option(triples_block_details2)
  {
    match rest with
      None -> [s]
    | Some l -> s :: l
  }
;

triples_block_details2:
| DOT t=option(triples_block_details)
  { match t with None -> [] | Some l -> l }
;

triples_same_subject_path:
| v=var_or_term p=property_list_path_not_empty
  {
    TriplesVar (v, p)
  }
| t=triples_node_path p=property_list_path
  {
    TriplesNode (t, p)
  }
;

property_list_path: l=option(property_list_path_not_empty)
  { match l with None -> [] | Some l -> l }
;

property_list_path_not_empty:
| v=verb_path_or_simple olp=object_list_path more=list(verbp_object_list_l)
  {
    let loc = mk_loc $startpos(v) $endpos(more) in
    let more = List.fold_left
      (fun acc -> function
         | None -> acc
         | Some x -> x :: acc
      )
      []
      more
    in
    {
      propol_loc = loc ;
      propol_verb = v ;
      propol_objects = olp ;
    } :: (List.rev more)
  }
;

verbp_object_list_l:
| SEMICOLON option(verbp_object_list)
  { $2 }
;

verbp_object_list:
| v=verb_path_or_simple l=object_list
  {
    let loc = mk_loc $startpos(v) $endpos(l) in
    { propol_loc = loc ;
      propol_verb = v ;
      propol_objects = l
    }
  }
;

object_list: separated_nonempty_list(COMMA, object_) { $1 }
;

object_: graph_node { $1 }
;

verb_path_or_simple:
| path { VerbPath $1 }
| var { VerbVar $1 }
;

graph_node:
| var_or_term { GraphNodeVT $1 }
| triples_node { GraphNodeTriples $1 }
;

triples_node:
| collection { TNodeCollection $1 }
| blank_node_property_list { TNodeBlank $1 }
;

collection:
LPAR nonempty_list(graph_node) RPAR { $2 }
;

blank_node_property_list:
LBRACKET property_list_not_empty RBRACKET { $2 }
;

property_list:
  l=option(property_list_not_empty) {
    match l with None -> [] | Some l -> l
  }
;

property_list_not_empty:
| v=verb_object_list l=list(verb_object_list_l)
  {
    let l = List.fold_left
      (fun acc -> function
        | None -> acc
        | Some x -> x :: acc
       )
       []
       l
     in
     let l = List.rev l in
     v :: l
  }
;

verb_object_list_l:
| SEMICOLON option(verb_object_list)
  { $2 }
;

verb_object_list:
| v=verb l=object_list
  {
    let loc = mk_loc $startpos(v) $endpos(l) in
    { propol_loc = loc ;
      propol_verb = v ;
      propol_objects = l
    }
  }
;

verb:
| var_or_iri {
  match $1 with
    VIVar v -> VerbVar v
  | VIIri iri -> VerbIri iri
  }
| A { VerbA }
;

var_or_iri:
| var { VIVar $1 }
| iri { VIIri $1 }
;

var_or_term:
| var { Var $1 }
| graph_term { GraphTerm $1 }
;

var:
| s=Var1
  {
    let loc = mk_loc $startpos(s) $endpos(s) in
    { var_loc = loc ; var_name = s }
  }
| s=Var2
  {
    let loc = mk_loc $startpos(s) $endpos(s) in
    { var_loc = loc ; var_name = s }
  }
;

graph_term:
| iri { GraphTermIri $1 }
| rdf_literal { GraphTermLit $1 }
| numeric_literal { GraphTermNumeric $1 }
| boolean_literal { GraphTermBoolean $1 }
| blank_node { GraphTermBlank $1 }
| NIL { GraphTermNil }
;

graph_pattern_not_triples:
| group_of_union_graph_pattern { $1 }
| optional_graph_pattern { $1 }
| minus_graph_pattern { $1 }
| graph_graph_pattern { $1 }
| service_graph_pattern { $1 }
| filter { $1 }
| bind { $1 }
| inline_data { $1 }
;

group_of_union_graph_pattern:
  l=separated_nonempty_list(UNION, group_graph_pattern) { Union l }
;

optional_graph_pattern:
  OPTIONAL g=group_graph_pattern { Optional g }
;

minus_graph_pattern:
  MINUS g=group_graph_pattern { Minus g }
;

graph_graph_pattern:
  GRAPH vi=var_or_iri g=group_graph_pattern
  {
    let loc = mk_loc $startpos($1) $endpos(g) in
    let t =
      { graphgp_loc = loc ;
        graphgp_name = vi ;
        graphgp_pat = g ;
      }
    in
    GGP t
  }
;

service_graph_pattern:
  SERVICE silent=option(SILENT) vi=var_or_iri g=group_graph_pattern
  {
    let loc = mk_loc $startpos($1) $endpos(g) in
    let t =
      { servgp_loc = loc ;
        servgp_silent = silent <> None ;
        servgp_name = vi ;
        servgp_pat = g ;
      }
    in
    Service t
  }
;

filter: FILTER constraint_ { Filter $2 }
;

bind:
  BIND LPAR e=expression AS v=var RPAR
  {
    let loc = mk_loc $startpos($1) $endpos($6) in
    let t =
      { bind_loc = loc ;
        bind_expr = e ;
        bind_var = v ;
      }
    in
    Bind t
  }
;

inline_data:
  VALUES datablock { InlineData $2 }
;

object_list_path: separated_nonempty_list(COMMA, object_path) { $1 }
;

object_path: graph_node_path { $1 }
;

graph_node_path:
| var_or_term { GraphNodeVT $1 }
| triples_node_path { GraphNodeTriples $1 }
;

triples_node_path:
| collection_path { TNodeCollection $1 }
| blank_node_property_list_path { TNodeBlank $1 }
;

collection_path:
LPAR nonempty_list(graph_node_path) RPAR { $2 }
;

blank_node_property_list_path:
LBRACKET property_list_path_not_empty RBRACKET { $2 }
;

path: path_alternative { $1 };

path_alternative: separated_nonempty_list(PIPE, path_sequence)
  { $1 }
;

path_sequence: separated_nonempty_list(SLASH, path_elt_or_inverse)
  { $1 }
;

path_elt_or_inverse:
| path_elt { Elt $1 }
| HAT path_elt { Inv $2 }
;

path_elt:
| p=path_primary m=option(path_mod)
  {
    let loc = mk_loc $startpos(p) $endpos(m) in
    { pelt_loc = loc ;
      pelt_primary = p ;
      pelt_mod = m ;
    }
  }
;

path_primary:
| iri { PathIri $1 }
| A { PathA }
| BANG path_negated_property_list { PathNegPropSet $2 }
| LPAR path RPAR { Path $2 }
;

path_negated_property_list:
| path_one_in_property_set { [ $1 ] }
| LPAR l=separated_list(PIPE, path_one_in_property_set) { l }
;

path_one_in_property_set:
| iri { PathOneInIri $1 }
| A { PathOneInA }
| HAT iri { PathOneInNotIri $2 }
| HAT A { PathOneInNotA }
;

path_mod:
| QM { ModOptional }
| STAR { ModList }
| PLUS { ModOneOrMore }
;

blank_node:
| s=Blank_node_label
  {
    let loc = mk_loc $startpos(s) $endpos(s) in
    { bnode_loc = loc ;
      bnode_label = Some s ;
    }
  }
| ANON
  {
    let loc = mk_loc $startpos($1) $endpos($1) in
    { bnode_loc = loc ;
      bnode_label = None ;
    }
  }
;


arg_list:
| NIL {
    let loc = mk_loc $startpos($1) $endpos($1) in
    { argl_loc = loc ; argl_distinct = false ; argl = [] }
  }
| LPAR o=option(DISTINCT) l=separated_nonempty_list(COMMA, expression) RPAR
  {
    let loc = mk_loc $startpos($1) $endpos($4) in
    { argl_loc = loc ; argl_distinct = o <> None ; argl = l }
  }
;

iri_or_function:
| iri=iri
  {
    EIri iri
  }
| function_call { EFuncall $1 }
;

function_call:
| i=iri a=arg_list
  {
    let loc = mk_loc $startpos(i) $endpos(a) in
    { func_loc = loc ; func_iri = i ; func_args = a }
  }
;

(*
expression:
  e=and_expression { e }
| e1=and_expression PIPEPIPE e2=and_expression {
    let loc = mk_loc $startpos(e1) $endpos(e2) in
    { expr_loc = loc ; expr = EBin (e1, EOr, e2) }
  }
;

and_expression:
| e1=value_logical AMPAMP e2=value_logical
  {
    { expr_loc = mk_loc $startpos(e1) $endpos(e2) ;
      expr = EBin (e1, EAnd, e2) ;
    }
  }
| e=value_logical { e }
;
*)
expression: e=value_logical { e }
;

value_logical:
  e = relational_expression { e }

| e1=value_logical AMPAMP e2=value_logical
  {
    { expr_loc = mk_loc $startpos(e1) $endpos(e2) ;
      expr = EBin (e1, EAnd, e2) ;
    }
  }
| e1=value_logical PIPEPIPE e2=value_logical {
    let loc = mk_loc $startpos(e1) $endpos(e2) in
    { expr_loc = loc ; expr = EBin (e1, EOr, e2) }
  }
;

;

relational_expression:
| e1=numexp EQUAL e2=numexp {
    { expr_loc = mk_loc $startpos(e1) $endpos(e2) ;
      expr = EBin (e1, EEqual, e2) ;
    }
  }
| e1=numexp NOTEQUAL e2=numexp {
    { expr_loc = mk_loc $startpos(e1) $endpos(e2) ;
      expr = EBin (e1, ENotEqual, e2) ;
    }
  }
| e1=numexp LT e2=numexp {
    { expr_loc = mk_loc $startpos(e1) $endpos(e2) ;
      expr = EBin (e1, ELt, e2) ;
    }
  }
| e1=numexp GT e2=numexp {
    { expr_loc = mk_loc $startpos(e1) $endpos(e2) ;
      expr = EBin (e1, EGt, e2) ;
    }
  }
| e1=numexp LTE e2=numexp {
    { expr_loc = mk_loc $startpos(e1) $endpos(e2) ;
      expr = EBin (e1, ELte, e2) ;
    }
  }
| e1=numexp GTE e2=numexp {
    { expr_loc = mk_loc $startpos(e1) $endpos(e2) ;
      expr = EBin (e1, EGte, e2) ;
    }
  }
| e=numexp IN l=expression_list {
    { expr_loc = mk_loc $startpos(e) $endpos(l) ;
      expr = EIn (e, l) ;
    }
  }
| e=numexp NOT IN l=expression_list {
    { expr_loc = mk_loc $startpos(e) $endpos(l) ;
      expr = ENotIn (e, l) ;
    }
  }
| numexp { $1 }

;

numexp:
| e=primary_expression { e }
| e1=expression STAR e2=expression {
    let loc = mk_loc $startpos(e1) $endpos(e2) in
    { expr_loc = loc ;
      expr = EBin (e1, EMult, e2) ;
    }
  }
| e1=expression SLASH e2=expression {
    let loc = mk_loc $startpos(e1) $endpos(e2) in
    { expr_loc = loc ;
      expr = EBin (e1, EDiv, e2) ;
    }
  }
| e1=expression PLUS e2=expression {
    let loc = mk_loc $startpos(e1) $endpos(e2) in
    { expr_loc = loc ;
      expr = EBin (e1, EPlus, e2) ;
    }
  }
| e1=expression MINUS e2=expression {
    let loc = mk_loc $startpos(e1) $endpos(e2) in
    { expr_loc = loc ;
      expr = EBin (e1, EMinus, e2) ;
    }
  }
| e1=expression lit=numeric_literal_positive {
    let loc = mk_loc $startpos(e1) $endpos(lit) in
    let lit2 =
      let s = lit.Rdf_term.lit_value in
      let len = String.length s in
      (* remove starting '+' *)
      { lit with Rdf_term.lit_value = String.sub s 1 (len - 1) }
    in
    let e2 =
      let loc = mk_loc $startpos(lit) $endpos(lit) in
      let lit2 = { rdf_lit_loc = loc ; rdf_lit = lit2 ; rdf_lit_type = None } in
      { expr_loc = loc ;
        expr = ENumeric lit2 ;
      }
    in
    { expr_loc = loc ;
      expr = EBin (e1, EPlus, e2) ;
    }
  }
| e1=expression lit=numeric_literal_negative {
    let loc = mk_loc $startpos(e1) $endpos(lit) in
    let lit2 =
      let s = lit.Rdf_term.lit_value in
      let len = String.length s in
      (* remove starting '-' *)
      { lit with Rdf_term.lit_value = String.sub s 1 (len - 1) }
    in
    let e2 =
      let loc = mk_loc $startpos(lit) $endpos(lit) in
      let lit2 = { rdf_lit_loc = loc ; rdf_lit = lit2 ; rdf_lit_type = None } in
      { expr_loc = loc ;
        expr = ENumeric lit2 ;
      }
    in
    { expr_loc = loc ;
      expr = EBin (e1, EMinus, e2) ;
    }
  }
| BANG e=expression {
    { expr_loc = mk_loc $startpos($1) $endpos(e) ;
      expr = ENot e ;
    }
  }
| PLUS e=expression { e }
| MINUS e=expression %prec BANG {
    { expr_loc = mk_loc $startpos($1) $endpos(e) ;
      expr = EUMinus e ;
    }
  }
;

expression_list:
| NIL { [] }
| LPAR l=separated_nonempty_list(COMMA, expression) RPAR { l }
;

primary_expression:
| bracketted_expression { $1 }
| bic=built_in_call {
    { expr_loc = mk_loc $startpos(bic) $endpos(bic) ;
      expr = EBic bic ;
    }
  }
| lit=rdf_literal {
    { expr_loc = mk_loc $startpos(lit) $endpos(lit) ;
      expr = ELit lit ;
    }
  }
| lit=numeric_literal {
  { expr_loc = mk_loc $startpos(lit) $endpos(lit) ;
      expr = ENumeric lit ;
    }
  }
| lit=boolean_literal {
    { expr_loc = mk_loc $startpos(lit) $endpos(lit) ;
      expr = EBoolean lit ;
    }
  }
| v=var {
    { expr_loc = mk_loc $startpos(v) $endpos(v) ;
      expr = EVar v ;
    }
  }
| x=iri_or_function {
    { expr_loc = mk_loc $startpos(x) $endpos(x) ;
      expr = x ;
    }
  }
;

numeric_literal:
  numeric_literal_
  {
    let loc = mk_loc $startpos($1) $endpos($1) in
    { rdf_lit_loc = loc ;
      rdf_lit = $1 ;
      rdf_lit_type = None ;
    }
  }
;

numeric_literal_:
| numeric_literal_unsigned { $1 }
| numeric_literal_positive { $1 }
| numeric_literal_negative { $1 }
;

numeric_literal_unsigned:
| Integer { mk_integer $1 }
| Decimal { mk_decimal $1 }
| Double { mk_double $1 }
;

numeric_literal_positive:
| Integer_positive { mk_integer $1 }
| Decimal_positive { mk_decimal $1 }
| Double_positive { mk_double $1 }
;

numeric_literal_negative:
| Integer_negative { mk_integer $1 }
| Decimal_negative { mk_decimal $1 }
| Double_negative { mk_double $1 }
;

boolean_literal:
| Boolean {
    let loc = mk_loc $startpos($1) $endpos($1) in
    { rdf_lit_loc = loc ;
      rdf_lit = mk_boolean $1 ;
      rdf_lit_type = None ;
    }
  }
;

string: String_literal { $1 }
;

rdf_literal:
| r=rdf_literal_
  {
    let loc = mk_loc $startpos(r) $endpos(r) in
    let (s, lang, typ) = r in
    { rdf_lit_loc = loc ;
      rdf_lit = mk_lit ?lang s ;
      rdf_lit_type = typ ;
    }
  }
;

rdf_literal_:
| s=string { (s, None, None) }
| s=string HATHAT iri=iri { (s, None, Some iri) } (* FIXME: iri can also be a prefixed name *)
| s=string t=Langtag { (s, Some t, None) }
;

built_in_call:
  aggregate { Bic_agg $1 }
| regexp_expression { $1 }
| STR LPAR e=expression RPAR { Bic_fun ("STR", [e]) }
| LANG LPAR e=expression RPAR { Bic_fun ("LANG", [e]) }
| LANGMATCHES LPAR e1=expression COMMA e2=expression RPAR { Bic_fun ("LANGMATCHES", [e1; e2]) }
| DATATYPE LPAR e=expression RPAR { Bic_fun ("DATATYPE", [e])}
| BOUND LPAR v=var RPAR { Bic_BOUND v }
| IRI LPAR e=expression RPAR { Bic_fun ("IRI", [e])}
| URI LPAR e=expression RPAR { Bic_fun ("URI", [e])}
| BNODE LPAR e=expression RPAR { Bic_fun ("BNODE", [e]) }
| BNODE NIL { Bic_fun ("BNODE", []) }
| RAND NIL { Bic_fun ("RAND", []) }
| ABS LPAR e=expression RPAR { Bic_fun ("ABS", [e])}
| CEIL LPAR e=expression RPAR { Bic_fun ("CEIL", [e])}
| FLOOR LPAR e=expression RPAR { Bic_fun ("FLOOR", [e])}
| ROUND LPAR e=expression RPAR { Bic_fun ("ROUND", [e])}
| CONCAT e=expression_list { Bic_fun ("CONCAT", e)}
| substring_expression { $1 }
| STRLEN LPAR e=expression RPAR { Bic_fun ("STRLEN", [e])}
| str_replace_expression { $1 }
| UCASE LPAR e=expression RPAR { Bic_fun ("UCASE", [e])}
| LCASE LPAR e=expression RPAR { Bic_fun ("LCASE", [e])}
| ENCODE_FOR_URI LPAR e=expression RPAR { Bic_fun ("ENCODE_FOR_URI", [e])}
| CONTAINS LPAR e1=expression COMMA e2=expression RPAR { Bic_fun ("CONTAINS", [e1 ; e2]) }
| STRSTARTS LPAR e1=expression COMMA e2=expression RPAR { Bic_fun ("STRSTARTS", [e1 ; e2]) }
| STRENDS LPAR e1=expression COMMA e2=expression RPAR { Bic_fun ("STRENDS", [e1 ; e2]) }
| STRBEFORE LPAR e1=expression COMMA e2=expression RPAR { Bic_fun ("STRBEFORE", [e1 ; e2]) }
| STRAFTER LPAR e1=expression COMMA e2=expression RPAR { Bic_fun ("STRAFTER", [e1 ; e2]) }
| YEAR LPAR e=expression RPAR { Bic_fun ("YEAR", [e])}
| MONTH LPAR e=expression RPAR { Bic_fun ("MONTH", [e])}
| DAY LPAR e=expression RPAR { Bic_fun ("DAY", [e])}
| HOURS LPAR e=expression RPAR { Bic_fun ("HOURS", [e])}
| MINUTES LPAR e=expression RPAR { Bic_fun ("MINUTES", [e])}
| SECONDS LPAR e=expression RPAR { Bic_fun ("SECONDS", [e])}
| TIMEZONE LPAR e=expression RPAR { Bic_fun ("TIMEZONE", [e])}
| TZ LPAR e=expression RPAR { Bic_fun ("TZ", [e])}
| NOW NIL { Bic_fun ("NOW", []) }
| UUID NIL { Bic_fun ("UUID", []) }
| STRUUID NIL { Bic_fun ("STRUUID", []) }
| MD5 LPAR e=expression RPAR { Bic_fun ("MD5", [e])}
| SHA1 LPAR e=expression RPAR { Bic_fun ("SHA1", [e])}
| SHA256 LPAR e=expression RPAR { Bic_fun ("SHA256", [e])}
| SHA384 LPAR e=expression RPAR { Bic_fun ("SHA384", [e])}
| SHA512 LPAR e=expression RPAR { Bic_fun ("SHA512", [e])}
| COALESCE l=expression_list { Bic_fun("COALESCE", l) }
| IF LPAR e1=expression COMMA e2=expression COMMA e3=expression RPAR { Bic_fun("IF", [e1; e2; e3]) }
| STRLANG LPAR e1=expression COMMA e2=expression RPAR { Bic_fun ("STRLANG", [e1 ; e2]) }
| STRDT LPAR e1=expression COMMA e2=expression RPAR { Bic_fun ("STRDT", [e1 ; e2]) }
| SAMETERM LPAR e1=expression COMMA e2=expression RPAR { Bic_fun ("SAMETERM", [e1 ; e2]) }
| ISIRI LPAR e=expression RPAR { Bic_fun ("ISIRI", [e])}
| ISURI LPAR e=expression RPAR { Bic_fun ("ISURI", [e])}
| ISBLANK LPAR e=expression RPAR { Bic_fun ("ISBLANK", [e])}
| ISLITERAL LPAR e=expression RPAR { Bic_fun ("ISLITERAL", [e])}
| ISNUMERIC LPAR e=expression RPAR { Bic_fun ("ISNUMERIC", [e])}
| EXISTS g=group_graph_pattern { Bic_EXISTS g }
| NOT EXISTS g=group_graph_pattern { Bic_NOTEXISTS g }
;

regexp_expression:
| REGEX LPAR e1=expression COMMA e2=expression RPAR
  { Bic_fun("REGEX", [e1 ; e2]) }
| REGEX LPAR e1=expression COMMA e2=expression COMMA e3=expression RPAR
  { Bic_fun ("REGEX", [e1 ; e2 ; e3]) }
;

substring_expression:
| SUBSTR LPAR e1=expression COMMA e2=expression RPAR
  { Bic_fun("SUBSTR", [e1 ; e2]) }
| SUBSTR LPAR e1=expression COMMA e2=expression COMMA e3=expression RPAR
  { Bic_fun ("SUBSTR", [e1 ; e2 ; e3]) }
;

str_replace_expression:
| REPLACE LPAR e1=expression COMMA e2=expression COMMA e3=expression RPAR
  { Bic_fun("REPLACE", [e1 ; e2 ; e3]) }
| REPLACE LPAR e1=expression COMMA e2=expression COMMA e3=expression COMMA e4=expression RPAR
  { Bic_fun ("REPLACE", [e1 ;e2 ; e3 ; e4]) }
;

aggregate:
| COUNT LPAR d=option(DISTINCT) STAR RPAR { Bic_COUNT (d<>None, None) }
| COUNT LPAR d=option(DISTINCT) e=expression RPAR { Bic_COUNT (d<>None, Some e) }
| SUM LPAR d=option(DISTINCT) e=expression RPAR { Bic_SUM (d<>None, e) }
| MIN LPAR d=option(DISTINCT) e=expression RPAR { Bic_MIN (d<>None, e) }
| MAX LPAR d=option(DISTINCT) e=expression RPAR { Bic_MAX (d<>None, e) }
| AVG LPAR d=option(DISTINCT) e=expression RPAR { Bic_AVG (d<>None, e) }
| SAMPLE LPAR d=option(DISTINCT) e=expression RPAR { Bic_SAMPLE (d<>None, e) }
| GROUP_CONCAT LPAR d=option(DISTINCT) e=expression RPAR { Bic_GROUP_CONCAT (d<>None, e, None) }
| GROUP_CONCAT LPAR d=option(DISTINCT) e=expression SEMICOLON SEPARATOR EQUAL sep=string RPAR
  { Bic_GROUP_CONCAT (d<>None, e, Some sep) }
;
