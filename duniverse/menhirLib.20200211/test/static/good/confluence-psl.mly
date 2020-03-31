/*
    InFormal Digital Logic Verification Environment
    Copyright (C) 2004 Tom Hawkins (tomahawkins@yahoo.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
*/

%{
(* Pre Code *)
let parse_error s =
  Parser_util.error ("Unexpected token: '" ^ (Parser_util.get_current_token ()) ^ "'")
;;

let debug msg =
  print_string msg; print_newline ()
;;

%}

// PSL Keywords and Operators

%token <Psl_ast.token> Brackl
%token <Psl_ast.token> Brackr
%token <Psl_ast.token> Parenl
%token <Psl_ast.token> Parenr
%token <Psl_ast.token> Bracel
%token <Psl_ast.token> Bracer
%token <Psl_ast.token> Comma
%token <Psl_ast.token> Semicolon
%token <Psl_ast.token> Colon
%token <Psl_ast.token> Period_period
%token <Psl_ast.token> Equal
%token <Psl_ast.token> Colon_equal
%token <Psl_ast.token> Aster
%token <Psl_ast.token> Plus
%token <Psl_ast.token> Bar_dash_gt
%token <Psl_ast.token> Bar_equal_gt
%token <Psl_ast.token> Lt_dash_gt
%token <Psl_ast.token> Dash_gt
%token <Psl_ast.token> Brackl_aster
%token <Psl_ast.token> Brackl_plus_brackr
%token <Psl_ast.token> Brackl_dash_gt
%token <Psl_ast.token> Brackl_equal
%token <Psl_ast.token> Amp_amp
%token <Psl_ast.token> Amp
%token <Psl_ast.token> Bar_bar
%token <Psl_ast.token> Bar
%token <Psl_ast.token> Bang
%token <Psl_ast.token> Dollar
%token <Psl_ast.token> At
%token <Psl_ast.token> Period
%token <Psl_ast.token> Slash
%token <Psl_ast.token> A
%token <Psl_ast.token> AG
%token <Psl_ast.token> AF
%token <Psl_ast.token> AX
%token <Psl_ast.token> Abort
%token <Psl_ast.token> Always
%token <Psl_ast.token> Assert
%token <Psl_ast.token> Assume
%token <Psl_ast.token> Assume_guarantee
%token <Psl_ast.token> Before
%token <Psl_ast.token> Before_bang
%token <Psl_ast.token> Before_bang_
%token <Psl_ast.token> Before_
%token <Psl_ast.token> Boolean
%token <Psl_ast.token> Clock
%token <Psl_ast.token> Const
%token <Psl_ast.token> Countones
%token <Psl_ast.token> Cover
%token <Psl_ast.token> Default
%token <Psl_ast.token> E
%token <Psl_ast.token> EF
%token <Psl_ast.token> EG
%token <Psl_ast.token> EX
%token <Psl_ast.token> Endpoint
%token <Psl_ast.token> Eventually_bang
%token <Psl_ast.token> F
%token <Psl_ast.token> Fairness
%token <Psl_ast.token> Fell
%token <Psl_ast.token> Forall
%token <Psl_ast.token> G
%token <Psl_ast.token> In
%token <Psl_ast.token> Inf
%token <Psl_ast.token> Inherit
%token <Psl_ast.token> Isunknown
%token <Psl_ast.token> Never
%token <Psl_ast.token> Next
%token <Psl_ast.token> Next_bang
%token <Psl_ast.token> Next_a
%token <Psl_ast.token> Next_a_bang
%token <Psl_ast.token> Next_e
%token <Psl_ast.token> Next_e_bang
%token <Psl_ast.token> Next_event
%token <Psl_ast.token> Next_event_bang
%token <Psl_ast.token> Next_event_a
%token <Psl_ast.token> Next_event_a_bang
%token <Psl_ast.token> Next_event_e
%token <Psl_ast.token> Next_event_e_bang
%token <Psl_ast.token> Onehot
%token <Psl_ast.token> Onehot0
%token <Psl_ast.token> Property
%token <Psl_ast.token> Prev
%token <Psl_ast.token> Report
%token <Psl_ast.token> Restrict
%token <Psl_ast.token> Restrict_guarantee
%token <Psl_ast.token> Rose
%token <Psl_ast.token> Sequence
%token <Psl_ast.token> Stable
%token <Psl_ast.token> Strong
%token <Psl_ast.token> U
%token <Psl_ast.token> W
%token <Psl_ast.token> Union
%token <Psl_ast.token> Until
%token <Psl_ast.token> Until_bang
%token <Psl_ast.token> Until_bang_
%token <Psl_ast.token> Until_
%token <Psl_ast.token> Vmode
%token <Psl_ast.token> Vprop
%token <Psl_ast.token> Vunit
%token <Psl_ast.token> Within
%token <Psl_ast.token> X
%token <Psl_ast.token> X_bang

// HDL Keywords and Operators
%token <Psl_ast.token> Negedge
%token <Psl_ast.token> Posedge
%token <Psl_ast.token> Equal_equal
%token <Psl_ast.token> Bang_equal
%token <Psl_ast.token> Lt
%token <Psl_ast.token> Lt_equal
%token <Psl_ast.token> Gt
%token <Psl_ast.token> Gt_equal
%token <Psl_ast.token> Dash
%token <Psl_ast.token> Tildy
%token <Psl_ast.token> Carrot

// Literals
%token <Psl_ast.token> Integer
%token <Psl_ast.token> Integer_width
%token <Psl_ast.token> Identifier
%token <Psl_ast.token> Identifier_esc
%token <Psl_ast.token> String

// Misc
%token <Psl_ast.token> EOF
%token <Psl_ast.token> Lexer_error


// Operator Precedence and Associativity

%right Always Never G
%right Dash_gt Lt_dash_gt
%right Bar_dash_gt Bar_equal_gt
%right U W Until Until_bang Until_bang_ Until_ Before Before_bang Before_bang_ Before_
%right X X_bang F Eventually_bang Next Next_bang Next_a Next_a_bang Next_e Next_e_bang Next_event Next_event_bang Next_event_a_bang Next_event_e_bang
%left  AX AG AF EX EG EF
%left  Abort
%left  Semicolon
%left  Colon
%left  Bar Bar_bar
%left  Amp Amp_amp
%left  Within
%left  Brackl_aster Brackl_plus_brackr Brackl_equal Brackl_dash_gt
%left  At
%left  Union
%left  Bang
// HDL operators have highest precedence.
%right Question
%left  Vl_bar_bar
%left  Vl_amp_amp
%left  Vl_bar
%left  Carrot
%left  Vl_amp
%left  Equal_equal Bang_equal
%left  Lt Lt_equal Gt Gt_equal
%left  Plus Dash
%left  Tildy Vl_bang Vl_uplus Vl_uminus
%left  Brackl

%start psl_specification
%type <Psl_ast.spec> psl_specification

%%

psl_specification
  : verification_items EOF { List.rev $1 }
  ;

verification_items
  : { [] }
  | verification_items verification_unit { $2 :: $1 }
  ;

verification_unit
  :  vunit_type identifier                                     Bracel inherit_specs vunit_items Bracer { raise (Parser_util.Error "Verification unit requires module name.") }
  |  vunit_type identifier Parenl hierarchical_hdl_name Parenr Bracel inherit_specs vunit_items Bracer { List.hd $4, List.rev $8 }
  ;

vunit_type
  : Vunit { $1 }
  | Vprop { raise (Parser_util.Error "\"vprop\" not supported.") }
  | Vmode { raise (Parser_util.Error "\"vmode\" not supported.") }
  ;

hierarchical_hdl_name
  : identifier hierarchical_hdl_names { fst $1 :: List.rev $2 }
  ;

hierarchical_hdl_names
  :                                                   { [] }
  | hierarchical_hdl_names path_separator identifier  { raise (Parser_util.Error "Hierarchical names are not supported.") }
  ;

path_separator
  : Period { () }
  | Slash  { () }
  ;

inherit_specs
  :                            { [] }
  | inherit_specs inherit_spec { $2 :: $1 }
  ;

inherit_spec
  : Inherit identifier comma_identifiers { Parser_util.error "\"inherit\" not supported." }
  ;

vunit_items
  :                        { [] }
  | vunit_items vunit_item { $2 :: $1 }
  ;

vunit_item
  // XXX : psl_declaration { $1 }
  : psl_directive   { $1 }
  ;

// PSL Declarations

/*
psl_declaration
  : Property identifier formal_parameter_list Equal property         Semicolon { raise (Parser_util.Error "\"property\" declaration not supported.") }
  | Sequence identifier formal_parameter_list Equal sequence         Semicolon { raise (Parser_util.Error "\"sequence\" declaration not supported.") }
  | Endpoint identifier formal_parameter_list Equal sequence         Semicolon { raise (Parser_util.Error "\"endpoint\" declaration not supported.") }
  | Default Clock                             Equal clock_expression Semicolon { raise (Parser_util.Error "\"default clock\" declaration not supported.") }
  ;

formal_parameter_list
  :
  | Parenl formal_parameter formal_parameters Parenr { () }
  ;

formal_parameters
  :
  | Semicolon formal_parameter { () }
  ;

formal_parameter
  : param_type identifier comma_identifiers { () }
  ;

param_type
  : Const    { () }
  | Boolean  { () }
  | Property { () }
  | Sequence { () }
  ;

actual_parameter_list
  : property                             { () }
  | actual_parameter_list Comma property { () }
  ;
*/


// PSL Directives

psl_directive
  :                  verification_directive { $1 }
  | identifier Colon verification_directive { $3 }
  ;

verification_directive
  : Assert             property report         Semicolon { Psl_ast.Assert (snd $1, $2, $3) }
  | Assume             property                Semicolon { raise (Parser_util.Error "\"assume\" directive not supported.") }
  | Assume_guarantee   property report         Semicolon { raise (Parser_util.Error "\"assume_guarantee\" directive not supported.") }
  | Restrict           property                Semicolon { raise (Parser_util.Error "\"restrict\" directive not supported.") }
  | Restrict_guarantee property report         Semicolon { raise (Parser_util.Error "\"restrict_guarantee\" directive not supported.") }
  | Cover              property report         Semicolon { raise (Parser_util.Error "\"cover\" directive not supported.") }
  | Fairness           property                Semicolon { raise (Parser_util.Error "\"fairness\" directive not supported.") }
  | Strong Fairness    property Comma property Semicolon { raise (Parser_util.Error "\"strong fairness\" directive not supported.") }
  ;

/*
count
  : number { $1, $1 }
  | range  { $1 }
  ;

finite_count
  : number        { $1, $1 }
  | finite_range  { $1 }
  ;

range
  : number range_sym number { $1, $3 }
  | number range_sym Inf    { raise (Parser_util.Error "Infintie range not supported.") }
  ;
*/

finite_range
  : number range_sym number { $1, $3 }
  ;

range_sym
  : Colon         { () }
  | Period_period { () }
  ;

report
  :               { "" }
  | Report String { let s = fst $2 in String.sub s 1 (String.length s - 2) }
  ;

identifier
  : Identifier     { $1 }
  | Identifier_esc { $1 }
  ;

comma_identifiers
  :                                     { () }
  | Comma identifier comma_identifiers  { () }
  ;

/* XXX
value_set
  : Brackl value_range value_ranges Brackr { () }
  | Boolean { () }
  ;

value_ranges
  : { () }
  | value_ranges Comma value_range { () }
  ;

value_range
  : value { () }
  | finite_range { () }
  ;

value
  : boolean { () }
  | number  { () }
  ;
*/

/* XXX
built_in_function_call
  : Prev      Parenl property Parenr { raise (Parser_util.Error "\"prev\" function not supported.") }
  | Prev      Parenl property Comma number Parenr { raise (Parser_util.Error "\"prev\" function not supported.") }
  // XXX | Next      Parenl property Parenr { raise (Parser_util.Error "\"next\" function not supported.") }
  | Stable    Parenl property Parenr { raise (Parser_util.Error "\"stable\" function not supported.") }
  | Rose      Parenl boolean  Parenr { raise (Parser_util.Error "\"rose\" function not supported.") }
  | Fell      Parenl boolean  Parenr { raise (Parser_util.Error "\"fell\" function not supported.") }
  | Isunknown Parenl boolean  Parenr { raise (Parser_util.Error "\"isunknown\" function not supported.") }
  | Countones Parenl boolean  Parenr { raise (Parser_util.Error "\"countones\" function not supported.") }
  | Onehot    Parenl boolean  Parenr { raise (Parser_util.Error "\"onehot\" function not supported.") }
  | Onehot0   Parenl boolean  Parenr { raise (Parser_util.Error "\"onehot0\" function not supported.") }
  ;
*/

number
  : Integer  { int_of_string (fst $1) }
  ;


property
  //: boolean                { $1 }
  : identifier             { Ltl.Variable (fst $1) }
  | Parenl property Parenr { $2 }

  // Replicator Properties
  //| Forall identifier                            In value_set Colon property { () }
  //| Forall identifier Brackl finite_range Brackr In value_set Colon property { () }

  // FL Properties
  // XXX
  // | sequence                           { raise (Parser_util.Error "PSL sequences are not supported.") }
  // | sequence Bang                      { raise (Parser_util.Error "PSL sequences are not supported.") }
  | property At clock_expression       { $3 $1 }
  // XXX
  // | property Abort property            { raise (Parser_util.Error "PSL \"abort\" is not supported.") }
  | Bang property                      { Ltl.not_ $2 }
  | property Amp_amp property          { Ltl.and_ $1 $3 }
  | property Bar_bar property          { Ltl.or_ $1 $3 }
  | property Dash_gt property          { Ltl.imply $1 $3 }
  | property Lt_dash_gt property       { Ltl.equivalent $1 $3 }
  | X property                         { Ltl.next false $2 }
  | X_bang property                    { Ltl.next true  $2 }
  | F property                         { Ltl.eventually $2 }
  | G property                         { Ltl.always $2 }
  | Brackl property U property Brackr  { Ltl.until true  $2 $4 }
  | Brackl property W property Brackr  { Ltl.until false $2 $4 }
  | Always property                    { Ltl.always $2 }
  | Never property                     { Ltl.never $2 }
  | Next property                      { Ltl.next false $2 }
  | Next_bang property                 { Ltl.next true  $2 }
  | Eventually_bang property           { Ltl.eventually $2 }
  | property Until property            { Ltl.until false $1 $3 }
  | property Until_bang property       { Ltl.until true  $1 $3 }
  | property Until_bang_ property      { Ltl.until_overlap true  $1 $3 }
  | property Until_ property           { Ltl.until_overlap false $1 $3 }
  | property Before property           { Ltl.before false $1 $3 }
  | property Before_bang property      { Ltl.before true  $1 $3 }
  | property Before_bang_ property     { Ltl.before_overlap true  $1 $3 }
  | property Before_ property          { Ltl.before_overlap false $1 $3 }

  // Most of the following parametric expressions can not be desugared until evaluation.
  | X          Brackl number Brackr Parenl property Parenr { Ltl.next_repeat false $3 $6 }
  | X_bang     Brackl number Brackr Parenl property Parenr { Ltl.next_repeat true  $3 $6 }
  | Next       Brackl number Brackr Parenl property Parenr { Ltl.next_repeat false $3 $6 }
  | Next_bang  Brackl number Brackr Parenl property Parenr { Ltl.next_repeat true  $3 $6 }
  | Next_a       Brackl finite_range Brackr Parenl property Parenr  { Ltl.next_ae false true  (fst $3) (snd $3) $6 }
  | Next_a_bang  Brackl finite_range Brackr Parenl property Parenr  { Ltl.next_ae true  true  (fst $3) (snd $3) $6 }
  | Next_e       Brackl finite_range Brackr Parenl property Parenr  { Ltl.next_ae false false (fst $3) (snd $3) $6 }
  | Next_e_bang  Brackl finite_range Brackr Parenl property Parenr  { Ltl.next_ae true  false (fst $3) (snd $3) $6 }
  // The first property is really a boolean.
  | Next_event         Parenl property Parenr Parenl property Parenr {Ltl.next_event false $3 $6 }
  | Next_event_bang    Parenl property Parenr Parenl property Parenr {Ltl.next_event true  $3 $6 }
  | Next_event         Parenl property Parenr Brackl number       Brackr Parenl property Parenr { Ltl.next_event_repeat false $6 $3 $9 }
  | Next_event_bang    Parenl property Parenr Brackl number       Brackr Parenl property Parenr { Ltl.next_event_repeat true  $6 $3 $9 }
  | Next_event_a       Parenl property Parenr Brackl finite_range Brackr Parenl property Parenr { Ltl.next_event_ae false true  (fst $6) (snd $6) $3 $9 }
  | Next_event_a_bang  Parenl property Parenr Brackl finite_range Brackr Parenl property Parenr { Ltl.next_event_ae true  true  (fst $6) (snd $6) $3 $9 }
  | Next_event_e       Parenl property Parenr Brackl finite_range Brackr Parenl property Parenr { Ltl.next_event_ae false false (fst $6) (snd $6) $3 $9 }
  | Next_event_e_bang  Parenl property Parenr Brackl finite_range Brackr Parenl property Parenr { Ltl.next_event_ae true  false (fst $6) (snd $6) $3 $9 }

  // XXX | Bracel sequence Bracer Parenl property Parenr  { () }
  // XXX | sequence Bar_dash_gt  property { raise (Parser_util.Error "PSL sequences are not supported.") }
  // XXX | sequence Bar_equal_gt property { raise (Parser_util.Error "PSL sequences are not supported.") }

  // OBE Properties
  | AX property                         { raise (Parser_util.Error "PSL OBE properties are not supported.") }
  | AF property                         { raise (Parser_util.Error "PSL OBE properties are not supported.") }
  | AG property                         { raise (Parser_util.Error "PSL OBE properties are not supported.") }
  | A Brackl property U property Brackr { raise (Parser_util.Error "PSL OBE properties are not supported.") }
  | EX property                         { raise (Parser_util.Error "PSL OBE properties are not supported.") }
  | EF property                         { raise (Parser_util.Error "PSL OBE properties are not supported.") }
  | EG property                         { raise (Parser_util.Error "PSL OBE properties are not supported.") }
  | E Brackl property U property Brackr { raise (Parser_util.Error "PSL OBE properties are not supported.") }
  ;

/*
sere
  : boolean { () }
  | sere Semicolon sere  { () }
  | sere Colon     sere  { () }
  | compound_sere { () }
  ;

compound_sere
  : repeated_sere { () }
  | braced_sere   { () }
  // XXX | clocked_sere  { () }
  | compound_sere Bar     compound_sere { () }
  | compound_sere Amp     compound_sere { () }
  | compound_sere Amp_amp compound_sere { () }
  | compound_sere Within  compound_sere { () }
  ;

sequence
  : repeated_sere { () }
  | braced_sere   { () }
  // XXX | clocked_sere  { () }
  ;

repeated_sere
  : boolean  Brackl_aster       Brackr { () }
  | boolean  Brackl_aster count Brackr { () }
  | sequence Brackl_aster       Brackr { () }
  | sequence Brackl_aster count Brackr { () }
  |          Brackl_aster       Brackr { () }
  |          Brackl_aster count Brackr { () }
  | boolean  Brackl_plus_brackr { () }
  | sequence Brackl_plus_brackr { () }
  |          Brackl_plus_brackr { () }
  | boolean  Brackl_equal count Brackr { () }
  | boolean  Brackl_dash_gt       Brackr { () }
  | boolean  Brackl_dash_gt count Brackr { () }
  ;

braced_sere
  : Bracel sere Bracer { () }
  ;
*/

/* XXX
clocked_sere
  : braced_sere At clock_expression { () }
  ;
*/

clock_expression
  : Parenl Posedge identifier Parenr  { Ltl.posedge (fst $3) }
  | Parenl Negedge identifier Parenr  { Ltl.negedge (fst $3) }
  | Rose   Parenl  identifier Parenr  { Ltl.posedge (fst $3) }
  | Fell   Parenl  identifier Parenr  { Ltl.negedge (fst $3) }
  ;

// Boolean, Vector, and Constant Expressions
// XXX For now, everything is a property.
/*
boolean
  : identifier { () }
  | boolean Brackl finite_count Brackr  { () }
  | Integer  { () }
  | Integer_width  { () }
  | built_in_function_call { () }
  | identifier Parenl actual_parameter_list Parenr { () }

  // Shared property and boolean expressions.
  // Should account for 12 shift/reduce and 17 reduce/reduce conflicts.
  | Bang boolean %prec Vl_bang { () }
  | boolean Amp_amp boolean %prec Vl_amp_amp { () }
  | boolean Bar_bar boolean %prec Vl_bar_bar { () }
  | boolean Dash_gt boolean  { () }
  | boolean Lt_dash_gt boolean  { () }

  | Tildy boolean { () }
  | Plus boolean %prec Vl_uplus { () }
  | Dash boolean %prec Vl_uminus { () }
  | boolean Plus boolean  { () }
  | boolean Dash boolean  { () }
  | boolean Lt boolean  { () }
  | boolean Lt_equal boolean  { () }
  | boolean Gt boolean  { () }
  | boolean Gt_equal boolean  { () }
  | boolean Equal_equal boolean { () }
  | boolean Bang_equal boolean { () }
  | boolean Amp boolean %prec Vl_amp  { () }
  | boolean Carrot boolean  { () }
  | boolean Bar boolean %prec Vl_bar  { () }
  | boolean Question boolean Colon boolean  { () }
  | boolean Union boolean { () }
  ;
*/



%%
(* Post Code *)

