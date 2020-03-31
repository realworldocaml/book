(*
 * Copyright (c) 2014, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to orse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

%token GT LT NEQ GEQ LEQ EQ EQEQ LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE SEMICOLON COMMA DOT COLON COLONEQ

%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token <string> STRING
%token DOTPOWER POWER PLUS MINUS TIMES DIV DOTPLUS DOTMINUS DOTTIMES DOTDIV
%token EOF

%token ALGORITHM DISCRETE FALSE LOOP PURE AND EACH FINAL MODEL RECORD ANNOTATION ELSE
%token FLOW NOT REDECLARE ASSERT ELSEIF FOR OPERATOR REPLACEABLE BLOCK ELSEWHEN FUNCTION OR RETURN
%token BREAK ENCAPSULATED IF OUTER STREAM CLASS END IMPORT OUTPUT THEN ENUMERATION IMPURE
%token PACKAGE TRUE CONNECTOR EQUATION IN PARAMETER TYPE CONSTANT EXPANDABLE INITIAL PARTIAL WHEN
%token CONSTRAINEDBY EXTENDS INNER PROTECTED WHILE DER EXTERNAL INPUT PUBLIC WITHIN
%token ENDWHEN ENDIF ENDFOR ENDWHILE INITIAL_EQUATION INITIAL_ALGORITHM
%token <string> END_IDENT

%right lowest /* lowest precedence */
%nonassoc IDENT INT FLOAT STRING LPAREN RPAREN RBRACKET LBRACE RBRACE

%left COMMA
%left SEMICOLON
%left COLON
%right Not
%left AND OR
%left GT LT NEQ GEQ LEQ EQEQ
%left PLUS MINUS DOTPLUS DOTMINUS     /* medium precedence */
%right UMinus
%right FUNCTION
%left TIMES DIV DOTTIMES DOTDIV
%left POWER DOTPOWER
%nonassoc below_app
%left app_prec

%left type_mod
%left type_var
%left type_conn
%left type_caus
%left type_array
%left type_proj

%left DOT LBRACKET /* highest precedence */

%{
    open Ast.Flags
    open Syntax
    open Syntax_fragments
    open Utils

    (* cannot open Location due to menhir's Error exception *)
    type 'a loc = 'a Location.loc = {
       txt : 'a;
       loc : Location.t;
    }

    (* merge the two sources of a modelica-style component definition (i.e. the declaration and the component_clause *)
    let declaration_to_def def_type def_options def_constraint = function
        (def_name, None, None, def_if, def_rhs, comment) ->
           { commented = { def_name ; def_type ; def_options ; def_constraint ; def_rhs ; def_if ; } ;
             comment }
      | (def_name, Some(dims), None, def_if, def_rhs, comment) ->
           { commented = { def_name ; def_type = TArray { base_type = def_type ; dims } ;
                           def_options ; def_constraint ; def_rhs ; def_if ; } ;
             comment }
      | (def_name, Some(dims), Some(modification), def_if, def_rhs, comment) ->
           { commented = { def_name ; def_type = TArray { base_type = TMod { mod_type = def_type ; modification } ; dims } ;
                           def_options ; def_constraint ; def_rhs ; def_if ; } ;
             comment }
      | (def_name, None, Some(modification), def_if, def_rhs, comment) ->
           { commented = { def_name ; def_type = TMod { mod_type = def_type ; modification } ;
                           def_options ; def_constraint ; def_rhs ; def_if ; } ;
             comment }

     let mkloc x loc_start loc_end = {txt=x ; loc={Location.loc_start; Location.loc_end; Location.loc_ghost=false } }
%}


%start <Syntax.exp> modelica_expr
%start <Syntax.statement> modelica_stmt
%start <Syntax.equation> modelica_eq
%start <Syntax.texp> modelica_texpr
%start <Syntax.import> modelica_import
%start <Syntax.extend> modelica_extends
%start <Syntax.definition list> modelica_definitions
%start <Syntax.typedef> modelica_type_definition
%start <Syntax.unit_> modelica_stored_definition
%%

modelica_stored_definition : within = option(within_clause) toplevel_defs = list(type_definition_clause) EOF { { within; toplevel_defs } }

modelica_definitions : defs = component_clauses EOF { defs }

modelica_expr: e = expr EOF { e }

modelica_stmt : s = statement EOF { s }

modelica_type_definition : t = type_definition EOF { t }

modelica_eq : eq = equation EOF { eq }

modelica_texpr : texpr = type_expression EOF { texpr }

modelica_import : import = import EOF { import }

modelica_extends : extends = extends EOF { extends }

ident : x=IDENT { mkloc x $startpos $endpos }

str : x=STRING { mkloc x $startpos $endpos }

expr : e = simple_expr { e }
     | IF condition = expr THEN then_ = expr else_if = list(else_if) ELSE else_=expr
       { no_attr (If { condition ; then_ ; else_if ; else_ }) }
     | start = simple_expr COLON first=simple_expr second=option(preceded(COLON, simple_expr))
        { no_attr (Range (match second with Some end_ -> { start; step=Some first; end_ }
                                          | None -> {start; step=None; end_=first} ) )
        }

simple_expr:
  | TRUE { no_attr (Bool(true)) }
  | FALSE { no_attr (Bool(false)) }
  | i = INT
        { no_attr (Int (i)) }
  | f = FLOAT
        { no_attr (Real (f)) }
  | s = STRING
        { no_attr (String(s)) }
  | DOT x = IDENT
        { no_attr (RootIde x)}
  | x = IDENT
        { no_attr (Ide(x)) }
  | LPAREN e = expr RPAREN
        { e }
  | LPAREN RPAREN { no_attr (OutputExpression [None]) }
  | LPAREN e=expr COMMA ps=patterns RPAREN { no_attr (OutputExpression ((Some e)::ps)) }
  | LPAREN COMMA ps=patterns RPAREN { no_attr (OutputExpression (None::ps)) }

  | LBRACE es=array_args RBRACE
        { no_attr (Array es) }
  | lhs = simple_expr LBRACKET indices=separated_nonempty_list(COMMA, expr) RBRACKET
        { no_attr (ArrayAccess { lhs; indices }) }
  | LBRACKET els = separated_nonempty_list(SEMICOLON, separated_nonempty_list(COMMA, expr)) RBRACKET
        { no_attr (MArray els) }
  | FUNCTION e = simple_expr
        { no_attr (ExplicitClosure e) }
  | END { no_attr (End) } %prec END
  | DER { no_attr (Der) }
  | INITIAL { no_attr (Initial) }
  | COLON { no_attr (Colon) }
  | ASSERT { no_attr (Assert) }

  | fun_ = simple_expr LPAREN arguments = function_args RPAREN
        { let (args, named_args) = arguments in no_attr (App { fun_ ; args; named_args }) }

  | left = simple_expr PLUS right = simple_expr
       { no_attr (Plus ( {left ; right} )) }
  | left = simple_expr MINUS right = simple_expr
       { no_attr (Minus ( {left ; right} )) }
  | left = simple_expr TIMES right = simple_expr
       { no_attr (Mul ( {left ; right} )) }
  | left = simple_expr DIV right = simple_expr
       { no_attr (Div ( {left ; right} )) }
  | left = simple_expr POWER right = simple_expr
       { no_attr (Pow ( {left ; right} )) }


  | left = simple_expr DOTPLUS right = simple_expr
       { no_attr (DPlus ( {left ; right} )) }
  | left = simple_expr DOTMINUS right = simple_expr
       { no_attr (DMinus ( {left ; right} )) }
  | left = simple_expr DOTTIMES right = simple_expr
       { no_attr (DMul ( {left ; right} )) }
  | left = simple_expr DOTDIV right = simple_expr
       { no_attr (DDiv ( {left ; right} )) }
  | left = simple_expr DOTPOWER right = simple_expr
       { no_attr (DPow ( {left ; right} )) }

  | left = simple_expr LT right = simple_expr
       { no_attr (Lt ( {left ; right} )) }
  | left = simple_expr GT right = simple_expr
       { no_attr (Gt ( {left ; right} )) }
  | left = simple_expr GEQ right = simple_expr
       { no_attr (Geq ( {left ; right} )) }
  | left = simple_expr LEQ right = simple_expr
       { no_attr (Leq ( {left ; right} )) }
  | left = simple_expr NEQ right = simple_expr
       { no_attr (Neq ( {left ; right} )) }
  | left = simple_expr EQEQ right = simple_expr
       { no_attr (Eq ( {left ; right} )) }

  | left = simple_expr AND right = simple_expr
       { no_attr (And ( {left ; right} )) }
  | left = simple_expr OR right = simple_expr
       { no_attr (Or ( {left ; right} )) }

  | object_ = simple_expr DOT field = IDENT
       { no_attr (Proj { object_ ; field }) }

  | MINUS e = simple_expr { no_attr (UMinus e) } %prec UMinus
  | PLUS e = simple_expr { no_attr (UPlus e) } %prec UMinus
  | DOTMINUS e = simple_expr { no_attr (UDMinus e) } %prec UMinus
  | DOTPLUS e = simple_expr { no_attr (UDPlus e) } %prec UMinus
  | NOT e = simple_expr { no_attr (Not e) } %prec Not


else_if : ELSEIF guard=expr THEN elsethen = expr { {guard; elsethen} }

index_range : IN e = expr { e }

index : variable = ident range = option(index_range) { { variable ; range } }

array_args : es=separated_list(COMMA, expr) { es }
           | exp = expr FOR idxs = separated_nonempty_list(COMMA, index) { [no_attr (Compr { exp ; idxs })] }


function_args : e = expr COMMA fs = function_args { let (args, named_args) = fs in (e::args, named_args) }
              | e = expr { ([e], []) }
              | m = named_function_args { ([], m) }
              | exp = expr FOR idxs = separated_nonempty_list(COMMA, index) { ([no_attr (Compr { exp ; idxs })], []) }

named_argument : argument_name=ident EQ argument=expr { {argument_name ; argument } }

named_function_args : args=separated_nonempty_list (COMMA, named_argument) { args }
                    | { [] }

annotation : ANNOTATION m=class_modification { m }

comment : s=option(str) m=option(annotation) { { annotated_elem=s ; annotation=m} }

statement : s=statement_body comment=comment SEMICOLON { {commented=s ; comment} }

else_statements : ELSE else_ = list(statement) { else_ }
                | { [] }

elseif_statement : ELSEIF guard = expr THEN elsethen=list(statement) { { guard ; elsethen } }

elsewhen_statement : ELSEWHEN guard = expr THEN elsethen=list(statement) { { guard ; elsethen } }

component_reference : x = IDENT { no_attr (Ide x) }
                    | ASSERT { no_attr (Assert) }
                    | DOT x = IDENT { no_attr (RootIde x) }
                    | object_=component_reference DOT field=IDENT { no_attr (Proj { object_ ; field }) }
                    | lhs = component_reference LBRACKET indices=separated_nonempty_list(COMMA, expr) RBRACKET
                                                                                        { no_attr (ArrayAccess { lhs; indices }) }
lexpr : r = component_reference { r }
      | LPAREN ps=patterns RPAREN { no_attr (OutputExpression ps) }

patterns : p=option(lexpr) ps=list(preceded(COMMA, option(lexpr))) { p::ps }

statement_body : procedure=component_reference LPAREN arguments = function_args RPAREN
                 { let (pargs, pnamed_args) = arguments in Call { procedure ; pargs; pnamed_args } }
               | BREAK { Break }

               | RETURN { Return }
               | IF condition=expr THEN then_ = list(statement) else_if = list(elseif_statement) else_ = else_statements ENDIF
                    { IfStmt { condition; then_ ; else_if; else_ } }
               | WHEN condition=expr THEN then_ = list(statement) else_if = list(elsewhen_statement) ENDWHEN
                    { WhenStmt { condition; then_ ; else_if; else_ = []} }
               | FOR idx = list(index) LOOP body=list(statement) ENDFOR { ForStmt { idx; body } }
               | WHILE while_=expr LOOP while_body = list(statement) ENDWHILE { WhileStmt { while_; while_body } }
               | target=lexpr COLONEQ source=expr { Assignment { target; source } }


equation : commented=equation_body comment=comment SEMICOLON { { commented ; comment } }

else_equations : ELSE else_ = list(equation) { else_ }
                | { [] }

elseif_equation : ELSEIF guard = expr THEN elsethen=list(equation) { { guard ; elsethen } }

elsewhen_equation : ELSEWHEN guard = expr THEN elsethen=list(equation) { { guard ; elsethen } }

equation_body : e = simple_expr { ExpEquation e }
              | left = simple_expr EQ right = expr { SimpleEquation { left ; right } }
              | IF condition=expr THEN then_ = list(equation) else_if = list(elseif_equation) else_ = else_equations ENDIF
                   { IfEquation { condition; then_ ; else_if; else_ } }
              | WHEN condition=expr THEN then_ = list(equation) else_if = list(elsewhen_equation) ENDWHEN
                   { WhenEquation { condition; then_ ; else_if; else_ = []} }
              | FOR idx = list(index) LOOP body=list(equation) ENDFOR { ForEquation { idx; body } }

variability : CONSTANT { Constant }
            | PARAMETER { Parameter }
            | DISCRETE { Discrete }

connectivity : FLOW { Flow }
             | STREAM { Stream }

causality : INPUT { Input }
          | OUTPUT { Output }

type_expression :
                | x = separated_nonempty_list(DOT, ident) { TName x }
                | DOT x = separated_nonempty_list(DOT, ident) { TRootName x }
                | flag=variability flagged=type_expression { TVar { flag ; flagged } } %prec type_var
                | flag=causality flagged=type_expression { TCau { flag ; flagged } } %prec type_caus
                | flag=connectivity flagged=type_expression { TCon { flag ; flagged } } %prec type_conn
                | base_type = type_expression dims = array_subscripts { TArray { base_type ; dims } } %prec type_array
                | mod_type = type_expression modification = class_modification { TMod { mod_type ; modification } } %prec type_mod

class_modification : LPAREN m=modification_arguments_head RPAREN { m }

modification_arguments_head : m = modification_arguments { m }
                            | { { types = [] ; components = [] ; modifications = [] } }

modification_arguments : REDECLARE redecl_each=flag(EACH) type_final=flag(FINAL) type_replaceable=flag(REPLACEABLE)
                         partial=flag(PARTIAL) sort = type_sort
                         td_name=ident EQ type_exp = type_expression comment=comment cns = option(constraining_clause)
                         rest=modification_arguments_tail
                         { { rest with types = {
                                    redecl_each ;
                                    redecl_type = { commented = { td_name ; sort ;
                                                                  type_options = { no_type_options with partial ;
                                                                                   type_final; type_replaceable } ;
                                                                  type_exp ; cns} ;
                                                    comment }
                                    } :: rest.types } }
                       | redecl_each=flag(EACH) type_final=flag(FINAL) REPLACEABLE partial=flag(PARTIAL) sort = type_sort
                         td_name=ident EQ type_exp = type_expression comment=comment cns = option(constraining_clause)
                         rest=modification_arguments_tail
                         { { rest with types = {
                                    redecl_each ;
                                    redecl_type = { commented = { td_name ; sort ;
                                                                  type_options = { no_type_options with partial ;
                                                                                   type_final; type_replaceable=true } ;
                                                                  type_exp ; cns} ;
                                                    comment }
                                    } :: rest.types } }
                       | REDECLARE each=flag(EACH) final=flag(FINAL) replaceable=flag(REPLACEABLE) def=mod_component_clause
                         rest=modification_arguments_tail
                         { {rest with components =
                             { each ; def = { def with commented =
                                                       { def.commented with def_options =
                                                                            {def.commented.def_options with final; replaceable} };
                                            }
                         }::rest.components} }
                       | each=flag(EACH) final=flag(FINAL) REPLACEABLE def=mod_component_clause
                         rest=modification_arguments_tail
                         { {rest with components =
                             { each ; def = { def with commented =
                                                       { def.commented with def_options =
                                                                            {def.commented.def_options with final; replaceable=true} };
                                            }
                         }::rest.components} }
                       | mod_each=flag(EACH) mod_final=flag(FINAL) mod_name = separated_nonempty_list(DOT, ident)
                         mod_value=option(modification) comment=comment
                         rest=modification_arguments_tail
                         { let m = {commented={mod_name;mod_final;mod_each;mod_value};comment} in
                               { rest with modifications = m::rest.modifications }
                         }

modification : EQ e=expr | COLONEQ e=expr { Rebind e }
             | m=class_modification { Nested m }
             | nested=class_modification EQ new_value=expr { NestedRebind {nested;new_value} }

modification_arguments_tail : COMMA m = modification_arguments { m }
                            | { { types = [] ; components = [] ; modifications = [] } }

mod_component_clause : scope=scope def_type = type_expression component=declaration
                       def_constraint=option(constraining_clause)
                       { declaration_to_def def_type {no_def_options with scope} def_constraint component }

import : IMPORT name=separated_nonempty_list(DOT, ident) comment = comment { { commented = Unnamed name ; comment } }
       | IMPORT local=ident EQ global=separated_nonempty_list(DOT, ident) comment = comment
         { { commented = NamedImport {global;local} ; comment } }
       | IMPORT name=separated_nonempty_list(DOT, ident) DOTTIMES comment = comment { { commented = UnqualifiedImport name ; comment } }

extends : EXTENDS ext_type = type_expression ext_annotation=option(annotation) { { ext_type ; ext_annotation } }

flag (F) : F { true } | { false }

scope : INNER { Inner }
      | OUTER { Outer }
      | INNER OUTER { InnerOuter }
      | { Local }

type_prefix : final = flag(FINAL) replaceable = flag(REPLACEABLE) scope=scope
                { { final ; scope ; replaceable } }

array_subscripts : LBRACKET dims = separated_list(COMMA, expr) RBRACKET { dims }

decl_condition : IF cond=expr { cond }

%inline
binder : EQ {} | COLONEQ {}

decl_modification : m=option(class_modification) e=option(preceded(binder, expr)) { (m, e) }

declaration : x = IDENT dims = option(array_subscripts) m=decl_modification cond=option(decl_condition) comment=comment
              { let (modification, rhs) = m in (x, dims, modification, cond, rhs, comment) }

constraining_clause : CONSTRAINEDBY commented= type_expression comment=comment { { commented ; comment } }

component_clauses : defs = component_clause { defs }
                  | defs = component_clause SEMICOLON defs2 = component_clauses { List.append defs defs2 }

component_clause : def_options = type_prefix def_type = type_expression components=separated_nonempty_list(COMMA, declaration)
                   def_constraint=option(constraining_clause)
                     { List.map (declaration_to_def def_type def_options def_constraint) components }

type_sort : CLASS { Class }
           | PACKAGE {Package}
           | MODEL { Model } | BLOCK { Block } | CONNECTOR { Connector } | EXPANDABLE CONNECTOR { ExpandableConnector }
           | RECORD { Record } | FUNCTION { Function } | TYPE { Type } | OPERATOR { Operator } | OPERATOR RECORD { OperatorRecord }
           | OPERATOR FUNCTION { OperatorFunction }

typedef_prefix : type_final = flag (FINAL) type_replaceable = flag(REPLACEABLE)
                 encapsulated = flag(ENCAPSULATED) partial=flag(PARTIAL)
                 { { type_final ; type_replaceable ; encapsulated ; partial } }


enum_literal : commented=IDENT comment=comment { { commented ; comment } }

composition_annotation : a = annotation SEMICOLON { a }

type_definition : type_options = typedef_prefix sort = type_sort td_name=ident EQ type_exp = type_expression
                  comment=comment cns = option(constraining_clause)
                  { { commented = Short { td_name ; sort ; type_options ; type_exp ; cns} ;  comment } }

                | type_options = typedef_prefix sort = type_sort td_name=ident annotated_elem=option(str) type_exp=composition
                  annotation=option(composition_annotation) end_name=END_IDENT cns = option(constraining_clause)
                  { { commented = Composition { td_name ; sort ; type_options ; type_exp ; cns} ;  comment = {annotated_elem;annotation}}}

                | type_options = typedef_prefix sort = type_sort EXTENDS td_name=ident modification=option(class_modification)
                  annotated_elem=option(str) composition=composition annotation=option(composition_annotation) end_name=END_IDENT
                  cns = option(constraining_clause)
                  { { commented = Extension { td_name ; sort ; type_options ; type_exp=(composition,modification) ; cns} ;
                      comment = {annotated_elem;annotation}}}

                | type_options = typedef_prefix sort = type_sort td_name=ident EQ ENUMERATION LPAREN type_exp=separated_nonempty_list(COMMA, enum_literal) RPAREN comment = comment cns = option(constraining_clause)
                  { { commented = Enumeration { td_name ; sort ; type_options ; type_exp ; cns} ;  comment } }

                | type_options = typedef_prefix sort = type_sort td_name=ident EQ ENUMERATION LPAREN COLON RPAREN comment = comment cns = option(constraining_clause)
                  { { commented = OpenEnumeration { td_name ; sort ; type_options ; type_exp = () ; cns} ;  comment } }

                | type_options = typedef_prefix sort = type_sort td_name=ident EQ DER LPAREN der_name=separated_nonempty_list(DOT, ident)
                  COMMA idents=separated_nonempty_list(COMMA, ident) RPAREN comment = comment cns = option(constraining_clause)
                  { { commented = DerSpec { td_name ; sort ; type_options ; type_exp = {der_name;idents} ; cns} ;  comment } }

composition : c = public_composition_elements { c }

equation_section : equation=equation rest=equation_section
                     { {rest with cargo = { rest.cargo with equations = equation::rest.cargo.equations } } }
                 | rest = end_of_section { rest }

initial_equation_section : equation=equation rest=initial_equation_section
                             { {rest with cargo = { rest.cargo with initial_equations = equation::rest.cargo.initial_equations } } }
                         | rest = end_of_section { rest }

algorithm : stmts=nonempty_list(statement) { stmts }

algorithm_section : alg=algorithm rest=end_of_section
                      { {rest with cargo = { rest.cargo with algorithms = alg::rest.cargo.algorithms } } }
                  | rest = end_of_section { rest }

initial_algorithm_section : alg=algorithm rest=end_of_section
                              { {rest with cargo = { rest.cargo with initial_algorithms = alg::rest.cargo.initial_algorithms } } }
                          | rest = end_of_section { rest }

%inline end_of_section :   PUBLIC rest = public_composition_elements { rest }
                         | PROTECTED rest = public_composition_elements { rest }
                         | rest = cargo_sections { rest }

cargo_sections : EQUATION rest = equation_section
                { rest }
            | INITIAL_EQUATION rest = initial_equation_section
                { rest }
            | ALGORITHM rest = algorithm_section
                { rest }
            | INITIAL_ALGORITHM rest = initial_algorithm_section
                { rest }
            | external_ = option ( composition_external ) { { empty_composition with cargo = { empty_behavior with external_ } } }

public_composition_elements :
              import = import SEMICOLON rest = public_composition_elements
                { {rest with imports = import::rest.imports} }
            | extend = extends SEMICOLON rest = public_composition_elements
                { {rest with public = { rest.public with extensions = extend::rest.public.extensions } } }
            | defs = component_clause SEMICOLON rest = public_composition_elements
                { {rest with public = { rest.public with defs = defs @ rest.public.defs } } }
            | REDECLARE defs = component_clause SEMICOLON rest = public_composition_elements
                { {rest with public = { rest.public with redeclared_defs = defs @ rest.public.redeclared_defs } } }
            | typedef = type_definition SEMICOLON rest = public_composition_elements
                { {rest with public = { rest.public with typedefs=typedef::rest.public.typedefs} } }
            | REDECLARE typedef = type_definition SEMICOLON rest = public_composition_elements
                { {rest with public = { rest.public with redeclared_types=typedef::rest.public.redeclared_types} } }
            | rest = cargo_sections { rest }
            | PROTECTED rest = protected_composition_elements { rest }
            | PUBLIC rest = public_composition_elements { rest }

protected_composition_elements :
              import = import SEMICOLON rest = protected_composition_elements
                { {rest with imports = import::rest.imports} }
            | extend = extends SEMICOLON rest = protected_composition_elements
                { {rest with protected = { rest.protected with extensions = extend::rest.protected.extensions } } }
            | defs = component_clause SEMICOLON rest = protected_composition_elements
                { {rest with protected = { rest.protected with defs = defs @ rest.protected.defs } } }
            | REDECLARE defs = component_clause SEMICOLON rest = protected_composition_elements
                { {rest with protected = { rest.protected with redeclared_defs = defs @ rest.protected.redeclared_defs } } }
            | typedef = type_definition SEMICOLON rest = protected_composition_elements
                { {rest with protected = { rest.protected with typedefs=typedef::rest.protected.typedefs} } }
            | REDECLARE typedef = type_definition SEMICOLON rest = protected_composition_elements
                { {rest with protected = { rest.protected with redeclared_types=typedef::rest.protected.redeclared_types} } }
            | rest = cargo_sections { rest }
            | PROTECTED rest = protected_composition_elements { rest }
            | PUBLIC rest = public_composition_elements { rest }

composition_external :
            | EXTERNAL lang=STRING lhs=external_lhs ext_ident=IDENT
              LPAREN ext_args = separated_list(COMMA, expr) RPAREN annotation=option(annotation) SEMICOLON
                { { annotated_elem = {lang ; ext_ident; ext_lhs=Some lhs; ext_args}; annotation} }
            | EXTERNAL lang=STRING ext_ident=IDENT
              LPAREN ext_args = separated_list(COMMA, expr) RPAREN annotation=option(annotation) SEMICOLON
                { { annotated_elem = {lang ; ext_ident; ext_lhs=None; ext_args}; annotation} }


external_lhs : e=component_reference EQ { e }

type_definition_clause : td=type_definition SEMICOLON { td }

within_clause : WITHIN name=separated_list(DOT, ident) SEMICOLON { name }

