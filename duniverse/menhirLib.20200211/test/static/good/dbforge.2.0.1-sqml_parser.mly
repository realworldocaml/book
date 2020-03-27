(* Original file: dbforge.2.0.1/dbforge-2.0.1/src/sqml/sqml_parser.mly *)
/*********************************************************************************/
/*                Cameleon                                                       */
/*                                                                               */
/*    Copyright (C) 2004-2011 Institut National de Recherche en Informatique     */
/*    et en Automatique. All rights reserved.                                    */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU Library General Public License as            */
/*    published by the Free Software Foundation; either version 2 of the         */
/*    License, or any later version.                                             */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU Library General Public License for more details.                       */
/*                                                                               */
/*    You should have received a copy of the GNU Library General Public          */
/*    License along with this program; if not, write to the Free Software        */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*********************************************************************************/

%{
  open Sqml_sqlstx
  open Sqml_helper_lp
  open Printf
  open Parsing

  let parse_error s = (err ())#error s

  let normal_int = function `int i -> i | _ -> parse_error "absurd integer"

(* #load "parser.cmo" #load "lexer.cmo"; open Parser open Lexer;; *)
%}

/* symbolic tokens */
%token < string > IDENT STRING
%token < [`int of int | `inttoomuch of string] > INTNUM
%token < [`float of int * int * float | `floattoomuch of string] > FLOATNUM /* precision before and after the dot */
%token < Sqml_sqlstx.comparison > COMPARISON

/* literal kw */
%token ALL ANY AS ASC AUTHORIZATION BETWEEN BY
%token CHARACTER CHECK CLOSE COMMIT CONTINUE CREATE CURRENT
%token CURSOR DECIMAL DECLARE DEFAULT DELETE DESC DISTINCT DOUBLE
%token ESCAPE EXISTS FETCH FLOAT FOR FOREIGN FOUND FROM GOTO
%token GRANT GROUP HAVING IN INDICATOR INSERT INTEGER INTO
%token IS KEY LANGUAGE LIKE MODULE NULL NUMERIC OF ON
%token OPEN OPTION ORDER PRECISION PRIMARY PRIVILEGES PROCEDURE
%token PUBLIC REAL REFERENCES ROLLBACK SCHEMA SELECT SET
%token SMALLINT SOME SQLCODE SQLERROR TABLE TO UNION
%token UNIQUE UPDATE USER VALUES VIEW WHENEVER WHERE WITH WORK
%token COBOL FORTRAN PASCAL PLI C ADA OCAML

/* known functions */
%token AVG MIN MAX SUM COUNT


/* punctuation */
%token COLON LPAREN RPAREN COMMA DOT SEMICOLON

/* operators */
%token OR AND NOT PLUS MINUS TIMES DIV

/* eof */
%token EOF

%left OR
%left AND
%left NOT
%left COMPARISON
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%type < Sqml_sqlstx.query > query
%type < Sqml_sqlstx.cmd list > sql_cmd_list
%type < Sqml_sqlstx.cmd > cmd
%type < Sqml_sqlstx.select * Sqml_sqlstx.ordering list > full_select
%type < unit > ugly_eof
%start query sql_cmd_list cmd full_select ugly_eof

/*
%type < Sqml_sqlstx.from > tst
%start tst
*/

%%

/* Database Querying */

/* Section 1 : queries */

ugly_eof : EOF { () }

query: query_exp opt_order_by_clause endsql         { $1, $2 }

full_select: select_exp opt_order_by_clause endsql       { $1, $2 }

endsql:
                EOF                                 { }
        |       SEMICOLON                           { }

/* tst: from_clause SEMICOLON { $1 } */

	/* query expressions */

query_exp:
		query_term                          { $1 }
	|	query_exp UNION query_term          { `union ($1, $3) }
	|	query_exp UNION ALL query_term      { `unionall ($1, $4) }
	;

query_term:
		select_exp                          { `select $1 }
	|	LPAREN query_exp RPAREN             { $2 }
	;

select_exp :
		SELECT opt_all_distinct selection table_exp    { let a, b, c, d = $4 in $2, $3, a, b, c, d }
	;

selection:
		rev_scalar_exp_commalist                { `list (List.rev $1) }
	|	TIMES                               { `star }
	;

table_exp:
		from_clause
		opt_where_clause
		opt_group_by_clause
		opt_having_clause
                                                       { $1, $2, $3, $4 }
	;

from_clause:
		FROM rev_table_ref_commalist            { List.rev $2 }
	;

rev_table_ref_commalist:
		table_ref                           { [$1] }
	|	rev_table_ref_commalist COMMA table_ref { $3 :: $1 }
	;

table_ref:
		table                               { `table $1 }
	|	table AS IDENT                       { `tableas ($1, $3) }
	;

table:
		IDENT                                { $1 }
/* 	|	IDENT DOT IDENT */ /* UNKNOWN : what is this ? */
	;

where_clause:
		WHERE search_condition              { $2 }
	;

opt_where_clause:
		/* empty */                         { None }
	|	where_clause                        { Some $1 }
	;

opt_all_distinct:
		/* empty */                         { `nomod }
	|	ALL                                 { `all }
	|	DISTINCT                            { `distinct }
	;

	/* scalar expressions */

scalar_exp:
		scalar_exp PLUS scalar_exp            { `binop (`plus, $1, $3) }
	|	scalar_exp MINUS scalar_exp           { `binop (`minus, $1, $3) }
	|	scalar_exp TIMES scalar_exp           { `binop (`times, $1, $3) }
	|	scalar_exp DIV scalar_exp             { `binop (`div, $1, $3) }
	|	PLUS scalar_exp %prec UMINUS          { $2 }
	|	MINUS scalar_exp %prec UMINUS         { `uminus $2 }
	|	atom                                  { `atom $1 }
	|	column_ref                            { `column $1 }
	|	function_ref                          { `functioncall $1 }
	|	LPAREN scalar_exp RPAREN              { $2 }
	;

rev_scalar_exp_commalist:
		scalar_exp                            { [$1] }
	|	rev_scalar_exp_commalist COMMA scalar_exp { $3 :: $1 }
	;

atom:
		parameter_ref                         { `parameter $1 }
	|	literal                               { ($1 :> atom) }
	|	USER /* UNKNOWN : what is this ? */   { `user }
	;

parameter_ref:
		parameter                             { `single $1 }
	|       parameter COLON IDENT DOT IDENT       { `single_annotated ($1, $3, $5) }
	|	parameter parameter                   { `couple ($1, $2) } /* UNKNOWN : what is this ? */
	|	parameter INDICATOR parameter         { `indicator ($1, $3) } /* UNKNOWN : what is this ? */
	;

function_ref:
		ammsc LPAREN TIMES RPAREN                 { $1, `star }
	|	ammsc LPAREN DISTINCT column_ref RPAREN   { $1, `distinct $4 }
	|	ammsc LPAREN ALL scalar_exp RPAREN        { $1, `exp (`all, $4) }
	|	ammsc LPAREN scalar_exp RPAREN            { $1, `exp (`nomod, $3) }
	;

ammsc :
                AVG    { `avg }
	|       MIN    { `min }
	|       MAX    { `max }
	|       SUM    { `sum }
	|       COUNT  { `count }
	|       IDENT  { `other $1 }

literal:
		STRING          { `string $1 }
	|	INTNUM          { $1 :> literal }
	;


opt_group_by_clause:
		/* empty */                      { None }
	|	GROUP BY rev_column_ref_commalist    { Some (List.rev $3) }
	;

rev_column_ref_commalist:
		column_ref                             { [$1] }
	|	rev_column_ref_commalist COMMA column_ref  { $3 :: $1 }
	;

column_ref:
		IDENT               { `ref $1 }
	|	IDENT DOT IDENT	   { `refdotref ($1, $3) }
/*	|	IDENT DOT IDENT DOT IDENT */ /* UNKNOWN : what is this ? */
	;


opt_having_clause:
		/* empty */               { None }
	|	HAVING search_condition   { Some $2 }
	;


	/* search conditions */

search_condition:
	|	search_condition OR search_condition   { `cor ($1, $3) }
	|	search_condition AND search_condition  { `cand ($1, $3) }
	|	NOT search_condition                   { `cnot $2 }
	|	LPAREN search_condition RPAREN         { $2 }
	|	predicate                              { `p $1 }
	;

predicate:
		comparison_predicate                   { $1 }
	|	between_predicate                      { $1 }
	|	like_predicate                         { $1 }
	|	test_for_null                          { $1 }
	|	in_predicate                           { $1 }
	|	all_or_any_predicate                   { $1 }
	|	existence_test                         { $1 }
	;

comparison_predicate:
		scalar_exp COMPARISON scalar_exp                       { `comparisonexp ($1, $2, $3) }
	|	scalar_exp COMPARISON LPAREN select_exp RPAREN             { `comparisonselect ($1, $2, $4) }
	;

between_predicate:
		scalar_exp NOT BETWEEN scalar_exp AND scalar_exp       { `between (true, $1, $4, $6) }
	|	scalar_exp BETWEEN scalar_exp AND scalar_exp           { `between (false, $1, $3, $5) }
	;

like_predicate:
		scalar_exp NOT LIKE atom opt_escape                    { `like (true, $1, $4, $5) }
	|	scalar_exp LIKE atom opt_escape                        { `like (false, $1, $3, $4) }
	;

opt_escape:
		/* empty */                                            { None }
	|	ESCAPE atom                                            { Some $2 }
	;

test_for_null:
		column_ref IS NOT NULL                                 { `iscolnull (true, $1) }
	|	column_ref IS NULL                                     { `iscolnull (false, $1) }
	;

in_predicate:
		scalar_exp NOT IN LPAREN select_exp RPAREN                 { `in_select (true, $1, $5) }
	|	scalar_exp IN LPAREN select_exp RPAREN                     { `in_select (false, $1, $4) }
	|	scalar_exp NOT IN LPAREN rev_atom_commalist RPAREN     { `in_atom_list (true, $1, List.rev $5) }
	|	scalar_exp IN LPAREN rev_atom_commalist RPAREN         { `in_atom_list (false, $1, List.rev $4) }
	;

rev_atom_commalist:
		atom                                                   { [$1] }
	|	rev_atom_commalist COMMA atom                          { $3 :: $1 }
	;

all_or_any_predicate:
		scalar_exp COMPARISON any_all_some LPAREN select_exp RPAREN { `allorany ($1, $2, $3, $5) }
	;

any_all_some:
		ANY         { `some }
	|	ALL         { `all }
	|	SOME        { `some }
	;

existence_test:
		EXISTS LPAREN select_exp RPAREN                          { `exists $3 }
	;

parameter:
		COLON IDENT                                              { $2 }
	;

/* Database modifications */

sql_cmd_list:
		cmd endsql                      { [$1] }
	|	cmd SEMICOLON sql_cmd_list      { $1 :: $3 }
	;

/* Section 2 : Inserts */
insert_statement:
		INSERT INTO table opt_column_commalist values_or_select_exp     { $3, $4, $5 }
	;

values_or_select_exp:
		VALUES LPAREN rev_insert_atom_commalist RPAREN                  { `values (List.rev $3) }
	|	select_exp                                                      { `select $1 }
	;

rev_insert_atom_commalist:
		insert_atom                                                     { [ $1 ] }
	|	rev_insert_atom_commalist COMMA insert_atom                     { $3 :: $1 }
	;

insert_atom:
		atom                                                            { `atom $1 }
	|	NULL                                                            { `null }
	;

/* Section 3 : schemas */

	/* Note: other ``cmd:'' rules appear later in the grammar */
cmd:		schema { `schemadef $1 }
	;

	/* schema definition language */
schema:
		CREATE SCHEMA AUTHORIZATION user opt_schema_element_list { $4, $5 }
	;

opt_schema_element_list:
		/* empty */ { [] }
	|	rev_schema_element_list { List.rev $1 }
	;

rev_schema_element_list:
		schema_element                         { [ $1 ] }
	|	rev_schema_element_list schema_element { $2 :: $1 }
	;

schema_element:
		base_table_def        { `tabledef $1 }
	|	view_def              { `viewdef $1 }
	|	privilege_def         { `privdef $1 }
	;

cmd :           schema_element { $1 :> cmd }
        ;

base_table_def:
		CREATE TABLE table LPAREN base_table_element_commalist RPAREN { $3, $5 }
	;

base_table_element_commalist:
		base_table_element { [ $1 ] }
	|	base_table_element_commalist COMMA base_table_element { $1 @ [ $3 ] }
	;

base_table_element:
		column_def                { `columndef $1 }
	|	table_constraint_def      { `tblcnstr $1 }
	;

column:		IDENT { $1 }
	;

column_def:
		column data_type column_def_opt_list { $1, $2, $3 }
	;

		/* data types */
data_type:
		CHARACTER                                                   { `char None}
	|	CHARACTER LPAREN INTNUM RPAREN                              { `char (Some (normal_int $3)) }
	|	NUMERIC                                                     { `numeric `default }
	|	NUMERIC LPAREN INTNUM RPAREN                                { `numeric (`length (normal_int $3)) }
	|	NUMERIC LPAREN INTNUM COMMA INTNUM RPAREN                   { `numeric (`lengthdec (normal_int $3, normal_int $5)) }
	|	DECIMAL                                                     { `decimal `default }
	|	DECIMAL LPAREN INTNUM RPAREN                                { `decimal (`length (normal_int $3)) }
	|	DECIMAL LPAREN INTNUM COMMA INTNUM RPAREN                   { `decimal (`lengthdec (normal_int $3, normal_int $5)) }
	|	INTEGER                                                     { `int }
	|	SMALLINT                                                    { `smallint }
	|	FLOAT                                                       { `float None }
	|	FLOAT LPAREN INTNUM RPAREN                                  { `float (Some (normal_int $3)) }
	|	REAL                                                        { `real }
	|	DOUBLE PRECISION                                            { `doubleprecision }
	;

column_def_opt_list:
		/* empty */ { [] }
	|	column_def_opt_list column_def_opt { $1 @ [$2] }
	;

column_def_opt:
		NOT NULL                                                    { `not_null }
	|	NOT NULL UNIQUE                                             { `not_null_unique }
	|	NOT NULL PRIMARY KEY                                        { `not_null_primary_key }
	|	DEFAULT literal                                             { `default $2 }
	|	DEFAULT NULL                                                { `default_null }
	|	DEFAULT USER                                                { `default_user }
	|	CHECK LPAREN search_condition RPAREN                        { `check $3 }
	|	REFERENCES table                                            { `references ($2, []) }
	|	REFERENCES table LPAREN column_commalist RPAREN             { `references ($2, $4) }
	;

table_constraint_def:
		UNIQUE LPAREN column_commalist RPAREN                       { `unique $3 }
	|	PRIMARY KEY LPAREN column_commalist RPAREN                  { `primkey $4 }
	|	FOREIGN KEY LPAREN column_commalist RPAREN
			REFERENCES table                                    { `foreignkey ($4, $7, []) }
	|	FOREIGN KEY LPAREN column_commalist RPAREN
			REFERENCES table LPAREN column_commalist RPAREN     { `foreignkey ($4, $7, $9) }
	|	CHECK LPAREN search_condition RPAREN                        { `check $3 }
	;


column_commalist:
		column                                                      { [$1] }
	|	column_commalist COMMA column                               { $1 @ [$3] }
	;

opt_column_commalist:
		/* empty */                                                 { [] }
	|	LPAREN column_commalist RPAREN                              { $2 }
	;

user:		IDENT              { $1 }
	;

view_def:
		CREATE VIEW table opt_column_commalist
		AS select_exp opt_with_check_option                         { $3, $4, $6, $7 }
	;

opt_with_check_option:
		/* empty */                                                 { `nocheck }
	|	WITH CHECK OPTION                                           { `check }
	;


privilege_def:
		GRANT privileges ON table TO grantee_commalist
		opt_with_grant_option                                       { $2, $4, $6, $7 }
	;

opt_with_grant_option:
		/* empty */                                                 { `nograntoption }
	|	WITH GRANT OPTION                                           { `grantoption }
	;

privileges:
		ALL PRIVILEGES                                              { `all }
	|	ALL                                                         { `all }
	|	operation_commalist                                         { `some $1 }
	;

operation_commalist:
		operation                                                   { [$1] }
	|	operation_commalist COMMA operation                         { $1 @ [$3] }
	;

operation:
		SELECT                                                      { `select }
	|	INSERT                                                      { `insert }
	|	DELETE                                                      { `delete }
	|	UPDATE opt_column_commalist                                 { `update $2 }
	|	REFERENCES opt_column_commalist                             { `references $2 }
	;


grantee_commalist:
		grantee                                                     { [$1] }
	|	grantee_commalist COMMA grantee                             { $1 @ [$3] }
	;

grantee:
		PUBLIC                                                      { `public }
	|	user                                                        { `user $1 }
	;

	/* module language */
cmd:		module_def                                                  { `moduledef $1 }
	;

module_def:
		MODULE opt_module
		LANGUAGE lang
		AUTHORIZATION user
		opt_cursor_def_list
		procedure_def_list                                          { $2, $4, $6, $7, $8 }
	;

opt_module:
		/* empty */                                                 { None }
	|	sqlmodule                                                   { Some $1 }
	;

sqlmodule:		IDENT { $1 }
	;

lang:
		COBOL                                                       { `cobol }
	|	FORTRAN                                                     { `fortran }
	|	PASCAL                                                      { `pascal }
	|	PLI                                                         { `pli }
	|	C                                                           { `c }
	|	ADA                                                         { `ada }
	|       OCAML                                                       { `ocaml }
	;

opt_cursor_def_list:
		/* empty */                                                 { [] }
	|	cursor_def_list                                             { $1 }
	;

cursor_def_list:
		cursor_def                                                  { [$1] }
	|	cursor_def_list cursor_def                                  { $1 @ [$2] }
	;

cursor_def:
		DECLARE cursor CURSOR FOR query_exp opt_order_by_clause     { $2, $5, $6 }
	;

cursor:		IDENT { $1 }
	;


opt_order_by_clause:
		/* empty */                                                 { [] }
	|	ORDER BY ordering_spec_commalist                            { $3 }
	;

ordering_spec_commalist:
		ordering_spec                                               { [$1] }
	|	ordering_spec_commalist COMMA ordering_spec                 { $1 @ [$3] }
	;

ordering_spec:
		INTNUM opt_asc_desc                                         { `numcolumn (normal_int $1), $2 }
	|	column_ref opt_asc_desc                                     { `column $1, $2 }
	;

opt_asc_desc:
		/* empty */                                                 { None }
	|	ASC                                                         { Some `asc }
	|	DESC                                                        { Some `desc }
	;

procedure_def_list:
		procedure_def                                               { [$1] }
	|	procedure_def_list procedure_def                            { $1 @ [$2] }
	;

procedure_def:
		PROCEDURE procedure parameter_def_list SEMICOLON
		manipulative_statement_list                                 { $2, $3, $5 }
	;

procedure:	IDENT { $1 }
	;

manipulative_statement_list:
		manipulative_statement                                      { [$1] }
	|	manipulative_statement_list manipulative_statement          { $1 @ [$2] }
	;

parameter_def_list:
		parameter_def                                               { [$1] }
	|	parameter_def_list parameter_def                            { $1 @ [$2] }
	;

parameter_def:
		parameter data_type                                         { `par ($1, $2) }
	|	SQLCODE                                                     { `sqlcode }
	;

	/* manipulative statements */

cmd:		manipulative_statement                                      { `manip $1 }
	;

manipulative_statement:
		close_statement                                             { `close $1 }
	|	commit_statement                                            { `commit }
	|	delete_statement_positioned                                 { `delete_pos $1 }
	|	delete_statement_searched                                   { `delete_where $1 }
	|	fetch_statement                                             { `fetch $1 }
	|	insert_statement                                            { `insert $1 }
	|	open_statement                                              { `opencursor $1 }
	|	rollback_statement                                          { `rollback }
	|	select_statement                                            { `select $1 }
	|	update_statement_positioned                                 { `update_pos $1 }
	|	update_statement_searched                                   { `update_where $1 }
	;

close_statement:
		CLOSE cursor                                                { $2 }
	;

commit_statement:
		COMMIT WORK                                                 { () }
	;

delete_statement_positioned:
		DELETE FROM table WHERE CURRENT OF cursor                   { $3, $7 }
	;

delete_statement_searched:
		DELETE FROM table opt_where_clause                          { $3, $4 }
	;

fetch_statement:
		FETCH cursor INTO target_commalist                          { $2, $4 }
	;

open_statement:
		OPEN cursor                                                 { $2 }
	;

rollback_statement:
		ROLLBACK WORK                                               { () }
	;

select_statement:
		SELECT opt_all_distinct selection
		INTO target_commalist
		table_exp                                                   { let a, b, c, d = $6 in ($2, $3, a, b, c, d), $5 }
	;

update_statement_positioned:
		UPDATE table SET assignment_commalist
		WHERE CURRENT OF cursor                                     { $2, $4, $8 }
	;

assignment_commalist:
	|	assignment                                                  { [$1] }
	|	assignment_commalist COMMA assignment                       { $1 @ [$3] }
	;

assignment:
		column COMPARISON scalar_exp                                { if $2 <> `eq
									      then parse_error "assignment expected"
									      else `column_exp ($1, $3) }
	|	column COMPARISON NULL                                      { if $2 <> `eq
									      then parse_error "assignment expected"
									      else `column_null $1 }
	;

update_statement_searched:
		UPDATE table SET assignment_commalist opt_where_clause      { $2, $4, $5 }
	;

target_commalist:
		target                                                      { [$1] }
	|	target_commalist COMMA target                               { $1 @ [$3] }
	;

target:
		parameter_ref                                               { $1 }
	;


	/* embedded condition things */
cmd:		WHENEVER NOT FOUND when_action                              { `when_not_found $4 }
	|	WHENEVER SQLERROR when_action                               { `whenever_sqlerror $3 }
	;

when_action:	GOTO IDENT                                                  { `goto $2 }
	|	CONTINUE                                                    { `continue }
	;

%%
(*


*)
