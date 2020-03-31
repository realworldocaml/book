/*
 *  Oxford Oberon-2 compiler
 *  parser.mly
 *  Copyright (C) J. M. Spivey 1995, 1998
 */

%{
open Symtab
open Dict
open Tree
open Eval
open Error
open Print

let rcsid = "$Id: obc.mly,v 1.1 2005/08/23 11:15:14 fpottier Exp $"
%}

%token <Symtab.ident>	IDENT
%token <Symtab.op>	MULOP ADDOP RELOP
%token <int32>		NUMBER
%token <float>		FLOCON DBLCON
%token <char>		CHAR
%token <string> 	STRING DECIMAL

/* punctuation */
%token			SEMI DOT COLON LPAR RPAR COMMA SUB BUS LBRACE RBRACE
%token			STAR UPARROW EQUAL MINUS PLUS ASSIGN VBAR DOTDOT
%token			BADTOK

/* keywords */
%token			ARRAY BEGIN CONST DO ELSE ELSIF END IF IMPORT IS OF
%token			FOR MODULE PROCEDURE RECORD REPEAT RETURN THEN TO TYPE
%token			UNTIL VAR WHILE NOT POINTER NIL WITH
%token			CASE LOOP EXIT BY LOCAL ABSTRACT

/* operator priorities */

%right			error
%right			SEMI
%right			IF FOR REPEAT RETURN WHILE WITH CASE LOOP EXIT
%right			COMMA
%right			ADDOP RELOP EQUAL MINUS PLUS IS
%right	    		MULOP STAR
%right			IDENT LPAR DOT SUB UPARROW
%right			RPAR BUS

%type <Tree.program>	program
%start			program

%{
let parse_error msg =
  syn_error "$ at token '$'" [fStr msg; fToken]

let parse_error2 msg loc2 =
  syn_error2 "$ at token '$'" [fStr msg; fToken] loc2

let lloc () = (symbol_start (), symbol_end ())
let rloc n = (rhs_start n, rhs_end n)

let check_end bx ex loc =
  let ok = match ex with Some s -> s = bx | None -> false in
  if not ok then
    sem_error "expected identifier '$' after END" [fId bx] loc

let check_modname x =
  if x <> !current then
    syn_error "module name does not match file name" []

let make_if arms elsept =
  let build s1 (cond, s2) = makeStmt (IfStmt (cond, s2, s1), no_loc) in
  let s = List.fold_left build elsept arms in
  s.s_guts

(* make_call -- add empty params if needed in procedure call *)
let make_call e =
  match e.e_guts with
      FuncCall _ -> e
    | _ -> makeExpr (FuncCall (e, []), e.e_loc)

let fix e =
  match e.e_guts with
      Decimal s ->
	makeExpr (Const (IntVal (Int32.of_string s), numtype), e.e_loc)
    | _ -> e

let neg e =
  match e.e_guts with
      Decimal s ->
	Const (IntVal (Int32.of_string ("-" ^ s)), numtype)
    | _ ->
	Monop (Uminus, e)

let mkExpr e = makeExpr (e, lloc ())
let mkTypexpr tx = makeTypexpr (tx, lloc ())
%}

%%

program :
    MODULE modname semi imports block opt_ident DOT
	{ check_end $2.x_name $6 (rloc 6);
	  Module ($2, $4, $5, ref []) } ;

modname :
    name			{ check_modname $1.x_name; $1 }

imports :
    import			{ [$1] }
  | imports COMMA import	{ $1 @ [$3] } ;

import :
    name			{ ($1, $1.x_name, ref 0) }
  | name ASSIGN IDENT		{ ($1, $3, ref 0) } ;

block :
    decls body END		{ Block ($1, $2, ref 0) } ;

body :
    /* empty */			{ makeStmt (SkipStmt, no_loc) }
  | BEGIN stmts			{ $2 } ;

decls :
    /* empty */			{ [] }
  | decls decl			{ $1 @ $2 } ;

decl :
    CONST const_decls		{ $2 }
  | VAR var_decls		{ $2 }
  | TYPE type_decls		{ $2 }
  | proc			{ [$1] }
  | error SEMI 			{ [] } ;

const_decls :
    const_decl			{ [$1] }
  | const_decl const_decls	{ $1 :: $2 } ;

const_decl :
    defid EQUAL expr semi	{ ConstDecl ($1, $3) } ;

type_decls :
    type_decl			{ [$1] }
  | type_decl type_decls	{ $1 :: $2 } ;

type_decl :
    defid EQUAL typexpr semi 	{ TypeDecl ($1, $3) } ;

var_decls :
    var_decl			{ [$1] }
  | var_decl var_decls		{ $1 :: $2 } ;

var_decl :
    defids COLON typexpr semi	{ VarDecl (VarDef, $1, $3) } ;

proc :
    PROCEDURE defid params semi block opt_ident semi
      { check_end $2.x_name $6 (rloc 6);
        ProcDecl (Procedure, $2, $3, $5) }
  | PROCEDURE receiver defid params semi block opt_ident semi
      { check_end $3.x_name $7 (rloc 7);
        let (Heading (ps, r)) = $4 in
        ProcDecl (Method, $3, Heading ($2::ps, r), $6) }
  | ABSTRACT PROCEDURE receiver defid params semi
      { let (Heading (ps, r)) = $5 in
        ProcDecl (AbsMeth, $4, Heading ($3::ps, r), NoBlock) }
  | PROCEDURE defid params IS STRING semi
      { PrimDecl ($2, $3, $5) }
  | PROCEDURE error block opt_ident semi
      { DummyDecl } ;

receiver :
    LPAR defid COLON typename RPAR
      { VarDecl(ParamDef, [$2], $4) }
  | LPAR VAR defid COLON typename RPAR
      { VarDecl(VParamDef, [$3], $5) } ;

params :
    /* empty */			{ Heading ([], None) }
  | LPAR RPAR result		{ Heading ([], $3) }
  | LPAR formals RPAR result	{ Heading ($2, $4) } ;

formals :
    formal			{ [$1] }
  | formal semi formals		{ $1 :: $3 } ;

formal :
    defids COLON typexpr	{ VarDecl (ParamDef, $1, $3) }
  | VAR defids COLON typexpr	{ VarDecl (VParamDef, $2, $4) } ;

result :
    /* empty */			{ None }
  | COLON qualid		{ Some $2 } ;

opt_ident :
    /* empty */			{ None }
  | IDENT			{ Some $1 } ;

/* This rather complicated syntax for 'stmts' is designed to cope
gracefully with missing and duplicated semicolons.  The nonterminal
'stmts_a' generates sequences that (if non-empty) end with a
semicolon, and 'stmts_b' generates non-empty sequences that do not end
with a semicolon.  Missing semicolons are inserted before any
statement that begins with a keyword.

The salient fact is that the parser ends up with two states, one
(linked to stmts_a) where it has sen a semicolon and is ready to see
the next statement, and another (linked to stmts_b) where it needs to
see or insert a semicolon before continuing the sequence. */

stmts :
    stmts_a			{ makeStmt (SeqStmt (List.rev $1), lloc ()) }
  | stmts_b			{ makeStmt (SeqStmt (List.rev $1), lloc ()) } ;

stmts_a :
    /* empty */			{ [] }
  | stmts_a SEMI		{ $1 }
  | stmts_b SEMI		{ $1 }
  | stmts_b error SEMI		{ $1 } ;

stmts_b :
    stmts_a stmt0		{ makeStmt ($2, rloc 2) :: $1 }
  | stmts_a stmt1		{ makeStmt ($2, rloc 2) :: $1 }
  | stmts_b missing stmt1	{ makeStmt ($3, rloc 3) :: $1 } ;

missing :
    /* empty */ %prec error	{ parse_error "missing ';'" } ;

stmt0 :
    designator ASSIGN expr	{ Assign ($1, $3) }
  | designator			{ ProcCall (make_call $1) } ;

stmt1 :
    RETURN 			{ Return None }
  | RETURN expr			{ Return (Some $2) }
  | if_chain END 		{ make_if $1 (makeStmt (SkipStmt, no_loc)) }
  | if_chain ELSE stmts END 	{ make_if $1 $3 }
  | CASE expr OF case_arms else_part END  { CaseStmt ($2, $4, $5) }
  | CASE error case_arms else_part END    { ErrStmt }
  | WHILE expr DO stmts END	{ WhileStmt ($2, $4) }
  | WHILE error stmts END	{ ErrStmt }
  | REPEAT stmts UNTIL expr	{ RepeatStmt ($2, $4) }
  | LOOP stmts END		{ LoopStmt $2 }
  | EXIT			{ ExitStmt }
  | FOR designator ASSIGN expr TO expr by_part DO stmts END
      { ForStmt ($2, $4, $6, $7, $9, ref dummy_def) }
  | WITH with_branches else_part END
      { WithStmt ($2, $3) }
  | LOCAL decls body END	{ LocalStmt ($2, $3) }
  | error			{ ErrStmt } ;

/* We make if_chain be left recursive for better error recovery. Note
   that the list of arms is constructed in reverse order. */

if_chain :
    IF expr THEN stmts		{ [($2,$4)] }
  | IF error stmts		{ [] }
  | if_chain ELSIF expr THEN stmts  { ($3,$5) :: $1 }
  | if_chain ELSIF error stmts	{ $1 } ;

else_part :
    /* empty */			{ None }
  | ELSE stmts			{ Some $2 } ;

case_arms :
    case_arm			{ $1 }
  | case_arm VBAR case_arms	{ $1 @ $3 } ;

case_arm :
    /* EMPTY */			{ [] }
  | elements COLON stmts	{ [($1, $3)] };

elements :
    element			{ [$1] }
  | element COMMA elements	{ $1 :: $3 } ;

element :
    expr			{ Single $1 }
  | expr DOTDOT expr		{ Range ($1, $3) } ;

with_branches :
    with_branch			{ [$1] }
  | with_branch VBAR with_branches	{ $1 :: $3 } ;

with_branch :
    name COLON qualid DO stmts	{ ($1, $3, $5) } ;

by_part :
    /* empty */			{ mkExpr (Const (IntVal Int32.one, numtype)) }
  | BY expr			{ $2 } ;

expr :
    simple %prec error		{ $1 }
  | simple RELOP simple		{ mkExpr (Binop ($2, $1, $3)) }
  | simple EQUAL simple		{ mkExpr (Binop (Eq, $1, $3)) }
  | simple IS qualid		{ mkExpr (TypeTest ($1, $3)) } ;

simple :
    term %prec error		{ fix $1 }
  | PLUS term			{ mkExpr (Monop (Uplus, fix $2)) }
  | MINUS term			{ mkExpr (neg $2) }
  | simple PLUS term		{ mkExpr (Binop (Plus, $1, fix $3)) }
  | simple MINUS term		{ mkExpr (Binop (Minus, $1, fix $3)) }
  | simple ADDOP term		{ mkExpr (Binop ($2, $1, fix $3)) } ;

term :
    factor			{ $1 }
  | term MULOP factor		{ mkExpr (Binop ($2, fix $1, fix $3)) }
  | term STAR factor		{ mkExpr (Binop (Times, fix $1, fix $3)) } ;

factor :
    NUMBER			{ mkExpr (Const (IntVal $1, numtype)) }
  | DECIMAL			{ mkExpr (Decimal $1) }
  | FLOCON			{ mkExpr (Const (FloVal $1, realtype)) }
  | DBLCON			{ mkExpr (Const (FloVal $1, longreal)) }
  | CHAR			{ mkExpr (Const (IntVal (Int32.of_int
					    (int_of_char $1)), character)) }
  | STRING			{ mkExpr (String (save_string $1,
					    String.length $1)) }
  | NIL				{ mkExpr Nil }
  | designator %prec error	{ $1 }
  | LBRACE RBRACE		{ mkExpr (Set []) }
  | LBRACE elements RBRACE	{ mkExpr (Set $2) }
  | NOT factor			{ mkExpr (Monop (Not, fix $2)) }
  | LPAR expr RPAR		{ $2 }
  | LPAR expr %prec error	{ parse_error2 "mismatched brackets" (rloc 1);
				  raise Parse_error } ;

designator :
    name			{ mkExpr (Name $1) }
  | designator UPARROW		{ mkExpr (Deref $1) }
  | designator SUB exprs BUS	{ let sub a i = mkExpr (Sub (a, i)) in
				  List.fold_left sub $1 $3 }
  | designator SUB exprs %prec error
      { parse_error2 "mismatched brackets" (rloc 2);
	raise Parse_error }
  | designator DOT name		{ mkExpr (Select ($1, $3)) }
  | designator actuals		{ mkExpr (FuncCall ($1, $2)) } ;

actuals :
    LPAR RPAR			{ [] }
  | LPAR exprs RPAR		{ $2 }
  | LPAR exprs %prec error	{ parse_error2
				    "mismatched brackets in procedure call"
				    (rloc 1);
				  raise Parse_error } ;

exprs :
    expr %prec error		{ [$1] }
  | expr COMMA exprs		{ $1 :: $3 } ;

typexpr :
    typename			{ $1 }
  | LPAR defids RPAR		{ mkTypexpr (Enum $2) }
  | POINTER TO typexpr		{ mkTypexpr (Pointer $3) }
  | ARRAY exprs OF typexpr	{ let array n t = mkTypexpr (Array (n, t)) in
				  List.fold_right array $2 $4 }
  | ARRAY OF typexpr		{ mkTypexpr (Flex $3) }
  | absmark RECORD parent fields END
				{ mkTypexpr (Record ($1, $3, $4)) }
  | PROCEDURE params		{ mkTypexpr (Proc $2) } ;

typename :
    qualid			{ mkTypexpr (TypeName $1) } ;

absmark :
    /* empty */			{ false }
  | ABSTRACT			{ true } ;

parent :
    /* empty */			{ None }
  | LPAR typename RPAR		{ Some $2 } ;

fields :
    fieldlist			{ $1 }
  | fieldlist SEMI fields	{ $1 @ $3 } ;

fieldlist :
    /* empty */			{ [] }
  | defids COLON typexpr	{ [VarDecl (FieldDef, $1, $3)] } ;

qualid :
    IDENT %prec DOT		{ makeName (!current, $1, lloc ()) }
  | IDENT DOT IDENT		{ makeName ($1, $3, lloc ()) };

name :
    IDENT			{ makeName (!current, $1, lloc ()) } ;

defids :
    defid			{ [$1] }
  | defid COMMA defids		{ $1 :: $3 } ;

defid :
    IDENT export		{ makeDefId ($1, $2, rloc 1) } ;

export :
    /* empty */			{ Private }
  | MINUS			{ ReadOnly }
  | STAR			{ Visible } ;

semi :
    SEMI			{ () }
  | COMMA			{ parse_error "expected ';'" }
  | /* empty */ %prec error	{ parse_error "missing ';'" } ;

