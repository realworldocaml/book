/*  Copyright 2004 INRIA  */

%{
Version.add "$Id: parser.mly,v 1.14 2004/09/28 13:12:58 doligez Exp $";;

open Expr;;
open Phrase;;

let rec mk_quant q vs body =
  match vs with
  | [] -> body
  | h::t -> q (h, "", mk_quant q t body)
;;

let rec myfold f e el =
  match el with
  | [] -> e
  | h::t -> f (e, myfold f h t)
;;

let mkand e el = myfold eand e el;;
let mkor e el = myfold eor e el;;
let mkimply e el = myfold eimply e el;;
let mkequiv e el = myfold eequiv e el;;

let hyp_counter = ref 0;;

%}

%token OPEN
%token CLOSE
%token <string> IDENT
%token <string> STRING
%token <int> INT
%token DEF
%token GOAL
%token NOT
%token AND
%token OR
%token IMPLY
%token EQUIV
%token TRUE
%token FALSE
%token ALL
%token EX
%token TAU
%token EQUAL
%token EOF

%token INCLUDE
%token DOT
%token INPUT_CLAUSE
%token INPUT_FORMULA
%token LBRACKET
%token RBRACKET
%token <string> LIDENT
%token <string> UIDENT
%token POSITIVE
%token NEGATIVE
%token COMMA
%token COLON
%token RIMPLY
%token XOR
%token NOR
%token NAND

%token TOBE
%token QED
%token BY
%token BYDEF
%token COLONEQUAL
%token ARROW
%token FUNARROW
%token DOUBLEARROW
%token FORALL
%token LET
%token IN
%token FUN
%token TILDE
%token SLASHBACKSLASH
%token BACKSLASHSLASH
%token IF
%token THEN
%token ELSE
%token LOCAL
%token <string> BEGINPROOF
%token ENDPROOF

/* these precedences are mostly for coq syntax */
%nonassoc ELSE
%nonassoc forall
%right ARROW
%nonassoc DOUBLEARROW
%right BACKSLASHSLASH
%right SLASHBACKSLASH
%nonassoc TILDE
%nonassoc EQUAL

%start theory
%type <Phrase.phrase list> theory

%start phrase
%type <Phrase.phrase> phrase

%start tpfile
%type <Phrase.tpphrase list> tpfile

%start coqfile
%type <string * Phrase.phrase list> coqfile

%%

/* Native LISP-like syntax */

theory:
  | EOF               { [] }
  | phrase theory     { $1 :: $2 }
;

phrase:
  | DEF IDENT OPEN ident_list CLOSE expr { Def (DefReal ($2, $4, $6)) }
  | int_opt hyp_name expr                { Hyp ($2, $3, $1) }
  | GOAL expr                            { Globals.goal_found := true;
                                           Hyp ("_Zgoal", enot $2, 0) }
;

expr:
  | IDENT                                { evar $1 }
  | OPEN IDENT expr_list CLOSE           { eapp ($2, $3) }
  | OPEN NOT expr CLOSE                  { enot ($3) }
  | OPEN AND expr expr_list CLOSE        { mkand $3 $4 }
  | OPEN OR expr expr_list CLOSE         { mkor $3 $4 }
  | OPEN IMPLY expr expr_list CLOSE      { mkimply $3 $4 }
  | OPEN EQUIV expr expr_list CLOSE      { mkequiv $3 $4 }
  | OPEN TRUE CLOSE                      { etrue }
  | TRUE                                 { etrue }
  | OPEN FALSE CLOSE                     { efalse }
  | FALSE                                { efalse }
  | OPEN ALL lambda CLOSE                { eall $3 }
  | OPEN EX lambda CLOSE                 { eex $3 }
  | OPEN TAU lambda CLOSE                { etau $3 }
  | OPEN EQUAL expr expr CLOSE           { eapp ("=", [$3; $4]) }
;

expr_list:
  | expr expr_list     { $1 :: $2 }
  | /* empty */        { [] }
;

lambda:
  | OPEN OPEN IDENT STRING CLOSE expr CLOSE      { (evar $3, $4, $6) }
  | OPEN OPEN IDENT CLOSE expr CLOSE             { (evar $3, "", $5) }
;

ident_list:
  | /* empty */       { [] }
  | IDENT ident_list  { evar $1 :: $2 }
;

int_opt:
  | /* empty */       { 1 }
  | INT               { $1 }
;

hyp_name:
  | /* empty */       { incr hyp_counter; Printf.sprintf "_hyp%d" !hyp_counter }
  | STRING            { incr hyp_counter; $1 }

/* TPTP syntax */

tpfile:
  | EOF               { [] }
  | tpphrase tpfile   { $1 :: $2 }
;
tpphrase:
  | INCLUDE OPEN STRING CLOSE DOT  { Phrase.Include $3 }
  | INPUT_FORMULA OPEN LIDENT COMMA LIDENT COMMA tpformula CLOSE DOT
                                   { Phrase.Formula ($3, $5, $7) }
;
tpexpr:
  | UIDENT                                 { evar ($1) }
  | LIDENT tparguments                     { eapp ($1, $2) }
  | EQUAL OPEN tpexpr COMMA tpexpr CLOSE   { eapp ("=", [$3; $5]) }
;
tparguments:
  | OPEN tpexpr_list CLOSE         { $2 }
  | /* empty */                    { [] }
;
tpexpr_list:
  | tpexpr COMMA tpexpr_list       { $1 :: $3 }
  | tpexpr                         { [$1] }
;
tpformula:
  | tpatom                         { $1 }
  | tpatom AND tpformula           { eand ($1, $3) }
  | tpatom OR tpformula            { eor ($1, $3) }
  | tpatom IMPLY tpformula         { eimply ($1, $3) }
  | tpatom EQUIV tpformula         { eequiv ($1, $3) }
  | tpatom RIMPLY tpformula        { eimply ($3, $1) }
  | tpatom XOR tpformula           { enot (eequiv ($1, $3)) }
  | tpatom NOR tpformula           { enot (eor ($1, $3)) }
  | tpatom NAND tpformula          { enot (eand ($1, $3)) }
;
tpatom:
  | ALL LBRACKET tpvar_list RBRACKET COLON tpatom
                                   { mk_quant eall $3 $6 }
  | EX LBRACKET tpvar_list RBRACKET COLON tpatom
                                   { mk_quant eex $3 $6 }
  | NOT tpatom                     { enot ($2) }
  | OPEN tpformula CLOSE           { $2 }
  | tpexpr                         { $1 }
;
tpvar_list:
  | UIDENT COMMA tpvar_list        { evar $1 :: $3 }
  | UIDENT                         { [evar $1] }
;


/* Focal Syntax */

coqfile:
  | LOCAL IDENT COLON coqexpr COLONEQUAL
    TOBE coqexpr coq_hyp_def_list QED EOF
      { ($2, Hyp ("_Zgoal", enot $4, 0) :: $8) }
  | BEGINPROOF coqexpr coq_hyp_def_list ENDPROOF EOF
      { ($1, Hyp ("_Zgoal", enot $2, 0) :: $3) }
;
coqexpr:
  | OPEN coqexpr CLOSE
      { $2 }
  | OPEN IDENT COLON IDENT CLOSE coqexpr %prec forall
      { eall (evar $2, $4, $6) }
  | FORALL IDENT COLON IDENT COMMA coqexpr %prec forall
      { eall (evar $2, $4, $6) }
  | coqapplication
      { eapp $1 }
  | TILDE coqexpr
      { enot ($2) }
  | OPEN AND coqexpr coqexpr CLOSE
      { eand ($3, $4) }
  | OPEN OR coqexpr coqexpr CLOSE
      { eor ($3, $4) }
  | IF coqexpr THEN coqexpr ELSE coqexpr
      { eapp ("_if_then_else", [$2; $4; $6]) }
  | OPEN coqexpr EQUAL coqexpr CLOSE
      { eapp ("=", [$2; $4]) }
  | IDENT
      { evar ($1) }
  | coqexpr ARROW coqexpr
      { eimply ($1, $3) }
  | coqexpr DOUBLEARROW coqexpr
      { eequiv ($1, $3) }
  | coqexpr SLASHBACKSLASH coqexpr
      { eand ($1, $3) }
  | coqexpr BACKSLASHSLASH coqexpr
      { eor ($1, $3) }

  /* FIXME TODO voir comment coder les let-in */

  | LBRACKET IDENT COLONEQUAL coqexpr RBRACKET coqexpr %prec forall
      { Expr.substitute [(evar $2, $4)] $6 }
  | LET IDENT COLONEQUAL coqexpr IN coqexpr %prec forall
      { Expr.substitute [(evar $2, $4)] $6 }
;
coqapplication:
  | OPEN IDENT coqexpr_list1 CLOSE
      { ($2, $3) }
  | OPEN coqapplication coqexpr_list1 CLOSE
      { let (sym, args1) = $2 in (sym, args1 @ $3) }
;
coqexpr_list1:
  | coqexpr              { [$1] }
  | coqexpr coqexpr_list1 { $1 :: $2 }
;

/* normal identifier or unparsed coq expression */
id_or_coqexpr:
  | IDENT  { $1 }
  | STRING { $1 }

coqhyp:
  | id_or_coqexpr COLON coqexpr  { Hyp ($1, $3, 1) }
;
coqhyp_list:
  | /* empty */          { [] }
  | coqhyp coqhyp_list   { $1 :: $2 }
;
coqdef:
  | id_or_coqexpr COLONEQUAL coqparam_expr
      { let (params, expr) = $3 in Def (DefReal ($1, params, expr)) }
;
coqparam_expr:
  | coqexpr
      { ([], $1) }
  | LBRACKET IDENT COLON IDENT RBRACKET coqparam_expr
      { let (params, expr) = $6 in ((evar $2) :: params, expr) }
  | FUN OPEN IDENT COLON IDENT CLOSE FUNARROW coqparam_expr
      { let (params, expr) = $8 in ((evar $3) :: params, expr) }
;
coqdef_list:
  | /* empty */           { [] }
  | coqdef coqdef_list    { $1 :: $2 }
;
coq_hyp_def_list:
  | BY coqhyp_list coq_hyp_def_list
      { $2 @ $3 }
  | BYDEF coqdef_list coq_hyp_def_list
      { $2 @ $3 }
  | /* empty */
      { [] }
;
%%
