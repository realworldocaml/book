%{
open Utils
open Cminor
%}

%token ABSF
%token AMPERSAND
%token AMPERSANDAMPERSAND
%token BANG
%token BANGEQUAL
%token BANGEQUALF
%token BANGEQUALU
%token BAR
%token BARBAR
%token CARET
%token CASE
%token COLON
%token COMMA
%token DEFAULT
%token ELSE
%token EQUAL
%token EQUALEQUAL
%token EQUALEQUALF
%token EQUALEQUALU
%token EOF
%token EXIT
%token FLOAT
%token FLOAT32
%token FLOAT64
%token <float> FLOATLIT
%token FLOATOFINT
%token FLOATOFINTU
%token GREATER
%token GREATERF
%token GREATERU
%token GREATEREQUAL
%token GREATEREQUALF
%token GREATEREQUALU
%token GREATERGREATER
%token GREATERGREATERU
%token <string> IDENT
%token IF
%token INT
%token INT16S
%token INT16U
%token INT32
%token INT8S
%token INT8U
%token <int32> INTLIT
%token INTOFFLOAT
%token LBRACE
%token LBRACKET
%token LESS
%token LESSU
%token LESSF
%token LESSEQUAL
%token LESSEQUALU
%token LESSEQUALF
%token LESSLESS
%token LOOP
%token LPAREN
%token MINUS
%token MINUSF
%token PERCENT
%token PERCENTU
%token PLUS
%token PLUSF
%token QUESTION
%token RBRACE
%token RBRACKET
%token RETURN
%token RPAREN
%token SEMICOLON
%token SLASH
%token SLASHF
%token SLASHU
%token STACK
%token STAR
%token STARF
%token <string> STRINGLIT
%token SWITCH
%token TILDE
%token VAR

/* Precedences from low to high */

%left p_sequence
%left COMMA
%right EQUAL
%right QUESTION COLON
%left BARBAR
%left AMPERSANDAMPERSAND
%left BAR
%left CARET
%left AMPERSAND
%left EQUALEQUAL BANGEQUAL LESS LESSEQUAL GREATER GREATEREQUAL EQUALEQUALU BANGEQUALU LESSU LESSEQUALU GREATERU GREATEREQUALU EQUALEQUALF BANGEQUALF LESSF LESSEQUALF GREATERF GREATEREQUALF
%left LESSLESS GREATERGREATER GREATERGREATERU
%left PLUS PLUSF MINUS MINUSF
%left STAR SLASH PERCENT STARF SLASHF SLASHU PERCENTU
%nonassoc BANG TILDE p_uminus ABSF INTOFFLOAT FLOATOFINT FLOATOFINTU INT8S INT8U INT16S INT16U FLOAT32
%left LPAREN

/* Entry point */

%start prog
%type <Cminor.program> prog

%%

/* Programs */

prog:
    global_declarations proc_list EOF
      { { prog_procs = List.rev $2;
          prog_main = "main";
          prog_vars = List.rev $1; } }
;

global_declarations:
    /* empty */                                 { [] }
  | global_declarations global_declaration      { $2 :: $1 }
;

global_declaration:
    VAR STRINGLIT LBRACKET INTLIT RBRACKET      { ($2, Int32.to_int $4, 4) }
;

proc_list:
    /* empty */                                 { [] }
  | proc_list proc                              { $2 :: $1 }
;

/* Procedures */

proc:
    STRINGLIT LPAREN parameters RPAREN opt_type
    LBRACE
      stack_declaration
      var_declarations
      stmt_list
    RBRACE
      { { proc_name = $1;
          proc_return = $5;
          proc_params = List.rev $3;
          proc_stackspace = $7;
          proc_vars = List.rev $8;
          proc_body = List.rev $9 } }
;

parameters:
    /* empty */                                 { [] }
  | parameter_list                              { $1 }
;

parameter_list:
    parameter                                   { [$1] }
  | parameter_list COMMA parameter              { $3 :: $1 }
;

parameter:
    IDENT COLON type_                           { ($1, $3) }
;

stack_declaration:
    /* empty */                                 { 0 }
  | STACK INTLIT                                { Int32.to_int $2 }
;

var_declarations:
    /* empty */                                 { [] }
  | var_declarations var_declaration            { $2 @ $1 }
;

var_declaration:
    VAR parameter_list SEMICOLON                { $2 }
;

/* Statements */

stmt:
    expr SEMICOLON                              { Sexpr $1 }
  | IF LPAREN expr RPAREN stmts ELSE stmts      { Sifthenelse($3, $5, $7) }
  | IF LPAREN expr RPAREN stmts                 { Sifthenelse($3, $5, []) }
  | SWITCH expr LBRACE switch_cases RBRACE      { Sswitch($2, List.rev $4) }
  | LOOP stmts                                  { Sloop($2) }
  | INTLIT COLON stmts                          { Sblock(Int32.to_int $1, $3) }
  | EXIT INTLIT SEMICOLON                       { Sexit (Int32.to_int $2) }
  | RETURN SEMICOLON                            { Sreturn None }
  | RETURN expr SEMICOLON                       { Sreturn (Some $2) }
;

stmts:
    LBRACE stmt_list RBRACE                     { List.rev $2 }
  | stmt                                        { [$1] }
;

stmt_list:
    /* empty */                                 { [] }
  | stmt_list stmt                              { $2 :: $1 }
;

switch_cases:
    /* empty */                                 { [] }
  | switch_cases switch_case                    { $2 :: $1 }
;

switch_case:
    case_list stmt_list                         { (List.rev $1, List.rev $2) }
  | DEFAULT COLON stmt_list                     { ([], List.rev $3) }
;

case_list:
    case                                        { [$1] }
  | case_list case                              { $2 :: $1 }
;

case:
    CASE INTLIT COLON                           { $2 }
;

/* Expressions */

expr:
    LPAREN expr RPAREN                          { $2 }
  | IDENT                                       { Evar $1 }
  | IDENT EQUAL expr                            { Eassign($1, $3) }
  | INTLIT                                      { Econstint $1 }
  | FLOATLIT                                    { Econstfloat $1 }
  | STRINGLIT                                   { Eaddrsymbol $1 }
  | AMPERSAND INTLIT                            { Eaddrstack $2 }
  | MINUS expr    %prec p_uminus                { Eunop(OPnegint, $2) }
  | MINUSF expr   %prec p_uminus                { Eunop(OPnegfloat, $2) }
  | ABSF expr                                   { Eunop(OPabsfloat, $2) }
  | INTOFFLOAT expr                             { Eunop(OPintoffloat, $2) }
  | FLOATOFINT expr                             { Eunop(OPfloatofint, $2) }
  | FLOATOFINTU expr                            { Eunop(OPfloatofintu, $2) }
  | TILDE expr                                  { Eunop(OPnotint, $2) }
  | BANG expr                                   { Eunop(OPnotbool, $2) }
  | INT8S expr                                  { Eunop(OPcast8signed, $2) }
  | INT8U expr                                  { Eunop(OPcast8unsigned, $2) }
  | INT16S expr                                 { Eunop(OPcast16signed, $2) }
  | INT16U expr                                 { Eunop(OPcast16unsigned, $2) }
  | FLOAT32 expr                                { Eunop(OPsingleoffloat, $2) }
  | expr PLUS expr                              { Ebinop(OPaddint, $1, $3) }
  | expr MINUS expr                             { Ebinop(OPsubint, $1, $3) }
  | expr STAR expr                              { Ebinop(OPmulint, $1, $3) }
  | expr SLASH expr                             { Ebinop(OPdivint, $1, $3) }
  | expr PERCENT expr                           { Ebinop(OPmodint, $1, $3) }
  | expr SLASHU expr                            { Ebinop(OPdivintu, $1, $3) }
  | expr PERCENTU expr                          { Ebinop(OPmodintu, $1, $3) }
  | expr AMPERSAND expr                         { Ebinop(OPandint, $1, $3) }
  | expr BAR expr                               { Ebinop(OPorint, $1, $3) }
  | expr CARET expr                             { Ebinop(OPxorint, $1, $3) }
  | expr LESSLESS expr                          { Ebinop(OPshiftleftint, $1, $3) }
  | expr GREATERGREATER expr                    { Ebinop(OPshiftrightint, $1, $3) }
  | expr GREATERGREATERU expr                   { Ebinop(OPshiftrightintu, $1, $3) }
  | expr PLUSF expr                             { Ebinop(OPaddfloat, $1, $3) }
  | expr MINUSF expr                            { Ebinop(OPsubfloat, $1, $3) }
  | expr STARF expr                             { Ebinop(OPmulfloat, $1, $3) }
  | expr SLASHF expr                            { Ebinop(OPdivfloat, $1, $3) }
  | expr EQUALEQUAL expr                        { Ebinop(OPcmpint Cequal, $1, $3) }
  | expr BANGEQUAL expr                         { Ebinop(OPcmpint Cnotequal, $1, $3) }
  | expr LESS expr                              { Ebinop(OPcmpint Cless, $1, $3) }
  | expr LESSEQUAL expr                         { Ebinop(OPcmpint Clessequal, $1, $3) }
  | expr GREATER expr                           { Ebinop(OPcmpint Cgreater, $1, $3) }
  | expr GREATEREQUAL expr                      { Ebinop(OPcmpint Cgreaterequal, $1, $3) }
  | expr EQUALEQUALU expr                       { Ebinop(OPcmpintu Cequal, $1, $3) }
  | expr BANGEQUALU expr                        { Ebinop(OPcmpintu Cnotequal, $1, $3) }
  | expr LESSU expr                             { Ebinop(OPcmpintu Cless, $1, $3) }
  | expr LESSEQUALU expr                        { Ebinop(OPcmpintu Clessequal, $1, $3) }
  | expr GREATERU expr                          { Ebinop(OPcmpintu Cgreater, $1, $3) }
  | expr GREATEREQUALU expr                     { Ebinop(OPcmpintu Cgreaterequal, $1, $3) }
  | expr EQUALEQUALF expr                       { Ebinop(OPcmpfloat Cequal, $1, $3) }
  | expr BANGEQUALF expr                        { Ebinop(OPcmpfloat Cnotequal, $1, $3) }
  | expr LESSF expr                             { Ebinop(OPcmpfloat Cless, $1, $3) }
  | expr LESSEQUALF expr                        { Ebinop(OPcmpfloat Clessequal, $1, $3) }
  | expr GREATERF expr                          { Ebinop(OPcmpfloat Cgreater, $1, $3) }
  | expr GREATEREQUALF expr                     { Ebinop(OPcmpfloat Cgreaterequal, $1, $3) }
  | memory_chunk LBRACKET expr RBRACKET         { Eload($1, $3) }
  | memory_chunk LBRACKET expr RBRACKET EQUAL expr
                                                { Estore($1, $3, $6) }
  | expr LPAREN expr_list RPAREN opt_type       { Ecall($1, $3, $5) }
  | expr AMPERSANDAMPERSAND expr                { Eandbool($1, $3) }
  | expr BARBAR expr                            { Eorbool($1, $3) }
  | expr QUESTION expr COLON expr               { Econdition($1, $3, $5) }
  | expr COMMA expr %prec p_sequence            { Esequence2($1, $3) }
;

expr_list:
    /* empty */                                 { [] }
  | expr_list_1                                 { List.rev $1 }
;

expr_list_1:
    expr                     %prec COMMA        { [$1] }
  | expr_list_1 COMMA expr                      { $3 :: $1 }
;

memory_chunk:
    INT8S                                       { Mint8signed }
  | INT8U                                       { Mint8unsigned }
  | INT16S                                      { Mint16signed }
  | INT16U                                      { Mint16unsigned }
  | INT32                                       { Mint32 }
  | INT                                         { Mint32 }
  | FLOAT32                                     { Mfloat32 }
  | FLOAT64                                     { Mfloat64 }
  | FLOAT                                       { Mfloat64 }
;

/* Types */

type_:
    INT                                         { Tint }
  | FLOAT                                       { Tfloat }
;

opt_type:
    /* empty */                                 { None }
  | COLON type_                                 { Some($2) }
;
