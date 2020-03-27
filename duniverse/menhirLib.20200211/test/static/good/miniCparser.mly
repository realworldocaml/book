/*  A modular module system.
    The parser for core-C.

    Copyright 1999 Xavier Leroy.
    This file is distributed under the GNU Public Licence. */

%{

open Modules
open MiniC
open C

%}

%token BANG
%token BANGEQUAL
%token COLON
%token COMMA
%token DOT
%token ELSE
%token END
%token EOF
%token EQUAL
%token EQUALEQUAL
%token FLOAT
%token <float> FLOATCONST
%token FOR
%token FUNCTOR
%token GREATER
%token GREATEREQUAL
%token <string> LIDENT
%token <string> UIDENT
%token IF
%token INT
%token <int> INTCONST
%token LBRACE
%token LBRACKET
%token LESS
%token LESSEQUAL
%token LPAREN
%token MINUS
%token MODULE
%token PLUS
%token RBRACE
%token RBRACKET
%token RETURN
%token RPAREN
%token SEMI
%token SIG
%token SLASH
%token STAR
%token STRUCT
%token TYPEDEF
%token VOID

/* Precedences */
%right EQUAL
%left EQUALEQUAL BANGEQUAL
%left LESS LESSEQUAL GREATER GREATEREQUAL
%left PLUS MINUS
%left STAR SLASH
%left prec_uminus
%right BANG prec_deref
%left LPAREN RPAREN LBRACKET RBRACKET

%start implementation
%type <MiniC.CMod.mod_term> implementation
%start phrase
%type <MiniC.CMod.definition> phrase

%%

/* Paths */

upath:
    UIDENT           { Pident(Ident.create $1) }
  | upath DOT UIDENT { Pdot($1, $3) }
;
lpath:
    LIDENT           { Pident(Ident.create $1) }
  | upath DOT LIDENT { Pdot($1, $3) }
;

/* Values */

expr:
    INTCONST                            { Intconst $1 }
  | FLOATCONST                          { Floatconst $1 }
  | lpath                               { Variable $1 }
  | expr LPAREN expr_comma_list RPAREN  { Apply($1, List.rev $3) }
  | expr EQUAL expr                     { Assign($1, $3) }
  | STAR expr %prec prec_deref          { Unary_op("*", $2) }
  | MINUS expr %prec prec_uminus        { Unary_op("-", $2) }
  | BANG expr                           { Unary_op("!", $2) }
  | expr PLUS expr                      { Binary_op("+", $1, $3) }
  | expr MINUS expr                     { Binary_op("-", $1, $3) }
  | expr STAR expr                      { Binary_op("*", $1, $3) }
  | expr SLASH expr                     { Binary_op("/", $1, $3) }
  | expr EQUALEQUAL expr                { Binary_op("==", $1, $3) }
  | expr BANGEQUAL expr                 { Binary_op("!=", $1, $3) }
  | expr LESS expr                      { Binary_op("<", $1, $3) }
  | expr LESSEQUAL expr                 { Binary_op("<=", $1, $3) }
  | expr GREATER expr                   { Binary_op(">", $1, $3) }
  | expr GREATEREQUAL expr              { Binary_op(">=", $1, $3) }
  | expr LBRACKET expr RBRACKET         { Unary_op("*", Binary_op("+", $1, $3)) }
  | LPAREN ctype RPAREN expr            { Cast($4, $2) }
  | LPAREN expr RPAREN                  { $2 }
;
expr_comma_list:
    /* empty */                         { [] }
  | expr                                { [$1] }
  | expr_comma_list COMMA expr          { $3 :: $1 }
;
statement:
    expr SEMI                              { Expr $1 }
  | IF LPAREN expr RPAREN statement ELSE statement { If($3, $5, $7) }
  | IF LPAREN expr RPAREN statement        { If($3, $5, Block([], [])) }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN statement
                                           { For($3, $5, $7, $9) }
  | RETURN expr SEMI                       { Return $2 }
  | LBRACE decl_list statement_list RBRACE { Block(List.rev $2, List.rev $3) }
;
statement_list:
    /* empty */                         { [] }
  | statement_list statement            { $2 :: $1 }
;
decl_list:
    /* empty */                         { [] }
  | decl_list ctype LIDENT SEMI         { (Ident.create $3, $2) :: $1 }
;
term:
    ctype LIDENT SEMI
      { (Ident.create $2, Var_decl $1) }
  | ctype LIDENT LPAREN parameters RPAREN statement
      { (Ident.create $2, Fun_def(List.rev $4, $1, $6)) }
;
parameters:
    /* empty */                         { [] }
  | ctype LIDENT                        { [Ident.create $2, $1] }
  | parameters COMMA ctype LIDENT       { (Ident.create $4, $3) :: $1 }
;

/* Types */

ctype:
    VOID                                { Void }
  | INT                                 { Int }
  | FLOAT                               { Float }
  | upath                               { Typename $1 }
  | ctype STAR                          { Pointer $1 }
;

/* Type defs */

typedef:
    ctype typedef_cont                  { $2($1) }
;
typedef_cont:
    UIDENT
      { let id = Ident.create $1 in (fun ty -> (id, ty)) }
  | STAR typedef_cont
      { let f = $2 in fun ty -> let (id, ty') = f ty in (id, Pointer ty') }
  | LPAREN typedef_cont RPAREN
      { $2 }
  | typedef_cont LPAREN ty_params RPAREN
      { let f = $1 in
        let ty_args = List.rev $3 in
        fun ty -> let (id, ty_res) = f ty in (id, Function(ty_args, ty_res)) }
;
ty_params:
    ty_param                    { [$1] }
  | ty_params COMMA ty_param    { $3 :: $1 }
;
ty_param:
    ctype                       { $1 }
  | ctype LIDENT                { $1 }
;

/* Value declarations */

valuedecl:
    ctype valuedecl_cont                  { $2($1) }
;
valuedecl_cont:
    LIDENT
      { let id = Ident.create $1 in (fun ty -> (id, ty)) }
  | STAR valuedecl_cont
      { let f = $2 in fun ty -> let (id, ty') = f ty in (id, Pointer ty') }
  | LPAREN valuedecl_cont RPAREN
      { $2 }
  | valuedecl_cont LPAREN ty_params RPAREN
      { let f = $1 in
        let ty_args = List.rev $3 in
        fun ty -> let (id, ty_res) = f ty in (id, Function(ty_args, ty_res)) }
;

/* Value expressions for the module language */

modulexpr:
    upath                             { CMod.Longident $1 }
  | STRUCT structure END              { CMod.Structure(List.rev $2) }
  | FUNCTOR LPAREN UIDENT COLON moduletype RPAREN modulexpr
                                      { CMod.Functor(Ident.create $3, $5, $7) }
  | modulexpr LPAREN modulexpr RPAREN { CMod.Apply($1, $3) }
  | LPAREN modulexpr RPAREN           { $2 }
  | modulexpr COLON moduletype        { CMod.Constraint($1, $3) }
;
structure:
    /*nothing*/                       { [] }
  | structure structure_item          { $2 :: $1 }
;
structure_item:
    term
      { let (id, term) = $1 in CMod.Value_str(id, term) }
  | TYPEDEF typedef SEMI
      { let (id, ty) = $2 in CMod.Type_str(id, (), ty) }
  | MODULE UIDENT COLON moduletype EQUAL modulexpr SEMI
      { CMod.Module_str(Ident.create $2, CMod.Constraint($6, $4)) }
  | MODULE UIDENT EQUAL modulexpr SEMI
      { CMod.Module_str(Ident.create $2, $4) }
;

/* Type expressions for the module language */

moduletype:
    SIG signature END               { CMod.Signature(List.rev $2) }
  | FUNCTOR LPAREN UIDENT COLON moduletype RPAREN moduletype
                                    { CMod.Functor_type(Ident.create $3, $5, $7) }
  | LPAREN moduletype RPAREN        { $2 }
;
signature:
    /*nothing*/                     { [] }
  | signature signature_item        { $2 :: $1 }
;
signature_item:
    valuedecl SEMI
      { let (id, ty) = $1 in CMod.Value_sig(id, ty) }
  | TYPEDEF typedef SEMI
      { let (id, ty) = $2 in
        CMod.Type_sig(id, {CMod.kind = (); CMod.manifest = Some ty}) }
  | TYPEDEF UIDENT SEMI
      { CMod.Type_sig(Ident.create $2, {CMod.kind = (); CMod.manifest = None}) }
  | MODULE UIDENT COLON moduletype SEMI
      { CMod.Module_sig(Ident.create $2, $4) }
;

/* Toplevel entry point */

phrase:
    structure_item                    { $1 }
  | EOF                               { raise End_of_file }
;

/* Sep. comp. entry point */

implementation:
    modulexpr EOF                     { $1 }
;
