(* Original file: jingoo.1.2.9/jingoo-1.2.9/src/jg_parser.mly *)
%{
(*
  jg_parser.mly

  Copyright (c) 2011- by Masaki WATANABE

  License: see LICENSE
*)
  open Jg_utils
  open Jg_types

  let debug = false

  let pel x = if debug then print_endline x else ()
  let pelspf fmt x = if debug then print_endline (Printf.sprintf fmt x) else ()
  let ident_name = function IdentExpr(name) -> name | _ -> raise @@ SyntaxError "type error:ident_name"
%}

%token IF
%token ELSE
%token ELSEIF
%token ENDIF
%token FOR
%token ENDFOR
%token IN
%token SET
%token EXTENDS
%token INCLUDE
%token MACRO
%token ENDMACRO
%token BLOCK
%token ENDBLOCK
%token FILTER
%token ENDFILTER
%token CALL
%token ENDCALL
%token IMPORT
%token AS
%token FROM
%token IS
%token WITH
%token ENDWITH
%token WITHOUT
%token CONTEXT
%token AUTOESCAPE
%token ENDAUTOESCAPE
%token RAWINCLUDE
%token EOF

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> TEXT
%token <string> IDENT
%token TRUE
%token FALSE
%token NULL
%token COMMA
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token COLON
%token PLUS
%token MINUS
%token TIMES
%token POWER
%token DIV
%token MOD
%token EQ
%token EQ_EQ
%token NEQ
%token LT
%token GT
%token LT_EQ
%token GT_EQ
%token AND
%token OR
%token NOT
%token DOT
%token VLINE

%left EQ
%left OR
%left AND
%left EQ_EQ NEQ
%left LT GT LT_EQ GT_EQ
%left PLUS MINUS
%left TIMES DIV MOD
%left VLINE
%right POWER
%nonassoc NOT UMINUS
%left DOT

%start input
%type <Jg_types.ast> input

%%

input:
  EOF { [] }
| stmts EOF { $1 }
;

stmts:
  stmt { [$1] }
| stmt stmts { $1 :: $2 }
;

stmt:
  expr { pel "expand expr"; ExpandStatement($1) }
| error { raise @@ SyntaxError "expand stmt error" }
| SET ident_list EQ expr { pel "set sts"; SetStatement(SetExpr($2), $4) }
| SET error { raise @@ SyntaxError "set" }
| EXTENDS STRING { pel "extends sts"; ExtendsStatement($2) }
| EXTENDS error { raise @@ SyntaxError "extends" }
| BLOCK ident ENDBLOCK { pel "block sts"; BlockStatement($2, []) }
| BLOCK ident stmts ENDBLOCK { pel "block sts2"; BlockStatement($2, $3) }
| BLOCK error { raise @@ SyntaxError "block" }
| FILTER ident stmts ENDFILTER { pel "filter sts"; FilterStatement($2, $3) }
| FILTER error { raise @@ SyntaxError "filter" }
| INCLUDE expr context_part{ pel "include sts"; IncludeStatement($2, $3) }
| INCLUDE error { raise @@ SyntaxError "include" }
| RAWINCLUDE expr { pel "raw include sts"; RawIncludeStatement($2) }
| RAWINCLUDE error { raise @@ SyntaxError "rawinclude" }
| IMPORT STRING as_part { pel "import sts"; ImportStatement($2, $3) }
| IMPORT error{ raise @@ SyntaxError "import error" }
| FROM STRING IMPORT expr_list { pel "from import sts"; FromImportStatement($2, $4) }
| FROM error{ raise @@ SyntaxError "from import error" }
| MACRO ident LPAREN expr_list RPAREN stmts ENDMACRO { pel "macro sts"; MacroStatement($2, $4, $6) }
| MACRO error { raise @@ SyntaxError "macro" }
| CALL opt_args ident LPAREN expr_list RPAREN stmts ENDCALL { pel "call sts"; CallStatement($3, $2, $5, $7) }
| CALL error { raise @@ SyntaxError "call error" }
| IF if_chain else_part ENDIF { pel "if sts"; IfStatement($2, $3) }
| IF error { raise @@ SyntaxError "if" }
| FOR ident_list IN expr stmts ENDFOR { pel "for sts"; ForStatement(SetExpr($2), $4, $5) }
| FOR expr IN expr stmts ENDFOR { pel "for sts"; ForStatement($2, $4, $5) }
| FOR error { raise @@ SyntaxError "for" }
| WITH expr_list stmts ENDWITH { pel "with sts1"; WithStatement($2, $3) }
| WITH error { raise @@ SyntaxError "with" }
| AUTOESCAPE expr stmts ENDAUTOESCAPE { pel "autoescape"; AutoEscapeStatement($2, $3) }
| AUTOESCAPE error { raise @@ SyntaxError "autoescape" }
| TEXT { pel "text sts"; TextStatement($1) }
| TEXT error { raise @@ SyntaxError "text" }
;

if_chain:
  expr stmts { pel "if chain"; [($1, $2)] }
| expr stmts ELSEIF if_chain { pel "if chain2"; ($1, $2) :: $4 }
| expr stmts ELSEIF error { raise @@ SyntaxError "if_chain" }
;

else_part:
/* empty */ { pel "else part empty"; [] }
| ELSE stmts { pel "else part"; $2 }
| ELSE error { raise @@ SyntaxError "else_part" }
;

as_part:
/* empty */ { None }
| AS ident { Some (ident_name $2) }
| AS error { raise @@ SyntaxError "as_part" }
;

context_part:
/* empty */ { true }
| WITH CONTEXT { true }
| WITHOUT CONTEXT { false }
;

ident:
  IDENT { pelspf "ident(%s)" $1; IdentExpr($1) }
| IDENT error { raise @@ SyntaxError "ident" }
;

ident_list:
  ident { pel "ident list"; [$1] }
| ident COMMA ident_list { pel "iden list commna"; $1 :: $3 }
| ident COMMA error { raise @@ SyntaxError "ident_list" }
;

expr_list:
/* empty */ { pel "empty expr list"; [] }
| expr { pel "expr list"; [$1] }
| expr COMMA expr_list { pel "expr list comma"; $1 :: $3 }
| expr COMMA error { raise @@ SyntaxError "expr_list" }
;

expr:
  ident { pel "ident"; $1 }
| ident EQ expr { pel "keyword"; KeywordExpr($1, $3) }
| ident AS ident { pel "alias"; AliasExpr($1, $3) }
| ident LPAREN expr_list RPAREN { pel "apply(expr_list)"; ApplyExpr($1, $3) }
| expr LPAREN expr_list RPAREN { pel "apply(expr_list)"; ApplyExpr($1, $3) }
| INT { pel "int"; LiteralExpr (Tint $1) }
| FLOAT { pel "float"; LiteralExpr (Tfloat $1) }
| TRUE { pel "true"; LiteralExpr (Tbool true) }
| FALSE { pel "false"; LiteralExpr (Tbool false) }
| STRING { pel "string"; LiteralExpr (Tstr $1) }
| NULL { pel "null"; LiteralExpr Tnull }
| expr DOT ident { pel "dot_lookup"; DotExpr($1, $3) }
| expr LBRACKET STRING RBRACKET { pel "dot_lookup(dict)"; DotExpr($1, IdentExpr($3)) }
| NOT expr { pel "not expr"; NotOpExpr($2) }
| MINUS expr %prec UMINUS { pel "negative"; NegativeOpExpr($2) }
| LBRACKET expr_list RBRACKET { pel "list expr"; ListExpr($2) }
| LBRACE assoc_list RBRACE { pel "obj expr"; ObjExpr($2) }
| expr PLUS expr { pel "plus"; PlusOpExpr($1, $3) }
| expr MINUS expr { pel "minus"; MinusOpExpr($1, $3) }
| expr DIV expr { pel "div"; DivOpExpr($1, $3) }
| expr MOD expr { pel "mod"; ModOpExpr($1, $3) }
| expr TIMES expr { pel "times"; TimesOpExpr($1, $3) }
| expr POWER expr { pel "power"; PowerOpExpr($1, $3) }
| expr AND expr { pel "and"; AndOpExpr($1, $3) }
| expr OR expr { pel "or"; OrOpExpr($1, $3) }
| expr EQ_EQ expr { pel "eqeq"; EqEqOpExpr($1, $3) }
| expr NEQ expr { pel "noteq"; NotEqOpExpr($1, $3) }
| expr LT expr { pel "lt"; LtOpExpr($1, $3) }
| expr GT expr { pel "gt"; GtOpExpr($1, $3) }
| expr LT_EQ expr { pel "lteq"; LtEqOpExpr($1, $3) }
| expr GT_EQ expr { pel "gteq"; GtEqOpExpr($1, $3) }
| expr IN expr { pel "inop"; InOpExpr($1, $3) }
| expr VLINE expr { pel "expr|expr -> ApplyExpr"; ApplyExpr($3, [$1]) }
| expr IS expr expr{
  (** when expr1 is fun and expr2 is args with out LPAREN and RPAREN. *)
  (** for example, 'a is divisableby 2' *)
  pel "test(apply)";
  TestOpExpr($1, ApplyExpr($3, [$4]))
}
| expr IS expr { pel "test"; TestOpExpr($1,$3) }
| LPAREN expr RPAREN { pel "(expr)"; $2 }
| LPAREN expr_list RPAREN { pel "set expr"; SetExpr($2) }
| LPAREN error { raise @@ SyntaxError "expr" }
;

assoc_list:
  assoc { pel "assoc list1"; [$1] }
| assoc COMMA assoc_list { pel "assoc list2"; $1 :: $3 }
| assoc COMMA error { raise @@ SyntaxError "assoc list error" }
;

assoc:
  expr COLON expr { ($1, $3) }
| expr error { raise @@ SyntaxError "assoc error" }
;

opt_args:
/* empty */ { pel "opt_args empty"; [] }
| LPAREN expr_list RPAREN { $2 }
;
