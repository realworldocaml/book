%{
(***
 *** PARSER.MLY
 ***
 *** Copyright (c) 2003 The President and Fellows of Harvard College
 ***
 *** Parser for IBAL language
 ***)

  open List
  open Printf

  open Global
  open Util
  open Variable
  open Code
  open Types
  open Value
  open Prim
  open Parse_ctrl

  let tag () = (!curr_line_no, curr_pos_in_line ())

  let make_code : 'a code_form -> (int * int) code =
    fun c -> (c, tag ())

  let make_decl : 'a decl_form -> (int * int) decl =
    fun d -> (d, tag ())

  let make_param : string * float list -> (int * int) decl_form =
    fun (x,fs) -> Param (x,fs)

  let make_prim : prim * (int * int) code list -> (int * int) code =
    fun (p,es) -> (Appl ((Prim p, tag ()), es), tag ())

  let uniform_int : int -> (int * int) code =
    fun n ->
      if n < 1
      then error "Uniform requires a positive integer"
      else let t = tag () in
      let p = 1.0 /. float_of_int n in
      (Dist (Array.to_list
	       (Array.init n (fun i -> (p, (Integer i, t))))),
       t)

  let make_anon_compound : ty list -> ty =
    fun ts ->
      Compound (number_list ts)

  let tyvars : (string * tyvar) list ref = ref []

  let clear_tyvars () = tyvars := []

  let new_tyvar_for_name x =
    let i = List.length !tyvars in
    tyvars := (x,i) :: !tyvars;
    i

  let tyvar_of_name x =
    try List.assoc x !tyvars
    with Not_found ->
      new_tyvar_for_name x
(*      error (sprintf "Unquantified type variable %s" x) *)
%}

%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LSQUARE
%token RSQUARE
%token LTUP
%token RTUP
%token EQUALS
%token DOT
%token COLON
%token HASH
%token QUOTE
%token <string> STRING
%token COMMA
%token SEMICOLON
%token IF
%token THEN
%token ELSE
%token LET
%token IN
%token LAMBDA
%token FIX
%token MAPSTO
%token EQUALSEQUALS
%token TRUE
%token FALSE
%token DIST
%token FLIP
%token CASE
%token OF
%token OBSERVE
%token RETRACT
%token LARROW
%token NOTEQUAL
%token ANY
%token PRIVATE
%token DECIDE
%token FROM
%token GIVEN
%token REWARD
%token DISCOUNT
%token PARAM
%token PARAMS
%token PDIST
%token TYPE
%token DATA
%token ETYPE
%token EDATA
%token NOT
%token AND
%token BAR
%token SYMBOL
%token BOOL
%token INT
%token FORALL
%token ERROR
%token PRAGMA
%token OP
%token DOLLAR
%token <string> LIDENT
%token <string> UIDENT
%token <float> POSFLOAT
%token <int> NATURAL
%token PLUS
%token PLUSPLUS
%token TIMES
%token DIVIDE
%token MOD
%token CONCAT
%token MINUS
%token MINUSMINUS
%token DOTDOT
%token UNIFORM
%token EOF

%start expr
%start decl
%start top
%type <(int*int) Code.code> expr
%type <(int*int) Code.decl> decl
%type <(int*int) Code.top> top

%left BAR
%left AND
%nonassoc NOT
%left CONCAT
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc PLUSPLUS MINUSMINUS


%%

top : expr                              { Code $1 }
    | decl                              { Decl $1 }

expr :
    | expr1 { $1 }
    | expr2 { $1 }
    | expr3 { $1 }
    | expr4 { $1 }

expr1 :
    | integer                          { make_code (Integer $1) }

expr_nonint :
    | expr2 { $1 }
    | expr3 { $1 }
    | expr_nonint4 { $1 }

expr2 :
     | longvar                          { make_code (Var $1) }
     | LPAREN expr RPAREN               { $2 }
     | expr2 LPAREN RPAREN                  { make_code (Appl ($1, [])) }
     | expr2 LPAREN expr RPAREN             { make_code (Appl ($1, [$3])) }
     | expr2 LPAREN expr COMMA exprs RPAREN { make_code (Appl ($1, $3 :: $5)) }

expr3 :
     | string                           { make_code (Con $1) }
     | MINUS expr_nonint               { make_prim (int_unary_minus, [$2]) }
     | TRUE                             { make_code True }
     | FALSE                            { make_code False }
     | ulongvar opt_paren_exprs           { make_code (Cons ($1,$2)) }
     | IF expr THEN expr ELSE expr { make_code (If ($2,$4,$6)) }
     | CASE expr OF clauses        { make_code (Case ($2, $4)) }
     | DIST LSQUARE weighted_exprs RSQUARE { make_code (Dist $3) }
     | UNIFORM NATURAL                     { uniform_int $2 }
     | FLIP POSFLOAT { make_code (Dist [ ($2, make_code (True));
				      (1.0 -. $2, make_code (False)) ]) }
     | LET pat EQUALS expr IN expr    { make_code (Let ($2,$4,$6)) }
     | LET pat COLON type_expr EQUALS expr IN expr
	 { make_code (Let ($2, make_code (Typed ($6,quantify $4)), $8)) }
     | LET LIDENT LPAREN opt_vars RPAREN EQUALS expr IN expr
	 { make_code (Let (Bind $2, make_code (Fix ($2, $4, $7)), $9)) }
     | LET LIDENT LPAREN opt_vars RPAREN COLON type_expr EQUALS expr IN expr
	 { make_code (Let (Bind $2,
			   make_code (Typed (make_code (Fix ($2, $4, $9)),
					     quantify $7)), $11)) }
     | LAMBDA LPAREN opt_vars RPAREN MAPSTO expr  { make_code (Lambda ($3,$6)) }
     | LAMBDA LIDENT MAPSTO expr  { make_code (Lambda ([$2],$4)) }
     | FIX LIDENT LPAREN opt_vars RPAREN MAPSTO expr { make_code (Fix ($2, $4, $7)) }
     | FIX LIDENT LIDENT MAPSTO expr { make_code (Fix ($2, [$3], $5)) }
      | LTUP opt_named_exprs RTUP       { make_code (Tuple $2) }
      | LTUP exprs RTUP             { make_code (AnonTuple $2) }
     | LBRACE opt_decls RBRACE              { make_code (Block $2) }
     | OBSERVE obs IN expr       { make_code (Observe (fst $2, snd $2,$4)) }
     | PARAMS param_decls IN expr      { make_code (Params ($2, $4)) }
     | PDIST ident LSQUARE exprs RSQUARE { make_code (PDist ($2, $4)) }
     | REWARD float IN expr            { make_code (Reward (make_code True, [(Any, $2)], $4)) }
     | REWARD CASE expr OF rew_clauses IN expr
	 { make_code (Reward ($3, $5, $7)) }
     | DISCOUNT POSFLOAT IN expr          { make_code (Discount ($2, $4)) }
     | DECIDE LIDENT values GIVEN longvars IN expr
	 { make_code (Decision ($2, $3, $5, $7)) }
     | DECIDE LIDENT values IN expr
	 { make_code (Decision ($2, $3, [], $5)) }
     | ETYPE UIDENT opt_paren_type_args EQUALS type_expr IN expr
	 { clear_tyvars (); make_code (Deftype ($2, $3, $5, $7)) }
     | EDATA UIDENT opt_paren_type_args EQUALS tycon_expr IN expr
         { clear_tyvars (); make_code (Deftycon (new_name $2, $3, $5, $7)) }
     | OP NOT             { make_code (Prim bool_not) }
     | OP AND             { make_code (Prim bool_and) }
     | OP BAR             { make_code (Prim bool_or) }
     | OP PLUS            { make_code (Prim int_plus) }
     | OP MINUS           { make_code (Prim int_minus) }
     | OP ANY             { make_code (Prim int_unary_minus) }
     | OP TIMES           { make_code (Prim int_times) }
     | OP DIVIDE          { make_code (Prim int_div) }
     | OP MOD             { make_code (Prim int_mod) }
     | OP PLUSPLUS        { make_code (Prim int_succ) }
     | OP MINUSMINUS      { make_code (Prim int_pred) }
     | OP CONCAT          { make_code (Prim string_concat) }
     | NOT expr                    { make_code (Not $2) }
     | ERROR string        { make_code (Error $2) }
     | ERROR               { make_code (Error "") }
     | PRAGMA string IN expr { make_code (LetPragma ($2,$4)) }

expr4 :
     | expr PLUS expr                   { make_prim (int_plus, [$1;$3]) }
     | expr MINUS expr                   { make_prim (int_minus, [$1;$3]) }
     | expr TIMES expr                   { make_prim (int_times, [$1;$3]) }
     | expr DIVIDE expr                   { make_prim (int_div, [$1;$3]) }
     | expr MOD expr                   { make_prim (int_mod, [$1;$3]) }
     | expr CONCAT expr                { make_prim (string_concat, [$1;$3]) }
     | expr PLUSPLUS                      { make_prim (int_succ, [$1]) }
     | expr MINUSMINUS                      { make_prim (int_pred, [$1]) }
     | expr EQUALSEQUALS expr           { make_code (Eq ($1, $3)) }
     | expr AND expr               { make_code (And ($1,$3)) }
     | expr BAR expr                { make_code (Or ($1,$3)) }
     | expr DOT LIDENT              { make_code (Dot ($1,$3)) }
     | expr COLON type_expr { clear_tyvars (); make_code (Typed ($1, quantify $3)) }

expr_nonint4 :
     | expr_nonint PLUS expr                   { make_prim (int_plus, [$1;$3]) }
     | expr_nonint MINUS expr                   { make_prim (int_minus, [$1;$3]) }
     | expr_nonint TIMES expr                   { make_prim (int_times, [$1;$3]) }
     | expr_nonint DIVIDE expr                   { make_prim (int_div, [$1;$3]) }
     | expr_nonint MOD expr                   { make_prim (int_mod, [$1;$3]) }
     | expr_nonint CONCAT expr                { make_prim (string_concat, [$1;$3]) }
     | expr_nonint PLUSPLUS                      { make_prim (int_succ, [$1]) }
     | expr_nonint MINUSMINUS                      { make_prim (int_pred, [$1]) }
     | expr_nonint EQUALSEQUALS expr           { make_code (Eq ($1, $3)) }
     | expr_nonint AND expr               { make_code (And ($1,$3)) }
     | expr_nonint BAR expr                { make_code (Or ($1,$3)) }
     | expr_nonint DOT LIDENT              { make_code (Dot ($1,$3)) }
     | expr_nonint COLON type_expr { clear_tyvars (); make_code (Typed ($1, quantify $3)) }



opt_decls :					{ [] }
     | decls                                    { $1 }

decls : decl opt_semicolon                 { [$1] }
     | decl opt_semicolon decls	           { $1 :: $3 }

let_decl :
     | LIDENT EQUALS expr                        { ($1,$3) }
     | LIDENT COLON type_expr EQUALS expr        { ($1, make_code (Typed ($5, quantify $3))) }
     | LIDENT LPAREN opt_vars RPAREN EQUALS expr { ($1, make_code (Fix ($1,$3,$6))) }
     | LIDENT LPAREN opt_vars RPAREN COLON type_expr EQUALS expr
	 { ($1, make_code (Typed (make_code (Fix ($1,$3,$8)), quantify $6))) }
     | LIDENT LIDENT EQUALS expr                 { ($1, make_code (Fix ($1,[$2],$4))) }
     | LIDENT LIDENT COLON type_expr EQUALS expr
	 { ($1, make_code (Typed (make_code (Fix ($1,[$2],$6)), quantify $4))) }


opt_given :
     |                { [] }
     | GIVEN longvars { $2 }

decide_decl :
     | DECIDE LIDENT values opt_given { ($2, ($3, $4)) }

decl :
     | let_decl                          { make_decl (Vardef (fst $1, snd $1, true)) }
     | PRIVATE let_decl                  { make_decl (Vardef (fst $2, snd $2, false)) }
     | OBSERVE obs            { make_decl (Obs (fst $2, snd $2)) }
     | RETRACT longvar                 { make_decl (Retract $2) }
     | PRAGMA string                   { make_decl (Pragma $2) }
     | REWARD float                     { make_decl (Rew (make_code True, [(Any, $2)])) }
     | REWARD CASE expr OF rew_clauses
	 { make_decl (Rew ($3, $5)) }
     | PARAM param_decl                 { make_decl (make_param $2) }
     | decide_decl         { make_decl (Dec (fst $1, fst (snd $1), snd (snd $1), true)) }
     | PRIVATE decide_decl { make_decl (Dec (fst $2, fst (snd $2), snd (snd $2), false)) }
     | TYPE UIDENT opt_paren_type_args EQUALS type_expr
	 { clear_tyvars (); make_decl (Tydef ($2, $3, $5)) }
     | DATA UIDENT opt_paren_type_args EQUALS tycon_expr
	 { clear_tyvars (); make_decl (Tycondef (new_name $2, $3, $5)) }

obs :  longvar { ($1, BVal true) }
     | NOT longvar { ($2, BVal false) }
     | longvar EQUALS pat { ($1, $3) }

values :
     |                              { [ Bool false; Bool true ] }
     | FROM strings                 { map (fun x -> Symbol x) $2 }
     | FROM integer DOTDOT integer  { map (fun x -> Int x) (list_of_ints $2 $4) }

opt_paren_type_args : opt_type_args { $1 }
     | LSQUARE opt_type_args RSQUARE { $2 }

opt_type_args : { [] }
     | type_args { $1 }

type_args : new_type_var { [$1] }
     | new_type_var COMMA type_args { $1 :: $3 }

param_decl :
     | ident EQUALS LSQUARE params RSQUARE 	 { ($1, $4) }
     | ident NATURAL  { ($1, make_list $2 0.0) }

param_decls : param_decl   { [$1] }
     | param_decl SEMICOLON param_decls { $1 :: $3 }

type_expr : arrow_type_expr { $1 }
     | other_type_expr { $1 }

arrow_type_expr :
     | arrow_type_exprs MAPSTO type_expr { Arrow ($1,$3,[]) }

other_type_expr :
     | SYMBOL { Types.Symbol }
     | BOOL { Types.Bool }
     | INT { Types.Int }
     | LTUP opt_named_type_exprs RTUP { Compound $2 }
     | LTUP type_exprs RTUP { make_anon_compound $2 }
     | LIDENT { Tyvar (tyvar_of_name $1) }
     | FORALL new_type_var DOT type_expr  { Forall ($2, $4) }
     | ulongvar opt_sqparen_type_exprs { Tycon ($1,$2) }
     | LPAREN type_expr RPAREN { $2 }

arrow_type_exprs :
     | LPAREN RPAREN { [] }
     | LPAREN type_expr RPAREN { [$2] }
     | LPAREN type_expr COMMA type_exprs RPAREN { $2 :: $4 }

tycon_expr : tycon_item { [$1] }
     | tycon_item BAR tycon_expr { $1 :: $3 }

tycon_item : UIDENT opt_paren_type_exprs { ($1, $2) }

opt_paren_type_exprs :
     | { [] }
     | type_expr { [$1] }
     | LPAREN type_expr COMMA type_exprs RPAREN { $2 :: $4 }

opt_sqparen_type_exprs : opt_type_exprs { $1 }
     | LSQUARE opt_type_exprs RSQUARE { $2 }

opt_paren_exprs : { [] }
     | expr { [$1] }
     | LPAREN expr COMMA exprs RPAREN { $2 :: $4 }

new_type_var : LIDENT { new_tyvar_for_name $1 }

opt_type_exprs : { [] }
     | type_exprs { $1 }

type_exprs : type_expr { [$1] }
     | type_expr COMMA type_exprs { $1 :: $3 }

named_type_expr : LIDENT COLON type_expr { ($1, $3) }

opt_named_type_exprs : { [] }
     | named_type_exprs  { $1 }

named_type_exprs : named_type_expr { [$1] }
     | named_type_expr COMMA named_type_exprs { $1 :: $3 }

clauses : clauses1    { $1 }
     | HASH clauses1  { $2 }

clauses1 : clause                        { [$1] }
     | clause HASH clauses1               { $1 :: $3 }

clause : pat COLON expr                { ($1, $3) }

rew_clauses : rew_clauses1 { $1 }
     | HASH rew_clauses1 { $2 }

rew_clauses1 : rew_clause               { [$1] }
     | rew_clause HASH rew_clauses1     { $1 :: $3 }

rew_clause : pat COLON float            { ($1, $3) }

pat : ANY                               { Any }
     | LIDENT                            { Bind ($1) }
     | string                           { SVals [$1] }
     | LBRACE string_choices RBRACE     { SVals $2 }
     | integer                          { IRange ($1,$1) }
     | integer DOTDOT integer           { IRange ($1,$3) }
     | TRUE                             { BVal true }
     | FALSE                            { BVal false }
     | LTUP opt_named_pats RTUP             { Tup $2 }
     | LTUP pats RTUP                   { AnonTup $2 }
     | ulongvar opt_paren_pats            { Conspat ($1,$2) }

integer : NATURAL { $1 }
     | MINUS NATURAL { -$2 }

string_choices : string    { [ $1 ] }
     | string BAR string_choices { $1 :: $3 }

opt_paren_pats : { [] }
     | pat { [$1] }
     | LPAREN pat COMMA pats RPAREN { $2 :: $4 }

opt_named_pats : { [] }
     | named_pats { $1 }

pats : pat                              { [$1] }
     | pat COMMA pats                   { $1 :: $3 }

named_pats : named_pat                  { [$1] }
     | named_pat COMMA named_pats       { $1 :: $3 }

named_pat : LIDENT COLON pat             { ($1, $3) }

exprs : expr                           { [$1] }
     | expr COMMA exprs                { $1 :: $3 }

opt_named_exprs :                           { [] }
     | named_exprs                     { $1 }

named_exprs : named_expr               { [$1] }
     | named_expr COMMA named_exprs    { $1 :: $3 }

named_expr : LIDENT COLON expr           { ($1, $3) }

weighted_exprs : weighted_expr         { [$1] }
     | weighted_expr COMMA weighted_exprs { $1 :: $3 }

weighted_expr : POSFLOAT COLON expr          { ($1,$3) }

params : POSFLOAT                          { [$1] }
     | POSFLOAT COMMA params               { $1 :: $3 }

opt_vars :                                  { [] }
     | vars                            { $1 }

vars : LIDENT                           { [$1] }
     | LIDENT COMMA vars            { $1 :: $3 }

ulongvar : UIDENT { [$1] }
     | LIDENT DOT ulongvar { $1 :: $3 }
     | UIDENT DOT ulongvar { $1 :: $3 }

longvar : LIDENT { [$1] }
     | LIDENT DOT longvar { $1 :: $3 }
     | UIDENT DOT longvar { $1 :: $3 }

longvars : longvar                    { [$1] }
     | longvar COMMA longvars         { $1 :: $3 }

string : QUOTE ident { $2 }
     | STRING { $1 }

strings : string { [ $1 ] }
     | string COMMA strings { $1 :: $3 }

opt_semicolon :    {}
     | SEMICOLON   {}

float : POSFLOAT { $1 }
     | MINUS POSFLOAT { -. $2 }

ident : LIDENT {$1}
     | UIDENT {$1}

