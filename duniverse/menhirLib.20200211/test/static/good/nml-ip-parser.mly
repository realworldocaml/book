/* parser.mly -- functional parser for IPLib language */
/* DM/MCFA  02/99 */

%{

open Symbol
open Ip_parsetree
open Ip_location

module Dep = Ip_depend

let mkindex d =
  { pindex_desc = d; pindex_loc = symbol_loc() }
let mkpat d =
  { ppat_desc = d; ppat_loc = symbol_loc() }
let mkexp d =
  { pexp_desc = d; pexp_loc = symbol_loc() }
let mkstr d =
  { pstr_desc = d; pstr_loc = symbol_loc() }
let mktyp d =
  { ptyp_desc = d; ptyp_loc = symbol_loc() }

let mkoperator name pos =
  { pexp_desc = Pexp_ident(Lident name); pexp_loc = rhs_loc pos }

(* Ghost expressions and patterns:
    expressions and patterns added by the parser;
    their location is an empty range of characters.
    The profiler doesn't instrument such expressions.
*)
let ghexp d point =
  { pexp_desc = d; pexp_loc = {loc_start = point; loc_end = point} }

let ghpat d point =
  { ppat_desc = d; ppat_loc = {loc_start = point; loc_end = point} }

let mkldot modname name =
  Ldot(symbol modname, symbol name)

let mkident modname name =
  mkexp(Pexp_ident(mkldot modname name))

let mkapply modname name arglst =
  mkexp(Pexp_apply((mkident modname name),arglst))

let ghident modname name point =
  ghexp (Pexp_ident(mkldot modname name)) point

let ghapply modname name arglst point =
  ghexp (Pexp_apply((ghident modname name point),arglst)) point

let mklazy e =
  let {loc_start = st} = symbol_loc () in
  let pat  = ghpat (Ppat_constant Const_unit) st in
  let fn   = ghexp (Pexp_function [pat,e]) st in
    ghapply "Pervasive" "memo" [fn] st

let mkassert e =
  let {loc_start = st; loc_end = en} = symbol_loc () in
  let triple = ghexp (Pexp_tuple
                       [ghexp (Pexp_constant (Const_string !input_name)) en;
                        ghexp (Pexp_constant (Const_int st)) en;
                        ghexp (Pexp_constant (Const_int en)) en]) en in
  let ex = mkldot "Pervasive" "Assert_failure" in
  let bucket = ghexp (Pexp_construct (ex, Some triple)) en in
  let raiser = ghexp (Pexp_raise(bucket)) en in
  let un = ghexp (Pexp_constant(Const_unit)) en in
  match e with
  | {pexp_desc = Pexp_constant(Const_int 0)} -> raiser
  | _ -> if !Ip_syntaxerr.noassert
         then un
         else mkexp(Pexp_ifthenelse (e, un, raiser))

let mkinfix arg1 name arg2 =
(*
  if name == plussym then
    (match arg1 with
	 {pexp_desc = Pexp_apply({pexp_desc = Pexp_ident(Lident op)},
				 [arg1a;arg1b])} when op == starsym ->
	   (match arg2 with
		{pexp_desc = Pexp_apply({pexp_desc = Pexp_ident(Lident op)},
					[arg2a;arg2b])} when op == starsym ->
		  (* a*b + c*d -> interp a b c d *)
		  mkexp(Pexp_apply(mkoperator interpsym 2,
				   [arg1a;arg1b;arg2a;arg2b]))
	      | _ ->
		  (* a*b+c -> mpadd c a b *)
		  mkexp(Pexp_apply(mkoperator mpaddsym 2,
				   [arg2;arg1a;arg1b])))
       | _ ->
	   (match arg2 with
		{pexp_desc = Pexp_apply({pexp_desc = Pexp_ident(Lident op)},
					[arg2a;arg2b])} when op == starsym ->
		  (* a+b*c -> mpadd a b c *)
		  mkexp(Pexp_apply(mkoperator mpaddsym 2,
				   [arg1;arg2a;arg2b]))
	      | _ ->
		  (* a + b *)
		  mkexp(Pexp_apply(mkoperator name 2, [arg1;arg2]))))

  else if name == starsym then (* a*(b-c) -> nuc a b c *)
    (match arg1,arg2 with
	 {pexp_desc = Pexp_apply({pexp_desc = Pexp_ident(Lident op)},
				 [arg1a;arg1b])},_ when op == minussym ->
	   (* (a-b)*c -> nuc a b c *)
	   mkexp(Pexp_apply(mkoperator nucsym 2,[arg1a;arg1b;arg2]))
       | _,{pexp_desc = Pexp_apply({pexp_desc = Pexp_ident(Lident op)},
				   [arg2a;arg2b])} when op == minussym ->
	   (* a*(b-c) -> nuc b c a *)
	   mkexp(Pexp_apply(mkoperator nucsym 2,[arg2a;arg2b;arg1]))
       | _ ->
	   (* a*b *)
	   mkexp(Pexp_apply(mkoperator name 2, [arg1;arg2])))
  else
*)
    mkexp(Pexp_apply(mkoperator name 2, [arg1;arg2]))

let mkuminus name arg =
  match arg.pexp_desc with
    Pexp_constant(Const_int n) ->
      mkexp(Pexp_constant(Const_int(-n)))
  | Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float(-. f)))
  | _ ->
      mkexp(Pexp_apply(mkoperator name 1, [arg]))

let mkstrexp e =
  { pstr_desc = Pstr_eval e; pstr_loc = e.pexp_loc }

let rec mkrangepat c1 c2 =
  if c1 > c2 then mkrangepat c2 c1
  else if c1 = c2 then mkpat(Ppat_constant(Const_char c1))
  else mkpat(Ppat_char_range(c1,c2))

(* --------------------------------------------- *)
(* Let and TopLet bindings... we use a source	 *)
(* transformation trick to achieve the all or	 *)
(* nothing effect of "let x = ... and y = ..."	 *)
(* and "let rec x = ... and y = ...". In the  	 *)
(* first case, we bind none immediately, while	 *)
(* in the second case we bind all immediately.	 *)
(*					      	 *)
(* In either case:     	       	       	      	 *)
(*		  				 *)
(*  let {rec} x = ... y = ...  			 *)
(*     	       	       	      	 		 *)
(* is equivalent to                              *)
(*    					       	 *)
(*  let {rec} [x; y] = [x_expr; y_expr] ...      *)
(*     	       	       	       	       	       	 *)
(* --------------------------------------------- *)
let mklistpat lst  = mkpat(Ppat_list lst)
let mklistexpr lst = mkexp(Pexp_list lst)

let mktoplet recflg patlist =
  let pats,exprs = List.split patlist in
  let pat' = mklistpat pats in
  let expr' = mklistexpr exprs in
    (* here we keep everything as lists because the toplevel *)
    (* bindings get their lambda lists printed *)
    mkstrexp(mkexp(Pexp_toplet(recflg,(pat',expr'))))

let mklet recflg patlist body =
  let pats,exprs = List.split patlist in
  let pat' = mklistpat pats in
  let expr' = mklistexpr exprs in
    mkexp(Pexp_let(recflg, (pat', expr'), body))

let mktuple = function
    [exp] -> exp
  | lst   -> mkexp(Pexp_tuple lst)

let mktuplepat = function
    [pat] -> pat
  | lst   -> mkpat(Ppat_tuple lst)

let mklistpat tl patlst =
  List.fold_left
    (fun tl pat ->
       match pat with
	   {ppat_desc = Ppat_restcons(id,_)} ->
	     mkpat(Ppat_restcons(id, tl))
	 | _ ->
	     mkpat(Ppat_cons(pat,tl)))
    tl patlst

(* -- Haskell-style List Comprehensions -- *)
let rec make_comprehension body gens initial =
  let rsltid = Symbol.symbol " rslt" in
  let rsltpat = mkpat(Ppat_ident rsltid) in
  let rslt  = mkexp(Pexp_ident (Lident rsltid)) in
  let generate_body pat lst tst rslt' =
    let body' =
      match tst with
	  None   -> rslt'
	| Some t -> mkexp(Pexp_when(t,rslt'))
    in
    let fn =
      match tst,pat with
	  None,{ppat_desc = Ppat_ident _} ->  (* irrefutable pattern *)
	    mkexp(
	      Pexp_function
		    [mktuplepat[pat;rsltpat],body'])
	| _ -> (* refutable pattern *)
	    mkexp(
	      Pexp_function
		    [mktuplepat[pat;rsltpat],body';
		     mktuplepat[mkpat(Ppat_any);rsltpat],rslt])
    in
      mkexp(Pexp_compre(fn,lst,initial))
  in
    match gens with
      | [(pat,lst,tst)] ->
	  generate_body pat lst tst
	    (mkinfix body conssym rslt)
      | (pat,lst,tst)::tl ->
	  generate_body pat lst tst
	    (make_comprehension body tl rslt)
      | _ -> Ip_types.cant_happen "make_comprehension"

let syntax_error () =
  raise Ip_syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Ip_syntaxerr.Error
	  (Ip_syntaxerr.Unclosed
	     (rhs_loc opening_num, opening_name,
              rhs_loc closing_num, closing_name)))

%}

%token<int>    INT
%token<float>  FLOAT
%token<float>  IMAGINARY
%token<string> STRING
%token<Symbol.symbol> INFIXOP1R
%token<Symbol.symbol> INFIXOP1L
%token<Symbol.symbol> INFIXOP2R
%token<Symbol.symbol> INFIXOP2L
%token<Symbol.symbol> INFIXOP3R
%token<Symbol.symbol> INFIXOP3L
%token<Symbol.symbol> INFIXOP4R
%token<Symbol.symbol> INFIXOP4L
%token<Symbol.symbol> INFIXOP5R
%token<Symbol.symbol> INFIXOP5L
%token<Symbol.symbol> PREFIXOP
%token<Symbol.symbol> LIDENT UIDENT TIDENT
%token<char>   CHAR

%token EQ OF LMATRIX LVECTOR WHERE AMPEROPT AMPERKEY AMPERREST
%token LET IN IF THEN ELSE FUN FUNCTION MATCH WITH WHEN TRY RAISE
%token AND AS REC ASSERT VAL
%token OPEN EXCEPTION MODULE
%token LAZY
%token COLONEQUAL
%token TYPE BEGIN END
%token WHILE FOR TO DOWNTO ABOVE BELOW DO DONE
%token COLON SEMI COMMA DOT
%token DOTDOT DOTDOTDOT COLONCOLON SEMISEMI SHARP DOLLAR
%token PLUS MINUS STAR
%token BAR BARBAR AMPERAMPER
%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE
%token MINUSGREATER UNDERSCORE LESSMINUS
%token LBRACKBAR BARRBRACK
%token EOF
%token CASE
%token MUTABLE
%token QUESTION BARCOLON
/* %token CLASS METHOD */

%token INTPAT FLOATPAT COMPLEXPAT NUMBERPAT REALPAT STRINGPAT
%token SYMBOLPAT LISTPAT TUPLEPAT ARRAYPAT FARRAYPAT CARRAYPAT
%token CHARPAT RECORDPAT BOXPAT VECTORPAT
%token HASHTABLEPAT STACKPAT QUEUEPAT WEAKARRAYPAT
%token IN_CHANNELPAT, OUT_CHANNELPAT, DIR_HANDLEPAT

/* Precedences and associativities. Lower precedences come first. */

%right prec_let                         /* let ... in ... */
%right SEMI                             /* e1; e2 (sequence) */
%right prec_fun prec_match prec_try     /* match ... with ... */
%right prec_list                        /* e1; e2 (list, array, record) */
%right prec_if                          /* if ... then ... else ... */
%right COLONEQUAL LESSMINUS             /* assignments */
%left  AS                               /* as in patterns */
%left  BAR                              /* | in patterns */
%left  COMMA                            /* , in expressions, patterns, types */
%right QUESTION
%right BARBAR                           /* or */
%right AMPERAMPER                       /* & */
%left  EQ INFIXOP1L                     /* = < > etc */
%right INFIXOP1R
%left  INFIXOP2L
%right INFIXOP2R                        /* @ ^ etc */
%right COLONCOLON                       /* :: */
%left  PLUS MINUS INFIXOP3L             /* + - */
%right INFIXOP3R
%left  STAR INFIXOP4L                   /* * / */
%right INFIXOP4R
%right prec_unary_minus                 /* - unary */
%left  INFIXOP5L
%right INFIXOP5R                        /* ** */
%left  prec_appl                        /* function application */
%right prec_constr_appl                 /* constructor application */
%left  DOT                              /* record access, array access */
%right PREFIXOP                         /* ! */

/* Entry points */

%start toplevel_phrase
%type<Ip_parsetree.structure_item list * Symbol.symbol list> toplevel_phrase
%start use_file
%type<Ip_parsetree.structure_item list * Symbol.symbol list> use_file

%%

toplevel_phrase:
    use_file_exprs EOF
     { List.rev $1, Dep.needed_modules() }
  | /* nothing */
      { [], [] }
;
use_file:
  use_file_exprs EOF
    {List.rev $1, Dep.needed_modules() }

use_file_exprs:
    top_expr
      { [$1] }
  | use_file_exprs top_expr
      { $2::$1 }

top_expr:
  | SEMISEMI
      { mkstr(Pstr_empty) }
  | LET rec_flag let_bindings %prec prec_let
      { mktoplet $2 (List.rev $3) }

      /* alternatives to top-level let expressions */
/*
  | FUN let_bindings %prec prec_let
      { mktoplet Recursive (List.rev $2) }
  | VAL let_bindings %prec prec_let
      { mktoplet Recursive (List.rev $2) }
*/
  | EXCEPTION UIDENT OF type_expr
      { mkstr(Pstr_exception($2,true)) }
  | EXCEPTION UIDENT
      { mkstr(Pstr_exception($2,false)) }


  | TYPE type_vars LIDENT EQ opt_bar tag_list
      { mkstr(Pstr_tags($3,List.rev $6)) }
  | TYPE LIDENT EQ opt_bar tag_list
      { mkstr(Pstr_tags($2,List.rev $5)) }

  | TYPE LIDENT EQ LBRACE lbl_type_list opt_comma RBRACE
      { mkstr(Pstr_recdef($2,List.rev $5)) }
  | TYPE LIDENT EQ LBRACE lbl_type_list opt_comma error
      { unclosed "{" 4 "}" 7 }

/*
  | CLASS LIDENT COLON ident_comma_list EQ LBRACE slot_list
      opt_comma RBRACE
      { mkstr(Pstr_class($2, List.rev $4, List.rev $7)) }
  | CLASS LIDENT COLON ident_comma_list EQ LBRACE slot_list
      opt_comma error
      { unclosed "{" 6 "}" 9 }
  | CLASS LIDENT EQ LBRACE slot_list opt_comma RBRACE
      { mkstr(Pstr_class($2, [], List.rev $5)) }
  | CLASS LIDENT EQ LBRACE slot_list opt_comma error
      { unclosed "{" 4 "}" 7 }
*/

  | MODULE mod_ident EQ mod_ident
      { Dep.add_equiv $2 $4;
	mkstr(Pstr_module_abbrev($2,$4)) }
  | OPEN mod_ident
      { Dep.add $2;
	mkstr(Pstr_open $2) }
  | seq_expr
      { mkstrexp $1 }
;
/*
ident_comma_list:
    label_longident
      {[$1]}
  | ident_comma_list COMMA label_longident
      { $3 :: $1 }
;
slot_list:
    slot_defn
      { [$1] }
  | slot_list COMMA slot_defn
      { $3 :: $1 }
;
slot_defn:
    basic_slot_defn
      { ($1,(Ip_types.Instance_slot,Ip_types.Immutable)) }
  | CLASS basic_slot_defn
      { ($2,(Ip_types.Class_slot,Ip_types.Immutable)) }
  | MUTABLE basic_slot_defn
      { ($2,(Ip_types.Instance_slot,Ip_types.Mutable)) }
  | MUTABLE CLASS basic_slot_defn
      { ($3,(Ip_types.Class_slot,Ip_types.Mutable)) }
  | CLASS MUTABLE basic_slot_defn
      { ($3,(Ip_types.Class_slot,Ip_types.Mutable)) }
;
basic_slot_defn:
    LIDENT COLON type_expr
      { $1 }
;
*/
tag_list:
    simple_tag_type %prec prec_list
     { [$1] }
  | tag_list BAR simple_tag_type %prec prec_list
      { $3 :: $1 }
;
simple_tag_type:
    UIDENT OF type_expr
      { ($1,true) }
  | UIDENT
      { ($1,false) }
;
type_vars:
    TIDENT
      { [$1] }
  | LPAREN type_list RPAREN
      { $2 }
;
type_list:
    TIDENT  %prec prec_list
      {[$1]}
  | type_list COMMA TIDENT %prec prec_list
      {$3::$1}
;
type_expr:
    label_longident
      { mktyp(Ptyp_ident $1)}
  | TIDENT
      {mktyp(Ptyp_ident(Tident $1))}
/*
  | LPAREN type_list RPAREN val_longident
      { mktyp(Ptyp_parm_constr($4,List.rev $2))}
*/
  | type_expr label_longident
      {mktyp(Ptyp_constr($2,$1))}
  | type_expr STAR type_expr
      {mktyp(Ptyp_prod($1,$3))}
  | type_expr MINUSGREATER type_expr %prec LESSMINUS
      {mktyp(Ptyp_arrow($1,$3))}
  | LPAREN type_expr RPAREN
      {$2}
  | LPAREN type_expr error
      { unclosed "(" 1 ")" 3 }
;
field_defn:
    LIDENT COLON type_expr
      { $1,Ip_types.Immutable }
  | MUTABLE LIDENT COLON type_expr
      { $2,Ip_types.Mutable }
;
lbl_type_list:
    field_defn  %prec prec_list
      { [$1] }
  | lbl_type_list COMMA field_defn  %prec prec_list
      { $3 :: $1 }
;
rec_flag:
   /* empty */
     { Nonrecursive }
  | REC
      { Recursive }

expr:
  | value_expr
      { $1 }
  | no_value_expr
      { $1 }
;
no_value_expr:
  | RAISE expr
      { mkexp(Pexp_raise($2)) }
  | WHILE expr DO seq_expr DONE
      { mkexp(Pexp_while($2,$4)) }
  | FOR LIDENT EQ expr direction_flag expr DO seq_expr DONE
      { mkexp(Pexp_for($2,$4,$5,$6,$8)) }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexp(Pexp_setfield($1,$3,$5)) }
  | simple_expr DOT LBRACK index_list RBRACK LESSMINUS expr
      { mkexp(Pexp_setindex($1,List.rev $4,$7)) }
  | expr COLONEQUAL expr
      { mkinfix $1 colonequalsym $3 }
  | ASSERT simple_expr %prec prec_appl
      { mkassert $2 }
;
value_expr:
    simple_expr
      { $1 }
  | simple_expr simple_expr_list %prec prec_appl
      { mkexp(Pexp_apply($1,List.rev $2)) }
  | LET rec_flag let_bindings IN seq_expr  %prec prec_let
      { mklet $2 (List.rev $3) $5 }
  | LET MODULE mod_ident EQ mod_ident IN seq_expr %prec prec_let
      { Dep.subst $5 $3;
	mkexp(Pexp_letmod($3,$5,$7)) }
  | FUN simple_pattern fun_def %prec prec_fun
      { mkexp(Pexp_function[$2,$3]) }
  | FUNCTION opt_bar match_cases %prec prec_fun
      { mkexp(Pexp_function(List.rev $3)) }
  | TRY seq_expr WITH opt_bar match_cases %prec prec_try
      { mkexp(Pexp_try($2,List.rev $5)) }
  | TRY seq_expr WITH error %prec prec_try
      { syntax_error() }
  | IF expr THEN expr ELSE expr %prec prec_if
      { mkexp(Pexp_ifthenelse($2,$4,$6)) }
  | IF expr THEN expr  %prec prec_if
      { mkexp(Pexp_ifthenelse($2,$4,ghexp (Pexp_constant Const_unit) 0)) }

  | WHERE expr THEN expr ELSE expr %prec prec_if
      { mkexp(Pexp_where($2, Some $4, Some $6)) }
  | WHERE expr %prec prec_if
      { mkexp(Pexp_where($2, None, None)) }
  | expr WHERE expr %prec prec_if
      { mkexp(Pexp_where($3, Some $1, None)) }

  | MATCH expr WITH opt_bar match_cases %prec prec_match
      { mkexp(Pexp_match($2,List.rev $5)) }
  | MATCH expr_comma_list opt_comma WITH opt_bar match_cases %prec prec_match
      { mkexp(Pexp_match(mkexp(Pexp_tuple(List.rev $2)),List.rev $6)) }
  | CASE opt_bar case_list
      { mkexp(Pexp_case(List.rev $3)) }
  | constr_longident simple_expr %prec prec_constr_appl
      { mkexp(Pexp_construct($1,Some $2)) }
  | expr QUESTION expr COLON expr
      { mkexp(Pexp_ifthenelse($1,$3,$5)) }
  | expr COLONCOLON expr
      { mkinfix $1 conssym $3 }
  | expr INFIXOP1L expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2L expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3L expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4L expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP5L expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP1R expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2R expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3R expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4R expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP5R expr
      { mkinfix $1 $2 $3 }
  | expr PLUS expr
      { mkinfix $1 plussym $3 }
  | expr MINUS expr
      { mkinfix $1 minussym $3 }
  | MINUS expr %prec prec_unary_minus
      { mkuminus negsym $2 }
  | PLUS expr %prec prec_unary_minus
      { $2 }
  | expr STAR expr
      { mkinfix $1 starsym $3 }
  | expr EQ expr
      { mkinfix $1 eqsym $3 }
  | expr BARBAR expr
      { mkexp(Pexp_scor($1,$3)) }
/*
  | expr OR expr
      { mkexp(Pexp_scor($1,$3)) }
*/
  | expr AMPERAMPER expr
      { mkexp(Pexp_scand($1,$3)) }
/*
  | expr AND expr
      { mkexp(Pexp_scand($1,$3)) }
*/
  | LAZY simple_expr %prec prec_appl
      { mklazy $2 }
;
case_list:
    simple_case %prec prec_list
      {[$1]}
  | case_list BAR simple_case %prec prec_list
      { $3::$1 }
;
simple_case:
    expr MINUSGREATER expr
      { $1,$3 }
  | UNDERSCORE MINUSGREATER expr
      { mkexp(Pexp_constant(Const_int 1)), $3 }
;
expr_comma_list:
    expr_comma_list_element %prec prec_list
      {$1}
  | expr_comma_list COMMA expr_comma_list_element %prec prec_list
      {$3@$1}
;
expr_comma_list_element:
    expr
      {[$1]}
  | colonized_symbol COLON expr
      {[$3; mkexp(Pexp_constant (Const_symbol $1))]}
;
colonized_symbol:
    LIDENT
      {$1}
  | UIDENT
      {$1}
;
basic_seq_expr:
    expr
      { [$1] }
  | basic_seq_expr SEMI expr
      { $3 :: $1 }
  | basic_seq_expr SEMI
      { $1 }
;
seq_expr:
    basic_seq_expr
      { match $1 with
	    [e] -> e
	  | _ ->
	      mkexp(Pexp_sequence(List.rev $1)) }
;
simple_expr:
  | val_longident
      { mkexp(Pexp_ident $1) }
  | constant
      { mkexp(Pexp_constant $1) }
  | constr_longident
      { mkexp(Pexp_construct($1,None)) }
  | LPAREN expr_comma_list opt_comma RPAREN
      { mktuple (List.rev $2) }
  | LPAREN expr_comma_list BAR expr RPAREN
      { mkexp(Pexp_as_tuple(mkinfix (mkexp(Pexp_list(List.rev $2)))
			      appendsym $4)) }
  | LPAREN expr SEMI basic_seq_expr RPAREN
      { mkexp(Pexp_sequence($2::(List.rev $4))) }
  | LPAREN expr SEMI basic_seq_expr error
      { unclosed "(" 1 ")" 5 }
  | LPAREN expr_comma_list error
      { unclosed "(" 1 ")" 3 }
  | BEGIN seq_expr END
      { $2 }
  | BEGIN seq_expr error
      { unclosed "begin" 1 "end" 3 }
  | simple_expr DOT label_longident
      { mkexp(Pexp_field($1,$3)) }
  | simple_expr DOT LBRACK index_list RBRACK
      { mkexp(Pexp_getindex($1,List.rev $4)) }
  | simple_expr DOT LBRACK index_list error
      { unclosed "[" 3 "]" 5 }
  | LBRACKBAR expr_comma_list opt_comma BARRBRACK
      { mkexp(Pexp_array(List.rev $2)) }
  | LBRACKBAR expr_comma_list opt_comma error
      { unclosed "[|" 1 "|]" 4 }
  | LBRACE record_expr opt_comma RBRACE
      { $2 }
  | LBRACE record_expr opt_comma error
      { unclosed "{" 1 "}" 4 }
  | LVECTOR list_expr RBRACK
      { mkexp(Pexp_vector($2)) }
  | LVECTOR list_expr error
      { unclosed "#[" 1 "]" 3 }
  | LBRACK list_expr RBRACK
      { $2 }
  | LBRACK list_expr error
      { unclosed "[" 1 "]" 3 }
  | LMATRIX matrix_expr RBRACK
      { mkexp(Pexp_matrix(List.rev $2)) }
  | LMATRIX matrix_expr error
      { unclosed "#m[" 1 "]" 3 }
  | PREFIXOP simple_expr
      { mkexp(Pexp_apply(mkoperator $1 1, [$2])) }
;
matrix_expr:
  | expr_comma_list
      { [mkexp(Pexp_list(List.rev $1))] }
  | matrix_expr SEMI expr_comma_list
      { mkexp(Pexp_list(List.rev $3)) :: $1 }
;
list_expr:
  | expr BARCOLON generator_list
      { make_comprehension $1 (List.rev $3)
	  (mkexp(Pexp_constant Const_empty_list)) }
  | expr_comma_list opt_comma
      { mkexp(Pexp_list(List.rev $1)) }
  | expr_comma_list opt_comma DOTDOT expr
      { match $1 with
	    [b;a] -> mkexp(Pexp_range(a,Some b,$4))
	  | [a]   -> mkexp(Pexp_range(a,None,$4))
	  | _ -> syntax_error()}
  | expr_comma_list BAR expr
      { mkinfix (mkexp(Pexp_list(List.rev $1))) appendsym $3 }
;
generator_list:
  | generator
      { [$1] }
  | generator_list SEMI generator
      { $3 :: $1 }
;
generator:
  | pattern LESSMINUS expr
      { $1,$3,None }
  | pattern LESSMINUS expr COMMA expr
      { $1,$3,Some $5 }
;
simple_expr_list:
    simple_expr
      {[$1]}
  | simple_expr_list simple_expr
      {$2::$1}
;
index_list:
    index
      {[$1]}
  | index_list COMMA index
      {$3 :: $1}
;
index:
    expr
      { mkindex(Pindex_expr $1) }
  | STAR
      { mkindex(Pindex_range_all) }
  | expr COLON expr
      { mkindex(Pindex_range($1,$3)) }
  | expr COLON STAR
      { mkindex(Pindex_range_to_end($1)) }
  | DOTDOT
      { mkindex(Pindex_range_all) }
  | expr DOTDOT expr
      { mkindex(Pindex_range($1,$3)) }
  | DOTDOT expr
      { mkindex(Pindex_range(mkexp(Pexp_constant(Const_int 0)),$2)) }
  | expr DOTDOT
      { mkindex(Pindex_range_to_end($1)) }
;
let_bindings:
    let_binding
      {[$1]}
  | let_bindings AND let_binding
      { $3 :: $1 }
;
let_binding:
    val_ident fun_binding
      { ({ppat_desc = Ppat_ident $1; ppat_loc = rhs_loc 1},$2) }
  | pattern_comma_list EQ seq_expr %prec prec_let
      { match $1 with
	    [pat] -> (pat,$3)
	  | _ -> (mkpat(Ppat_tuple(List.rev $1)),$3) }
;
fun_binding:
    EQ seq_expr  %prec prec_let
      { $2 }
  | simple_pattern fun_binding
      { mkexp(Pexp_function[$1,$2]) }
;
label_longident:
    LIDENT
      { Lident($1) }
  | UIDENT DOT LIDENT
      { Dep.add $1;
	Ldot($1,$3) }
;
lbl_expr_list:
    label_longident EQ expr %prec prec_list
      { [$1,$3] }
  | lbl_expr_list COMMA label_longident EQ expr %prec prec_list
      { ($3,$5) :: $1 }
;
record_expr:
    simple_expr WITH lbl_expr_list
      { mkexp(Pexp_record(Some $1, List.rev $3)) }
  | lbl_expr_list
      { match $1 with
	    [] -> mkexp(Pexp_constant(Const_empty_record))
	  | _  -> mkexp(Pexp_record(None, List.rev $1)) }
;
fun_def:
    match_action                                { $1 }
  | simple_pattern fun_def                      { mkexp(Pexp_function[$1,$2]) }
;
match_action:
    MINUSGREATER seq_expr                       { $2 }
  | WHEN expr MINUSGREATER seq_expr         { mkexp(Pexp_when($2,$4)) }
;
match_cases:
  | pattern_comma_list opt_comma match_action
      { match $1 with
	    [pat] -> [pat,$3]
	  | _ -> [mkpat(Ppat_tuple(List.rev $1)),$3] }
  | match_cases BAR pattern_comma_list opt_comma match_action
      { match $3 with
	    [pat] -> (pat,$5) :: $1
	  | _ -> (mkpat(Ppat_tuple(List.rev $3)),$5) :: $1 }
;
constant:
    INT                                         { Const_int $1 }
  | CHAR                                        { Const_char $1 }
  | STRING                                      { Const_string $1 }
  | FLOAT                                       { Const_float $1 }
  | LPAREN RPAREN                               { Const_unit }
  | LBRACK RBRACK                               { Const_empty_list }
  | LBRACE RBRACE                               { Const_empty_record }
  | SHARP STRING                                { Const_symbol (symbol $2) }
  | SHARP LIDENT                                { Const_symbol $2 }
  | SHARP UIDENT                                { Const_symbol $2 }
  | LVECTOR RBRACK                         { Const_empty_vector }
  | DOLLAR STRING                               { Const_symbol (symbol $2) }
  | DOLLAR LIDENT                               { Const_symbol $2 }
  | DOLLAR UIDENT                               { Const_symbol $2 }
  | LBRACKBAR BARRBRACK                         { Const_empty_array }
  | IMAGINARY                                   { Const_complex(0.0, $1) }
;

signed_constant:
    constant                                    { $1 }
  | MINUS INT                             { Const_int(- $2) }
  | MINUS FLOAT                           { Const_float(-. $2) }
;

/* Identifiers and long identifiers */

val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
;
operator:
  | PLUS                                        { plussym }
  | MINUS                                       { minussym }
  | STAR                                        { starsym }
  | EQ                                          { eqsym }
  | COLONCOLON                                  { conssym }
  | COLONEQUAL                                  { colonequalsym }
  | SHARP                                       { sharpsym }
  | DOLLAR                                      { dollarsym }
  | PREFIXOP                                    { $1 }
  | INFIXOP1L                                   { $1 }
  | INFIXOP2L                                   { $1 }
  | INFIXOP3L                                   { $1 }
  | INFIXOP4L                                   { $1 }
  | INFIXOP5L                                   { $1 }
  | INFIXOP1R                                   { $1 }
  | INFIXOP2R                                   { $1 }
  | INFIXOP3R                                   { $1 }
  | INFIXOP4R                                   { $1 }
  | INFIXOP5R                                   { $1 }
;
val_longident:
    val_ident                                   { Lident $1 }
  | UIDENT DOT val_ident                        { Dep.add $1;
						  Ldot($1, $3) }
;
constr_longident:
    UIDENT DOT UIDENT
      { Dep.add $1;
	Ldot($1,$3) }
  | UIDENT
      { Lident($1) }
;
mod_ident:
    UIDENT
      { $1 }
;

/* Patterns */

pattern:
    simple_pattern
      { $1 }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct($1,Some $2)) }
  | built_in_constr %prec prec_constr_appl
      { $1 }
  | pattern AS LIDENT
      { mkpat(Ppat_alias($1,$3)) }
  | pattern COLONCOLON pattern
      { mkpat(Ppat_cons($1,$3)) }
  | pattern BARBAR pattern %prec BAR
      { mkpat(Ppat_or($1,$3)) }
;

simple_pattern:
    val_ident
      { mkpat(Ppat_ident $1) }
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | CHAR DOTDOT CHAR
      { mkrangepat $1 $3 }
  | constr_longident
      { mkpat(Ppat_construct($1,None)) }

    /* record patterns */
  | LBRACE lbl_pattern_list opt_comma RBRACE
      { mkpat(Ppat_record(List.rev $2)) }
  | LBRACE lbl_pattern_list error
      { unclosed "{" 1 "}" 4 }

      /* list patterns */
  | LBRACK list_pattern RBRACK
      { $2 }
  | LBRACK list_pattern error
      { unclosed "[" 1 "]" 4 }

      /* tuple patterns */
  | LPAREN list_pattern RPAREN %prec prec_list
      { match $2 with
	    {ppat_desc = Ppat_list l} ->
	      if List.length l > 1 then
		mkpat(Ppat_tuple(l))
	      else List.hd l
	  | _ -> mkpat(Ppat_tuplecons($2)) }
  | LPAREN pattern_comma_list COMMA param_tail RPAREN %prec prec_list
      { mkpat(Ppat_tuplecons
		(mklistpat(mkpat(Ppat_optional(List.rev $4))) $2)) }
  | LPAREN param_tail RPAREN
      { mkpat(Ppat_tuplecons(mkpat(Ppat_optional(List.rev $2)))) }
  | LPAREN list_pattern error
      { unclosed "(" 1 ")" 4 }

  | LVECTOR list_pattern RBRACK
      { mkpat(Ppat_vector($2)) }
  | LVECTOR list_pattern error
      { unclosed "#[" 1 "]" 4 }
;
param_tail:
    AMPEROPT option_pattern_list
      { $2 }
  | AMPEROPT option_pattern_list COMMA AMPERKEY key_pattern
      { $5 @ $2 }
  | AMPERKEY key_pattern
      { $2 }
;
pattern_comma_list:
    pattern_comma_list_element
      { [$1] }
  | pattern_comma_list COMMA pattern_comma_list_element %prec prec_list
      { $3 :: $1 }
;
pattern_comma_list_element:
    pattern
      { $1 }
  | AMPERREST LIDENT
      { mkpat(Ppat_restcons($2, mkpat(Ppat_null))) }
;
list_pattern:
  | pattern_comma_list COMMA DOTDOTDOT AS LIDENT
      { mklistpat (mkpat(Ppat_ident $5)) $1 }
  | pattern_comma_list COMMA DOTDOTDOT
      { mklistpat (mkpat(Ppat_rest)) $1 }

  | pattern_comma_list BAR pattern
      { mklistpat $3 $1 }

  | pattern_comma_list opt_comma
      { if List.exists
	  (function
	       {ppat_desc = Ppat_restcons _} -> true
	     | _ -> false)
	  $1
	then
	  match List.hd $1 with
	      {ppat_desc = Ppat_restcons(id,_)} ->
		mklistpat (mkpat(Ppat_ident id)) (List.tl $1)
	    | _ ->
		mklistpat (mkpat(Ppat_null)) $1
	else
	  mkpat(Ppat_list(List.rev $1)) }
  | DOTDOTDOT AS LIDENT
      { mkpat(Ppat_ident $3) }
  | DOTDOTDOT
      { mkpat(Ppat_rest) }
;

option_pattern_list:
    option_pattern_list_element
      { [$1] }
  | option_pattern_list COMMA option_pattern_list_element %prec prec_list
      { $3 :: $1 }
;
option_pattern_list_element:
    LIDENT
      { Arg_Optional($1,None) }
  | LIDENT EQ value_expr
      { Arg_Optional($1, Some $3) }
  | AMPERREST LIDENT
      { Arg_Rest $2 }
;
key_pattern:
    key_pattern_list
      { $1 }
  | key_pattern_list COMMA DOTDOTDOT
      { Arg_OtherKeys None :: $1 }
  | key_pattern_list COMMA DOTDOTDOT AS LIDENT
      { Arg_OtherKeys (Some $5) :: $1 }
  | DOTDOTDOT
      { [Arg_OtherKeys None] }
  | DOTDOTDOT AS LIDENT
      { [Arg_OtherKeys(Some $3) ] }
;
key_pattern_list:
    key_pattern_list_element
      { [$1] }
  | key_pattern_list COMMA key_pattern_list_element %prec prec_list
      { $3 :: $1 }
;
key_pattern_list_element:
    LIDENT
      { Arg_Key($1,None) }
  | LIDENT EQ value_expr
      { Arg_Key($1, Some $3) }
  | AMPERREST LIDENT
      { Arg_Rest($2) }
;
lbl_pattern_list:
    lbl_pattern_list_element
       { [$1] }
  | lbl_pattern_list COMMA lbl_pattern_list_element %prec prec_list
      { $3 :: $1 }
;
lbl_pattern_list_element:
    label_longident EQ pattern
       { ($1,$3) }
;
built_in_constr:
    INTPAT bi_pattern
      { mkpat(Ppat_bi_int $2) }
  | FLOATPAT bi_pattern
      { mkpat(Ppat_bi_float $2) }
  | COMPLEXPAT LPAREN bi_pattern COMMA bi_pattern RPAREN
      { mkpat(Ppat_bi_complexpair($3,$5)) }
  | COMPLEXPAT bi_pattern
      { mkpat(Ppat_bi_complex $2) }
  | NUMBERPAT bi_pattern
      { mkpat(Ppat_bi_number $2) }
  | REALPAT bi_pattern
      { mkpat(Ppat_bi_real $2) }
  | STRINGPAT bi_pattern
      { mkpat(Ppat_bi_string $2) }
  | SYMBOLPAT bi_pattern
      { mkpat(Ppat_bi_symbol $2) }
  | LISTPAT pattern
      { mkpat(Ppat_bi_list $2) }
  | VECTORPAT pattern
      { mkpat(Ppat_bi_vector $2) }
  | TUPLEPAT pattern
      { mkpat(Ppat_bi_tuple $2) }
  | ARRAYPAT bi_pattern
      { mkpat(Ppat_bi_array $2) }
  | ARRAYPAT LPAREN pattern COMMA bi_pattern RPAREN
      { mkpat(Ppat_bi_arrayd($3,$5)) }
  | FARRAYPAT bi_pattern
      { mkpat(Ppat_bi_farray $2) }
  | FARRAYPAT LPAREN pattern COMMA bi_pattern RPAREN
      { mkpat(Ppat_bi_farrayd($3,$5)) }
  | CARRAYPAT bi_pattern
      { mkpat(Ppat_bi_carray $2) }
  | CARRAYPAT LPAREN pattern COMMA bi_pattern RPAREN
      { mkpat(Ppat_bi_carrayd($3,$5)) }
  | CHARPAT bi_pattern
      { mkpat(Ppat_bi_char $2) }
  | RECORDPAT pattern
      { mkpat(Ppat_bi_record $2) }
  | BOXPAT bi_pattern
      { mkpat(Ppat_bi_box $2) }
  | BOXPAT LPAREN pattern COMMA pattern RPAREN
      { mkpat(Ppat_bi_boxd($3,$5)) }
  | HASHTABLEPAT bi_pattern
      { mkpat(Ppat_bi_hashtable $2) }
  | STACKPAT bi_pattern
      { mkpat(Ppat_bi_stack $2) }
  | QUEUEPAT bi_pattern
      { mkpat(Ppat_bi_queue $2) }
  | WEAKARRAYPAT bi_pattern
      { mkpat(Ppat_bi_weak $2) }
  | IN_CHANNELPAT bi_pattern
      { mkpat(Ppat_bi_inchan $2) }
  | OUT_CHANNELPAT bi_pattern
      { mkpat(Ppat_bi_outchan $2) }
  | DIR_HANDLEPAT bi_pattern
      { mkpat(Ppat_bi_dirhandle $2) }
;
bi_pattern:
    simple_bi_pattern
      { $1 }
  | LPAREN simple_bi_pattern RPAREN
      { $2 }
;
simple_bi_pattern:
    UNDERSCORE
      { mkpat(Ppat_any) }
  | LIDENT
      { mkpat(Ppat_ident $1) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
;
/* Optional elements */

opt_comma:
    /* empty */  { () }
  | COMMA        { () }
;
opt_semi:
    /* empty */  { () }
  | SEMI         { () }
;
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
  | ABOVE                                       { Above }
  | BELOW                                       { Below }
;
%%

