(* Original file: obelisk.0.3.2/Obelisk-0.3.2/tests/zelus.mly *)
(**************************************************************************)
(*                                                                        *)
(*  The Zelus Hybrid Synchronous Language                                 *)
(*  Copyright (C) 2012-2014                                               *)
(*                                                                        *)
(*  Timothy Bourke                                                        *)
(*  Marc Pouzet                                                           *)
(*                                                                        *)
(*  Universite Pierre et Marie Curie - Ecole normale superieure - INRIA   *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

%{

open Lexing
open Location
open Parsetree

let make desc start_pos end_pos =
  { desc = desc; loc = Loc(start_pos.pos_cnum, end_pos.pos_cnum) }

let unop op e = Eapp(Eop(false, Name(op)), [e])

let unary_minus op e =
  match op, e.desc with
    | "-", Econst(Eint v) -> Econst(Eint(-v))
    | ("-" | "_."), Econst(Efloat v) -> Econst(Efloat(-.v))
    | _ -> unop ("~" ^ op) e

let unary_minus_int x = -x
and unary_minus_float x = -.x

let binop op e1 e2 = Eapp(Eop(false, Name(op)), [e1;e2])

let params p = match p.desc with | Etuplepat(l) -> l | _ -> [p]
let arg e = match e.desc with | Etuple(l) -> l | _ -> [e]

let typearg ty =
  match ty.desc with
    | Etypetuple(l) -> l | _ -> [ty]

let scond_true start_pos end_pos =
  make (Econdexp(make (Econst(Ebool(true))) start_pos end_pos))
    start_pos end_pos

(* Temporary solution: put a block arround a single equation *)
let block_of_equation ({ desc = desc; loc = loc } as eq) =
  match desc with
    | EQblock(b) -> b
    | _ -> { desc = { b_locals = []; b_vars = []; b_body = [eq] }; loc = loc }

%}

%token EQUAL          /* "=" */
%token EQUALEQUAL     /* "==" */
%token PLUSEQUAL      /* "+=" */
%token AMPERSAND      /* "&" */
%token AMPERAMPER     /* "&&" */
%token BARBAR         /* "||" */
%token QUOTE          /* "'" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token STAR           /* "*" */
%token COMMA          /* "," */
%token SEMI           /* ";" */
%token SEMISEMI       /* ";;" */
%token MINUSGREATER   /* "->" */
%token AFUN           /* "-A->" */
%token ADFUN          /* "-AD->" */
%token DFUN           /* "-D->" */
%token CFUN           /* "-C->" */
%token DOT            /* "." */
%token COLON          /* ":" */
%token LBRACE         /* "{" */
%token BAR            /* "|" */
%token RBRACE         /* "}" */
%token UNDERSCORE     /* "_" */
%token TEST           /* "?" */
%token <string> CONSTRUCTOR
%token <string> IDENT
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <char> CHAR
%token <string> STRING
%token AS             /* "as" */
%token AFTER          /* "after" */
%token AUTOMATON      /* "automaton" */
%token ATOMIC         /* "atomic" */
%token INLINE         /* "inline" */
%token CONTINUE       /* "continue" */
%token DO             /* "do" */
%token DONE           /* "done" */
%token UNTIL          /* "until" */
%token UNLESS         /* "unless" */
%token MATCH          /* "match" */
%token WITH           /* "with" */
%token EMIT           /* "emit" */
%token PRESENT        /* "present" */
%token PERIOD         /* "period" */
%token END            /* "end" */
%token IN             /* "in" */
%token LET            /* "let" */
%token REC            /* "rec" */
%token DER            /* "der" */
%token INIT           /* "init" */
%token DEFAULT        /* "default" */
%token LOCAL          /* "local" */
%token WHERE          /* "where" */
%token AND            /* "and" */
%token TYPE           /* "type" */
%token FUN            /* "fun" */
%token NODE           /* "node" */
%token HYBRID         /* "hybrid" */
%token DISCRETE       /* "discrete" */
%token UNSAFE         /* "unsafe" */
%token FBY            /* "fby" */
%token NEXT           /* "next" */
%token PRE            /* "pre" */
%token UP             /* "up" */
%token DISC           /* "disc" */
%token EVERY          /* "every" */
%token OR             /* "or" */
%token ON             /* "on" */
%token RESET          /* "reset" */
%token LAST           /* "last" */
%token IF             /* "if" */
%token THEN           /* "then" */
%token ELSE           /* "else" */
%token OPEN           /* "open" */
%token VAL            /* "val" */
%token <string> PREFIX
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2
%token <string> SUBTRACTIVE
%token <string> INFIX3
%token <string> INFIX4
%token EOF

%nonassoc prec_no_end
%nonassoc END
%right IN
%right prec_seq
%right SEMI
%nonassoc prec_ident
%right prec_list
%left EVERY
%left AUTOMATON
%left INIT
%left UNTIL
%left UNLESS
%nonassoc ELSE
%left  AS
%left  BAR
%left COMMA
%left RPAREN
%right MINUSGREATER
%left OR BARBAR
%left AMPERSAND AMPERAMPER
%left INFIX0 EQUAL
%right INFIX1
%left INFIX2 SUBTRACTIVE
%left STAR INFIX3
%left ON
%left INFIX4
%left AFTER
%right prec_uminus
%right FBY
%right PRE UP DISC TEST
%right PREFIX
%left DOT

%start implementation_file
%type <Parsetree.implementation list> implementation_file

%start interface_file
%type <Parsetree.interface list> interface_file

%%

/** Tools **/

/* Separated list */
list_of(S, X):
| x = X { [x] }
| r = list_of(S, X) S x = X { x :: r }
;

/* Localization */
localized(X):
| x = X { make x $startpos $endpos }
;

optional(X):
  | /* empty */
      { None }
  | x = X
      { Some(x) }
;

implementation_file:
  | EOF
      { [] }
  | i = decl_list(localized(implementation)) EOF
      { List.rev i }
;

decl_list(X):
  | dl = decl_list(X) x = X opt_semi_semi
      { x :: dl }
  | x = X opt_semi_semi
      { [x] }
;

opt_semi_semi:
  | /* empty */ {}
  | SEMISEMI {}
;

implementation:
  | OPEN c = CONSTRUCTOR
      { Eopen c }
  | TYPE tp = type_params id = IDENT td = type_declaration
      { Etypedecl(id, tp, td) }
  | LET ide = IDENT EQUAL seq = seq_expression
      { Econstdecl(ide, seq) }
  | LET i = IDENT fp = fun_params EQUAL seq = seq_expression
      { Efundecl(i, A, false, fp, seq) }
  | LET i = IDENT fp = fun_params EQUAL seq = seq_expression
    WHERE r = is_rec eqs = equation_list
      { Efundecl(i, A, false, fp, make(Elet(r, eqs, seq))
                              $startpos(seq) $endpos(eqs))}
  | is_let a = is_atomic k = kind i = IDENT fp = fun_params EQUAL seq = seq_expression
      { Efundecl(i, k, a, fp, seq) }
  | is_let a = is_atomic k = kind i = IDENT fp = fun_params EQUAL seq = seq_expression
    WHERE r = is_rec eqs = equation_list
      { Efundecl(i, k, a, fp, make(Elet(r, eqs, seq))
                              $startpos(seq) $endpos(eqs)) }
  ;

is_rec:
  | REC { true }
  |     { false }
;

is_atomic:
  | ATOMIC { true }
  | { false }
;

is_let:
  | LET { }
  | { }
;

fun_params:
  | p = pattern
      { params p }
;

/* Interface */
interface_file:
  | EOF
      { [] }
  | il = decl_list(localized(interface)) EOF
      { List.rev il }
;

interface:
  | OPEN c = CONSTRUCTOR
      { Einter_open(c) }
  | TYPE tp = type_params i = IDENT td = type_declaration
      { Einter_typedecl(i, tp, td) }
  | VAL i = ide COLON t = type_expression
      { Einter_constdecl(i, t) }
  | VAL UNSAFE i = ide COLON ta = type_expression a = arrow te = type_expression
      { Einter_fundecl(i, { sig_inputs = typearg ta; sig_output = te;
                            sig_kind = a; sig_safe = false }) }
  | VAL i = ide COLON ta = type_expression a = arrow te = type_expression
    { Einter_fundecl(i, { sig_inputs = typearg ta; sig_output = te;
                          sig_kind = a; sig_safe = true }) }
;

type_declaration:
  | /* empty */
      { Eabstract_type }
  | EQUAL e = list_of(BAR, CONSTRUCTOR)
      { Evariant_type (e) }
  | EQUAL LBRACE s = label_list(label_type) RBRACE
      { Erecord_type (s) }
  | EQUAL t = type_expression
      { Eabbrev(t) }
;

type_params :
  | LPAREN tvl = list_of(COMMA, type_var) RPAREN
      { tvl }
  | tv = type_var
      { [tv] }
  |
      { [] }
;

label_list(X):
  | x = X
      { [x] }
  | x = X SEMI
      { [x] }
  | x = X SEMI ll = label_list(X)
      { x :: ll }
;

label_type:
  i = IDENT COLON t = type_expression
  { (i, t) }
;

equation_empty_list:
  | /* empty */
      { [] }
  | eq_list = equation_list
      { eq_list }
;

optional_init:
  | /* empty */
      { None }
  | INIT e = expression
      { Some(e) }
;

%inline equation_list:
  | l = list_of(AND, localized(equation_desc)) { l }
;

simple_equation_desc:
  | AUTOMATON opt_bar a = automaton_handlers(equation_empty_list) opt_end
    { EQautomaton(List.rev a, None) }
  | AUTOMATON opt_bar a = automaton_handlers(equation_empty_list)
    INIT s = state
    { EQautomaton(List.rev a, Some(s)) }
  | MATCH e = seq_expression WITH opt_bar
    m = match_handlers(block_of_equation) opt_end
    { EQmatch(e, List.rev m) }
  | IF e = seq_expression THEN b1 = block_of_equation
    ELSE b2 = block_of_equation opt_end
    { EQifthenelse(e, b1, b2) }
  | PRESENT opt_bar p = present_handlers(block_of_equation) opt_end
    { EQpresent(List.rev p, None) }
  | PRESENT opt_bar p = present_handlers(block_of_equation)
    ELSE b = block_of_equation opt_end
    { EQpresent(List.rev p, Some(b)) }
  | RESET eq = equation_list EVERY e = expression
    { EQreset(eq, e) }
  | l = let_list lo = local_list DO eq_list = equation_empty_list DONE
    { EQblock(make { b_locals = l; b_vars = lo; b_body = eq_list }
		$startpos $endpos) }
;

equation_desc:
  | eq = simple_equation_desc
    { eq } 
  | p = pattern EQUAL e = expression
    { EQeq(p, e) }
  | i = ide PLUSEQUAL e = expression
    { EQpluseq(i, e) }
  | PERIOD p = pattern EQUAL e = period_expression
    { EQeq(p, make (Eperiod(e)) $startpos(e) $endpos(e)) }
  | DER i = ide EQUAL e = expression opt = optional_init
      { EQder(i, e, opt, []) }
  | DER i = ide EQUAL e = expression opt = optional_init
    RESET opt_bar pe = present_handlers(expression)
      { EQder(i, e, opt, List.rev pe) }
  | NEXT i = ide EQUAL e = expression
      { EQnext(i, e, None) }
  | NEXT i = ide EQUAL e = expression INIT e0 = expression
      { EQnext(i, e, Some(e0)) }
  | INIT i = ide EQUAL e = expression
      { EQinit(i, e) }
  | EMIT i = ide
      { EQemit(i, None) }
  | EMIT i = ide EQUAL e = expression
      { EQemit(i, Some(e)) }
;

opt_end:
  | { () } %prec prec_no_end
  | END { () } 
;

/* states of an automaton in an equation*/
automaton_handlers(X) :
  | a = automaton_handler(X)
      { [a] }
  | ahs = automaton_handlers(X) BAR a = automaton_handler(X)
      { a :: ahs }
;

automaton_handler(X):
  | sp = state_pat MINUSGREATER b = block(X) DONE
    { make { s_state = sp; s_block = b; s_until = []; s_unless = [] } $startpos $endpos}
  | sp = state_pat MINUSGREATER b = block(X) THEN st = state
    { make { s_state = sp; s_block = b;
             s_until =
               [{ e_cond = scond_true $endpos(b) $startpos(st);
                  e_reset = true; e_block = None; e_next_state = st }];
	   s_unless = [] }
      $startpos $endpos}
  | sp = state_pat MINUSGREATER b = block(X) CONTINUE st = state
    { make { s_state = sp;
             s_block = b;
             s_until =
               [{ e_cond = scond_true $endpos(b) $startpos(st);
                  e_reset = false;
                  e_block = None; e_next_state = st }];
	     s_unless = [] } $startpos $endpos }
  | sp = state_pat MINUSGREATER b = block(X) THEN emit = emission st = state
    { make { s_state = sp; s_block = b;
             s_until =
               [{ e_cond = scond_true $endpos(b) $startpos(emit);
                  e_reset = true; e_block = Some(emit); e_next_state = st}];
	     s_unless = [] } $startpos $endpos }
  | sp = state_pat MINUSGREATER b = block(X) CONTINUE emit = emission
    st = state
    { make { s_state = sp;
             s_block = b;
             s_until = [{ e_cond = scond_true $endpos(b) $startpos(emit);
                          e_reset = false; e_block = Some(emit);
                          e_next_state = st}];
	     s_unless = [] } $startpos $endpos }
  | sp = state_pat MINUSGREATER b = block(X) UNTIL e_until = escape_list
     { make { s_state = sp; s_block = b; s_until = List.rev e_until; s_unless = [] }
       $startpos $endpos }
  | sp = state_pat MINUSGREATER b = block(X) UNLESS e_unless = escape_list
    { make { s_state = sp; s_block = b; s_until = []; s_unless = List.rev e_unless }
      $startpos $endpos }
  | sp = state_pat MINUSGREATER b = block(X) UNTIL e_until = escape_list
					     UNLESS e_unless = escape_list
    { make { s_state = sp; s_block = b;
	     s_until = List.rev e_until; s_unless = List.rev e_unless }
      $startpos $endpos }
;

escape :
  | scondpat THEN state
      { { e_cond = $1; e_reset = true; e_block = None; e_next_state = $3 } }
  | scondpat CONTINUE state
      { { e_cond = $1; e_reset = false; e_block = None; e_next_state = $3 } }
  | scondpat THEN emission state
      { { e_cond = $1; e_reset = true; e_block = Some($3); e_next_state = $4 } }
  | scondpat CONTINUE emission state
      { { e_cond = $1; e_reset = false; e_block = Some($3); e_next_state = $4 } }
;

escape_list :
  | e = escape
      { [e] }
  | el = escape_list ELSE e = escape
      { e :: el }
;

state :
  | c = CONSTRUCTOR
      { make (Estate0(c)) $startpos $endpos }
  | c = CONSTRUCTOR LPAREN e = expression RPAREN
      { make (Estate1(c, [e])) $startpos $endpos }
  | c = CONSTRUCTOR LPAREN l = expression_comma_list RPAREN
      { make (Estate1(c, l)) $startpos $endpos }
;

state_pat :
  | c = CONSTRUCTOR
      { make (Estate0pat(c)) $startpos $endpos }
  | c = CONSTRUCTOR LPAREN l = list_of(COMMA, IDENT) RPAREN
      { make (Estate1pat(c, l)) $startpos $endpos }
;

/* Pattern on a signal */
scondpat :
  | e = simple_expression p = simple_pattern
      { make (Econdpat(e, p)) $startpos $endpos }
  | e = simple_expression
      { make (Econdexp(e)) $startpos $endpos }
  | UP e = simple_expression
      { make
          (Econdexp (make (Eapp(Eup, [e])) $startpos(e) $endpos(e)))
          $startpos $endpos }
  | scpat1 = scondpat AMPERSAND scpat2 = scondpat
      { make (Econdand(scpat1, scpat2)) $startpos $endpos }
  | scpat1 = scondpat BAR scpat2 = scondpat
      { make (Econdor(scpat1, scpat2)) $startpos $endpos }
  | scpat1 = scondpat ON e = simple_expression
      { make (Econdon(scpat1, e)) $startpos $endpos }
;

/* Block */
block(X):
  | l = let_list lo = local_list DO x = X
      { make { b_locals = l; b_vars = lo; b_body = x } $startpos $endpos }
;

block_of_equation:
  | eq = localized (simple_equation_desc) { block_of_equation(eq) }
;


emission:
  | l1 = one_let IN l2 = let_list
    { make { b_vars = []; b_locals = l1 :: l2; b_body = [] } $startpos $endpos }
  | l = let_list lo = local_list DO eq = equation_empty_list IN
      { make { b_vars = lo; b_locals = l; b_body = eq } $startpos $endpos }
;

let_list:
  | /* empty */
      { [] }
  | o = one_let IN l = let_list
      { o :: l }
;

one_let:
  | LET eq = equation_list
      { make (false, List.rev eq) $startpos $endpos }
  | LET REC eq = equation_list
      { make (true, List.rev eq) $startpos $endpos }
;

local_list:
  | /* empty */
      { [] }
  | LOCAL o = list_of(COMMA, one_local) IN l = local_list
      { o @ l }
;

one_local:
  | i = ide v = optional(default_or_init) c = opt_combine
    { make { vardec_name = i; vardec_default = v; vardec_combine = c }
	$startpos $endpos }
;

default_or_init:
  | DEFAULT c = constant
      { Default(c) }
  | INIT c = constant
    { Init(c) }
;

opt_combine:
  | /* empty */
      { None }
  | WITH i = ext_ident
    { Some(i) }
;

constant:
  | i = atomic_constant
    { Cimmediate(i) }
  | i = ext_ident
    { Cglobal(i) }
;


opt_bar:
  | BAR             { () }
  | /*epsilon*/     { () }
;


/* Testing the presence of a signals */
present_handlers(X):
  | p = present_handler(X)
      { [p ] }
  | ps = present_handlers(X) BAR p = present_handler(X)
      { p :: ps }
;

present_handler(X):
  | sc = scondpat MINUSGREATER x = X
      { { p_cond = sc; p_body = x } }
;

/* Pattern matching in an equation */
match_handlers(X):
  | m = match_handler(X)
      { [m ] }
  | mh = match_handlers(X) BAR m = match_handler(X)
      { m :: mh }
;

match_handler(X):
  | p = pattern MINUSGREATER x = X
      { { m_pat = p; m_body = x } }
;

/* Patterns */
pattern:
  | p = simple_pattern
      { p }
  | p = pattern AS i = IDENT
      { make (Ealiaspat(p, i)) $startpos $endpos }
  | p1 = pattern BAR p2 = pattern
      { make (Eorpat(p1, p2)) $startpos $endpos }
  | p = pattern_comma_list %prec prec_list
      { make (Etuplepat(List.rev p)) $startpos $endpos }
;

simple_pattern:
  | a = atomic_constant
      { make (Econstpat a) $startpos $endpos }
  | SUBTRACTIVE i = INT
      { make (Econstpat(Eint(unary_minus_int i))) $startpos $endpos }
  | SUBTRACTIVE f = FLOAT
      { make (Econstpat(Efloat(unary_minus_float f))) $startpos $endpos }
  | c = constructor
      { make (Econstr0pat(c)) $startpos $endpos }
  | i = ide
      { make (Evarpat i) $startpos $endpos }
  | LPAREN p = pattern RPAREN
      { p }
  | LPAREN p = pattern_comma_list RPAREN
      { make (Etuplepat (List.rev p)) $startpos $endpos }
  | LPAREN RPAREN
      { make (Econstpat(Evoid)) $startpos $endpos }
  | UNDERSCORE
      { make Ewildpat $startpos $endpos }
  | LPAREN p = pattern COLON t = type_expression RPAREN
      { make (Etypeconstraintpat(p, t)) $startpos $endpos }
  | LBRACE p = pattern_label_list RBRACE
      { make (Erecordpat(p)) $startpos $endpos }
;

pattern_comma_list:
  | p1 = pattern COMMA p2 = pattern
      { [p2; p1] }
  | pc = pattern_comma_list COMMA p = pattern
      { p :: pc }
;

pattern_label_list :
  | p = pattern_label SEMI pl = pattern_label_list
      { p :: pl }
  | p = pattern_label
      { [p] }
  | UNDERSCORE
      { [] }
  | /*epsilon*/
      { [] }
;

pattern_label :
  | ei = ext_ident EQUAL p = pattern
      { (ei, p) }
;

/* Expressions */
seq_expression :
  | e = expression SEMI seq = seq_expression
      { make (Eseq(e, seq)) $startpos $endpos }
  | e = expression %prec prec_seq
      { e }
;

simple_expression:
  | desc = simple_expression_desc
      { make desc $startpos $endpos }
;

simple_expression_desc:
  | c = constructor
      { Econstr0(c) }
  | i = ext_ident
      { Evar i }
  | a = atomic_constant
      { Econst a }
  | LBRACE l = label_expression_list RBRACE
      { Erecord(l) }
  | LPAREN RPAREN
      { Econst Evoid }
  | LPAREN e = expression_comma_list RPAREN
      { Etuple (List.rev e) }
  | LPAREN e = seq_expression RPAREN
      { e.desc }
  | LPAREN e = simple_expression COLON t = type_expression RPAREN
      { Etypeconstraint(e, t) }
;

expression_comma_list :
  | ecl = expression_comma_list COMMA e = expression
      { e :: ecl }
  | e1 = expression COMMA e2 = expression
      { [e2; e1] }
;

expression:
  | x = localized(expression_desc)
    { x }
;

expression_desc:
  | e = simple_expression_desc
      { e }
  | e = expression_comma_list %prec prec_list
      { Etuple(List.rev e) }
  | e1 = expression FBY e2 = expression
      { Eapp(Efby, [e1; e2]) }
  | PRE e = expression
      { Eapp(Eunarypre, [e]) }
  | INIT
      { Eapp(Einitial, []) }
  | UP e = expression
      { Eapp(Eup, [e]) }
  | TEST e = expression
      {  Eapp(Etest, [e]) }
  | DISC e = expression
      {  Eapp(Edisc, [e]) }
  | i = ext_ident e = simple_expression
      {  Eapp(Eop(false, i), arg e) }
  | INLINE i = ext_ident e = simple_expression
      {  Eapp(Eop(true, i), arg e) }
  | s = SUBTRACTIVE e = expression  %prec prec_uminus
      { unary_minus s e }
  | e1 = expression i = INFIX4 e2 = expression
      { binop i e1 e2 }
  | e1 = expression i = INFIX3 e2 = expression
      { binop i e1 e2 }
  | e1 = expression i = INFIX2 e2 = expression
      { binop i e1 e2 }
  | e1 = expression i = INFIX1 e2 = expression
      { binop i e1 e2 }
  | e1 = expression i = INFIX0 e2 = expression
      { binop i e1 e2 }
  | e1 = expression EQUAL e2 = expression
      { binop "=" e1 e2 }
  | e1 = expression OR e2 = expression
      { binop "or" e1 e2 }
  | e1 = expression STAR e2 = expression
      { binop "*" e1 e2 }
  | e1 = expression AMPERSAND e2 = expression
      { binop "&" e1 e2 }
  | e1 = expression s = SUBTRACTIVE e2 = expression
      { binop s e1 e2 }
  | e1 = expression AMPERAMPER e2 = expression
      { binop "&&" e1 e2 }
  | e1 = expression BARBAR e2 = expression
      { binop "||" e1 e2 }
  | p = PREFIX e = expression
      { unop p e }
  | IF e1 = seq_expression THEN e2 = seq_expression ELSE e3 = expression
      { Eapp(Eifthenelse, [e1; e2; e3]) }
  | e1 = expression MINUSGREATER e2 = expression
      { Eapp(Eminusgreater, [e1; e2]) }
  | e = expression AFTER n = IDENT
      { Eapp(Eafter([n]), [e]) }
  | e = expression AFTER LPAREN n_list = list_of(COMMA, IDENT) RPAREN
      { Eapp(Eafter(n_list), [e]) }
  | LAST i = ide
      { Elast(i) }
  | e = expression DOT i = ext_ident
      { Erecord_access(e, i) }
  | LET defs = equation_list IN e = seq_expression  
      { Elet(false, List.rev defs, e) }
  | LET REC defs = equation_list IN e = seq_expression 
      { Elet(true, List.rev defs, e) }
  | PERIOD p = period_expression
      { Eperiod(p) }
  | AUTOMATON opt_bar a = automaton_handlers(expression)
      { Eautomaton(List.rev a, None) }
  | AUTOMATON opt_bar a = automaton_handlers(expression) INIT s = state
      { Eautomaton(List.rev a, Some(s)) }
  | MATCH e = seq_expression WITH opt_bar m = match_handlers(expression) opt_end
      { Ematch(e, List.rev m) }
  | PRESENT opt_bar pe = present_handlers(expression) opt_end
      { Epresent(List.rev pe, None) }
  | PRESENT opt_bar pe = present_handlers(expression) INIT e = expression
      { Epresent(List.rev pe, Some(Init(e))) }
  | PRESENT opt_bar pe = present_handlers(expression) ELSE e = expression opt_end
      { Epresent(List.rev pe, Some(Default(e))) }
  | RESET e = seq_expression EVERY r = expression
      { Ereset(e, r) }
  | lo = local_list DO eqs = equation_list IN r = expression
      { Eblock(make { b_locals = []; b_vars = lo; b_body = eqs }
	       $startpos $endpos, r) }
;

/* Periods */
period_expression:
  | phase = phase LPAREN period = period RPAREN
      { { p_phase = phase; p_period = period } }
;

phase:
  | /* empty */
      { None }
  | f = FLOAT
      { Some(f) }
;

period:
  | f = FLOAT
      { f }
;

constructor:
  | c = CONSTRUCTOR
      { Name(c) } %prec prec_ident
  | c1 = CONSTRUCTOR DOT c2 = CONSTRUCTOR
      { Modname({qual = c1; id = c2}) }
;

qual_ident:
  | c = CONSTRUCTOR DOT i = ide
      { {qual = c; id = i} }
;

/* Constants */

atomic_constant:
  | i = INT
      { Eint(i) }
  | f = FLOAT
      { Efloat(f) }
  | s = STRING
      { Estring s }
  | c = CHAR
      { Echar c }
  | b = BOOL
      { Ebool b }
;

label_expression_list:
  | l = label_expression
      { [l] }
  | l = label_expression SEMI
      { [l] }
  | l = label_expression SEMI ll = label_expression_list
      { l :: ll }

label_expression:
  | i = ext_ident EQUAL e = expression
      { (i, e) }
;

/* identifiers */
ide:
  | i = IDENT
      { i }
  | LPAREN i = infx RPAREN
      { i }
;

ext_ident :
  | q = qual_ident
      { Modname(q) }
  | i = ide
      { Name(i) }
;

infx:
  | INFIX0          { $1 }
  | INFIX1          { $1 }    | INFIX2        { $1 }
  | INFIX3          { $1 }    | INFIX4        { $1 }
  | STAR            { "*" }
  | EQUAL           { "=" }
  | EQUALEQUAL      { "==" }
  | SUBTRACTIVE     { $1 }    | PREFIX        { $1 }
  | AMPERSAND       { "&" }   | AMPERAMPER    { "&&" }
  | OR              { "or" }  | BARBAR        { "||" }
  | ON              { "on" }
;

arrow:
  | MINUSGREATER
      { A }
  | AFUN
      { A }
  | ADFUN
      { AD }
  | DFUN
      { D }
  | CFUN
      { C }
;

type_expression:
  | t = simple_type_desc
      { make t $startpos $endpos}
  | tl = type_star_list
      { make(Etypetuple(List.rev tl)) $startpos $endpos}
;

simple_type:
  | desc = simple_type_desc
      { make desc $startpos $endpos }
;

simple_type_desc:
  | t = type_var
      { Etypevar t }
  | i = ext_ident
      { Etypeconstr(i, []) }
  | t = simple_type i = ext_ident
      { Etypeconstr(i, [t]) }
  | LPAREN t = type_expression COMMA tl = type_comma_list RPAREN i = ext_ident
      { Etypeconstr(i, t :: tl) }
  | LPAREN t = type_expression RPAREN
      { t.desc }

type_star_list:
  | t1 = simple_type STAR t2 = simple_type
      { [t2; t1] }
  | tsl = type_star_list STAR t = simple_type
      { t :: tsl }
;

type_var :
  | QUOTE i = IDENT
      { i }
;

type_comma_list :
  | te = type_expression COMMA tl = type_comma_list
      { te :: tl }
  | te = type_expression
      { [te] }
;

kind:
  | NODE
      { D }
  | HYBRID
      { C }
  | DISCRETE
      { AD }
  | FUN
      { A }
;
