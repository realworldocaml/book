/**************************************************************************/
/*                                                                        */
/*                               Flow Caml                                */
/*                                                                        */
/*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*  Copyright 2002, 2003 Institut National de Recherche en Informatique   */
/*  et en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.                  */
/*                                                                        */
/*  Author contact: Vincent.Simonet@inria.fr                              */
/*  Software page: http://cristal.inria.fr/~simonet/soft/flowcaml/        */
/*                                                                        */
/**************************************************************************/

/* $Id: parser.mly,v 1.16 2003/06/26 13:32:51 simonet Exp $ */
/* Parser: the parser definition (preprocessed by ocamlyacc) */

%{
open Location
open Longident
open Asttypes
open Parsetree



let mktyp d =
  { ptyp_desc = d; ptyp_loc = symbol_rloc() }
let mktys typ tyc_list =
  { ptys_type = typ; ptys_constraints = tyc_list; ptys_loc = symbol_rloc () }
let mktyc d =
  { ptyc_desc = d; ptyc_loc = symbol_rloc() }
let mkpat d =
  { ppat_desc = d; ppat_loc = symbol_rloc() }
let mktry d =
  { ptry_desc = d; ptry_loc = symbol_rloc() }
let mktryi lid opt_pat =
  { ptryi_exception = lid;
    ptryi_arg = opt_pat;
    ptryi_loc = symbol_rloc() }
let mkexp d =
  { pexp_desc = d; pexp_loc = symbol_rloc() }
let mkstr d =
  { pstr_desc = d; pstr_loc = symbol_rloc() }
let mkmty d =
  { pmty_desc = d; pmty_loc = symbol_rloc() }
let mksig d =
  { psig_desc = d; psig_loc = symbol_rloc() }
let mkmod d =
  { pmod_desc = d; pmod_loc = symbol_rloc() }
let mklvd (f, t) =
  { plvd_lb = f; plvd_ub = t; plvd_loc = symbol_rloc() }
let mklvl d =
  { plvl_desc = d; plvl_loc = symbol_rloc() }
let mkexn t m =
  { pexn_type = t; pexn_manifest = m; pexn_loc = symbol_rloc() }
let mkwth lid d =
  { pwth_ident = lid; pwth_desc = d; pwth_loc = symbol_rloc() }

let mkoperator name pos =
  { pexp_desc = Pexp_ident(Lident name); pexp_loc = rhs_loc pos }

let ghexp d = { pexp_desc = d; pexp_loc = symbol_gloc () };;
let ghpat d = { ppat_desc = d; ppat_loc = symbol_gloc () };;
let ghtyp d = { ptyp_desc = d; ptyp_loc = symbol_gloc () };;

let ptyp_any =
  let count = ref 0 in
  function () -> incr count; Ptyp_var ("%" ^ (string_of_int !count))

let mkassert e =
  match e with
  | {pexp_desc = Pexp_construct (Lident "false", None, false) } ->
         mkexp (Pexp_assertfalse)
  | _ -> mkexp (Pexp_assert (e))

let mklazy e =
  let void_pat = ghpat (Ppat_construct (Lident "()", None, false)) in
  let f = ghexp (Pexp_function ([void_pat, None, e])) in
  let delayed = Ldot (Lident "Lazy", "Delayed") in
  let df = ghexp (Pexp_construct (delayed, Some f, false)) in
  let r = ghexp (Pexp_ident (Ldot (Lident "Pervasives", "ref"))) in
  ghexp (Pexp_apply (r, [df]))

let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, [arg1; arg2]))

let neg_float_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus name arg =
  match arg.pexp_desc with
    Pexp_constant(Const_int n) ->
      mkexp(Pexp_constant(Const_int(-n)))
  | Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float(neg_float_string f)))
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, [arg]))

let rec mktailexp = function
    [] ->
      ghexp(Pexp_construct(Lident "[]", None, false))
  | e1 :: el ->
      let exp_el = mktailexp el in
      let l = {loc_start = e1.pexp_loc.loc_start;
               loc_end = exp_el.pexp_loc.loc_end;
               loc_ghost = false}
      in
      let arg = {pexp_desc = Pexp_tuple [e1; exp_el];
                 pexp_loc = {l with loc_ghost = true} } in
      {pexp_desc = Pexp_construct(Lident "::", Some arg, false); pexp_loc = l}

let rec mktailpat = function
    [] ->
      ghpat(Ppat_construct(Lident "[]", None, false))
  | p1 :: pl ->
      let pat_pl = mktailpat pl in
      let l = {loc_start = p1.ppat_loc.loc_start;
               loc_end = pat_pl.ppat_loc.loc_end;
               loc_ghost = false}
      in
      let arg = {ppat_desc = Ppat_tuple [p1; pat_pl];
                 ppat_loc = {l with loc_ghost = true} } in
      {ppat_desc = Ppat_construct(Lident "::", Some arg, false); ppat_loc = l}

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                                           rhs_loc closing_num, closing_name)))

let mkstrexp e =
  { pstr_desc = Pstr_eval e; pstr_loc = {e.pexp_loc with loc_ghost = true} }

let array_function str name =
  Ldot(Lident str, name)

let rec mkrangepat c1 c2 =
  if c1 > c2 then mkrangepat c2 c1 else
  if c1 = c2 then ghpat(Ppat_constant(Const_char c1)) else
  ghpat(Ppat_or(ghpat(Ppat_constant(Const_char c1)),
                mkrangepat (Char.chr(Char.code c1 + 1)) c2))


let bigarray_function str name =
  Ldot(Ldot(Lident "Bigarray", str), name)

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist} -> explist
  | exp -> [exp]

let bigarray_get arr arg =
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" "get")),
                       [arr; c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" "get")),
                       [arr; c1; c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" "get")),
                       [arr; c1; c2; c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "get")),
                       [arr; ghexp(Pexp_array coords)]))

let bigarray_set arr arg newval =
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" "set")),
                       [arr; c1; newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" "set")),
                       [arr; c1; c2; newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" "set")),
                       [arr; c1; c2; c3; newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "set")),
                       [arr;
                        ghexp(Pexp_array coords);
                        newval]))
%}



/* *************************************************************************
   Tokens definition
 */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token <string> CHARVECT
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZE
%token INITIALIZER
%token <int> INT
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token NEW
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
%token PLUS
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token SIG
%token STAR
%token <string> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH

%token AFFECTS
%token CONTENT
%token FINALLY
%token FLOW
%token GREATERWORD
%token LESSWORD
%token LEVEL
%token NONEQ
%token RAISE
%token RAISES
%token PROPAGATE
%token ROW
%token THAN

%token MINUSBRACE
%token BRACEMINUSGREATER
%token EQUALGREATER
%token EQUALBRACE
%token BRACEEQUALGREATER

/* Precedences and associativities. Lower precedences come first. */

%right prec_reraise
%right PROPAGATE

%right prec_let                         /* let ... in ... */
%right prec_type_def                    /* = in type definitions */
%right SEMI                             /* e1; e2 (sequence) */
%right prec_fun prec_match prec_try     /* match ... with ... */
%right prec_list                        /* e1; e2 (list, array, record) */
%right prec_if                          /* if ... then ... else ... */
%right COLONEQUAL LESSMINUS             /* assignments */
%left  AS                               /* as in patterns */
%left  BAR                              /* | in patterns */
%left  COMMA                            /* , in expressions, patterns, types */
%right prec_type_arrow                  /* -> in type expressions */
%right OR BARBAR                        /* or */
%right AMPERSAND AMPERAMPER             /* & */
%left  INFIXOP0 EQUAL LESS GREATER      /* = < > etc */
%right INFIXOP1                         /* @ ^ etc */
%right COLONCOLON                       /* :: */
%left  INFIXOP2 PLUS MINUS MINUSDOT     /* + - */
%left  INFIXOP3 STAR                    /* * / */
%right INFIXOP4                         /* ** */
%right prec_unary_minus                 /* - unary */
%left  prec_appl                        /* function application */
%right prec_constr_appl                 /* constructor application */
%left  SHARP                            /* method call */
%left  DOT                              /* record access, array access */
%right PREFIXOP                         /* ! */

/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.implementation> implementation
%start interface                        /* for interface files */
%type <Parsetree.interface> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file

%start val_longident
%type <Longident.t> val_longident
%start type_longident
%type <Longident.t> type_longident
%start level_longident
%type <Longident.t> level_longident
%start exception_longident
%type <Longident.t> exception_longident

%%



/* *************************************************************************
   Grammar definition
 */



/* -------------------------------------------------------------------------
   Entry points
 */

implementation:
    flows_declaration structure EOF
      { { pimp_structure = $2;
	  pimp_flows = $1;
	  pimp_header_loc = rhs_loc 1
	}
      }
;
interface:
    flows_declaration affects raises signature EOF
      { { pint_signature = List.rev $4;
	  pint_flows = $1;
	  pint_pci = $2;
	  pint_pcf = $3;
	  pint_header_loc = rhs_locs 1 3
	}
      }
;
toplevel_phrase:
    top_structure SEMISEMI                     { Ptop_def $1 }
  | FLOW flows SEMISEMI                        { Ptop_flow $2 }
  | seq_expr SEMISEMI                          { Ptop_def[mkstrexp $1] }
  | toplevel_directive SEMISEMI                { $1 }
  | EOF                                        { raise End_of_file }
;
top_structure:
    structure_item                       { [$1] }
  | structure_item top_structure         { $1 :: $2 }
;
use_file:
    use_file_tail                        { $1 }
  | seq_expr use_file_tail               { Ptop_def[mkstrexp $1] :: $2 }
;
use_file_tail:
    EOF                                         { [] }
  | SEMISEMI EOF                                { [] }
  | SEMISEMI seq_expr use_file_tail             { Ptop_def[mkstrexp $2] :: $3 }
  | SEMISEMI structure_item use_file_tail       { Ptop_def[$2] :: $3 }
  | SEMISEMI toplevel_directive use_file_tail   { $2 :: $3 }
  | structure_item use_file_tail                { Ptop_def[$1] :: $2 }
/*  | toplevel_directive use_file_tail            { $1 :: $2 } */
;
affects:
    /* empty */                                 { [] }
  | AFFECTS level_comma_list                    { $2 }
;
raises:
    /* empty */                                 { [] }
  | RAISES level_comma_list                    { $2 }
;


/* -------------------------------------------------------------------------
   Lattice declaration
 */

flows_declaration:
    /* empty */
    { [] }
  | FLOW flows
    { List.rev $2 }
;
flows:
    flow                                     { $1 :: [] }
  | flows AND flow                           { $3 :: $1 }
;
flow:
    flow_hand_side LESS flow_hand_side       { ($1, $3) }
;
flow_hand_side:
    principal                                { $1 :: [] }
  | flow_hand_side COMMA principal           { $3 :: $1 }
;



/* -------------------------------------------------------------------------
   Module expressions
 */

module_expr:
    mod_longident
      { mkmod(Pmod_ident $1) }
  | STRUCT structure END
      { mkmod(Pmod_structure($2)) }
  | STRUCT structure error
      { unclosed "struct" 1 "end" 3 }
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN MINUSGREATER module_expr
    %prec prec_fun
      { mkmod(Pmod_functor($3, $5, $8)) }
  | module_expr LPAREN module_expr RPAREN
      { mkmod(Pmod_apply($1, $3)) }
  | LPAREN module_expr COLON module_type RPAREN
      { mkmod(Pmod_constraint($2, $4)) }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN module_expr RPAREN
      { mkmod (Pmod_paren $2) }
  | LPAREN module_expr error
      { unclosed "(" 1 ")" 3 }
;
structure:
    structure_tail                              { $1 }
  | seq_expr structure_tail                     { mkstrexp $1 :: $2 }
;
structure_tail:
    /* empty */                                 { [] }
  | SEMISEMI                                    { [] }
  | SEMISEMI seq_expr structure_tail            { mkstrexp $2 :: $3 }
  | SEMISEMI structure_item structure_tail      { $2 :: $3 }
  | structure_item structure_tail               { $1 :: $2 }
;
structure_item:
    LET rec_flag let_bindings
      { match $3 with
          [{ppat_desc = Ppat_any}, exp] -> mkstr(Pstr_eval exp)
        | _ -> mkstr(Pstr_value($2, List.rev $3)) }
  | EXTERNAL val_ident_colon core_type_scheme EQUAL primitive_declaration
      { mkstr(Pstr_primitive($2, {pval_type = $3; pval_prim = $5})) }
  | TYPE type_declarations
      { mkstr(Pstr_type(List.rev $2)) }
  | LEVEL level_declaration
      { let name, decl = $2 in
        mkstr(Pstr_level (name, decl)) }
  | EXCEPTION UIDENT exception_declaration
      { mkstr(Pstr_exception($2, $3)) }
  | MODULE UIDENT module_binding
      { mkstr(Pstr_module($2, $3)) }
  | MODULE TYPE ident EQUAL module_type
      { mkstr(Pstr_modtype($3, $5)) }
  | OPEN mod_longident
      { mkstr(Pstr_open $2) }
  | INCLUDE module_expr
      { mkstr(Pstr_include $2) }
;
module_binding:
    EQUAL module_expr
      { $2 }
  | COLON module_type EQUAL module_expr
      { mkmod(Pmod_constraint($4, $2)) }
  | LPAREN UIDENT COLON module_type RPAREN module_binding
      { mkmod(Pmod_functor($2, $4, $6)) }
;
exception_declaration:
    exception_arguments
      { mkexn (None, $1) None }
  | COLON QUOTE LIDENT exception_arguments
      { mkexn (Some ($3, rhs_locs 1 3), $4) None }
  | exception_arguments EQUAL constr_longident
      { mkexn (None, $1) (Some ($3, rhs_locs 2 3)) }
  | COLON QUOTE LIDENT exception_arguments EQUAL constr_longident
      { mkexn (Some ($3, rhs_locs 1 3), $4) (Some ($6, rhs_locs 5 6)) }
;
exception_arguments:
    /*empty*/                                   { [] }
  | OF extcore_type_list                        { List.rev $2 }
;


/* -------------------------------------------------------------------------
   Module types
 */

module_type:
    mty_longident
      { mkmty(Pmty_ident $1) }
  | SIG signature END
      { mkmty(Pmty_signature(List.rev $2)) }
  | SIG signature error
      { unclosed "sig" 1 "end" 3 }
  | FUNCTOR LPAREN UIDENT COLON module_type RPAREN module_type_arrow module_type
    %prec prec_fun
      { let lb, ub = $7 in
        mkmty(Pmty_functor($3, $5, lb, ub, $8, Some (rhs_loc 7)))
      }
  | module_type WITH with_constraints
      { mkmty(Pmty_with($1, List.rev $3)) }
  | LPAREN module_type RPAREN
      { mkmty (Pmty_paren $2) }
  | LPAREN module_type error
      { unclosed "(" 1 ")" 3 }
;
module_type_arrow:
    MINUSGREATER
      { [], [] }
  | MINUSBRACE level_comma_list BAR level_comma_list BRACEMINUSGREATER
      { $2, $4 }
;
signature:
    /* empty */                                 { [] }
  | signature signature_item                    { $2 :: $1 }
  | signature signature_item SEMISEMI           { $2 :: $1 }
;
signature_item:
    VAL val_ident_colon core_type_scheme
      { mksig(Psig_value($2, {pval_type = $3; pval_prim = []})) }
  | EXTERNAL val_ident_colon core_type_scheme EQUAL primitive_declaration
      { mksig(Psig_value($2, {pval_type = $3; pval_prim = $5})) }
  | TYPE type_declarations
      { mksig(Psig_type(List.rev $2)) }
  | LEVEL level_declaration
      { let name, decl = $2 in
        mksig(Psig_level (name, decl)) }
  | EXCEPTION UIDENT exception_declaration
      { mksig(Psig_exception($2, $3)) }
  | MODULE UIDENT module_declaration
      { mksig(Psig_module($2, $3)) }
  | MODULE TYPE ident
      { mksig(Psig_modtype($3, Pmodtype_abstract)) }
  | MODULE TYPE ident EQUAL module_type
      { mksig(Psig_modtype($3, Pmodtype_manifest $5)) }
  | OPEN mod_longident
      { mksig(Psig_open $2) }
  | INCLUDE module_type
      { mksig(Psig_include $2) }
;
module_declaration:
    COLON module_type
      { $2 }
  | LPAREN UIDENT COLON module_type RPAREN module_declaration
      { mkmty(Pmty_functor($2, $4, [], [], $6, None)) }
;


with_constraints:
    with_constraint                             { [$1] }
  | with_constraints AND with_constraint        { $3 :: $1 }
;
with_constraint:
    TYPE type_parameters label_longident EQUAL core_type constraints
      { let params, variance = List.split $2 in
        mkwth $3 (Pwith_type { ptype_params = $2;
			       ptype_fun = false;
                               ptype_cstrs = List.rev $6;
                               ptype_repr = Ptype_abstract;
                               ptype_manifest = Some $5;
                               ptype_loc = rhs_locs 2 6;
			       ptype_loc' = rhs_loc 3;
			       ptype_topo = 0;
			       ptype_pred = [] })
      }
  | LEVEL level_strict_longident level_repr
      { mkwth $2 (Pwith_level (mklvd $3)) }
  | MODULE mod_longident EQUAL mod_ext_longident
      { mkwth $2 (Pwith_module $4) }
;
     /* used label_longident instead of type_longident
        to disallow functor applications in type path */




/* -------------------------------------------------------------------------
   Core expressions
 */

seq_expr:
  | expr              %prec SEMI  { $1 }
  | expr SEMI                     { $1 }
  | expr SEMI seq_expr            { mkexp(Pexp_sequence($1, $3)) }
;
expr:
    simple_expr
      { $1 }
  | simple_expr simple_expr_list %prec prec_appl
      { mkexp(Pexp_apply($1, List.rev $2)) }
  | LET rec_flag let_bindings IN seq_expr %prec prec_let
      { mkexp(Pexp_let($2, List.rev $3, $5)) }
  | FUNCTION opt_bar match_cases %prec prec_fun
      { mkexp(Pexp_function(List.rev $3)) }
  | FUN simple_pattern fun_def %prec prec_fun
      { mkexp(Pexp_function([$2, fst $3, snd $3])) }
  | MATCH seq_expr WITH opt_bar match_cases %prec prec_match
      { mkexp(Pexp_match($2, List.rev $5)) }
  | RAISE simple_exception_longident
      { mkexp(Pexp_raise($2, None)) }
  | RAISE LPAREN constr_longident simple_expr RPAREN
      { mkexp(Pexp_raise($3, Some $4)) }
  | TRY seq_expr FINALLY seq_expr
      { mkexp(Pexp_finally($2, $4)) }
  | TRY seq_expr WITH opt_bar try_cases %prec prec_try
      { mkexp(Pexp_try($2, List.rev $5)) }
  | TRY seq_expr WITH error %prec prec_try
      { syntax_error() }
  | expr_comma_list
      { mkexp(Pexp_tuple(List.rev $1)) }
  | constr_longident simple_expr %prec prec_constr_appl
      { mkexp(Pexp_construct($1, Some $2, false)) }
  | IF seq_expr THEN expr ELSE expr %prec prec_if
      { mkexp(Pexp_ifthenelse($2, $4, Some $6)) }
  | IF seq_expr THEN expr %prec prec_if
      { mkexp(Pexp_ifthenelse($2, $4, None)) }
  | WHILE seq_expr DO seq_expr DONE
      { mkexp(Pexp_while($2, $4)) }
  | FOR val_ident EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
      { mkexp(Pexp_for($2, $4, $6, $5, $8)) }
  | expr COLONCOLON expr
      { mkexp(Pexp_construct(Lident "::",
                             Some(ghexp(Pexp_tuple[$1;$3])),
                             false)) }
  | expr INFIXOP0 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 }
  | expr PLUS expr
      { mkinfix $1 "+" $3 }
  | expr MINUS expr
      { mkinfix $1 "-" $3 }
  | expr MINUSDOT expr
      { mkinfix $1 "-." $3 }
  | expr STAR expr
      { mkinfix $1 "*" $3 }
  | expr EQUAL expr
      { mkinfix $1 "=" $3 }
  | expr LESS expr
      { mkinfix $1 "<" $3 }
  | expr GREATER expr
      { mkinfix $1 ">" $3 }
  | expr OR expr
      { mkinfix $1 "or" $3 }
  | expr BARBAR expr
      { mkinfix $1 "||" $3 }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 }
  | expr AMPERAMPER expr
      { mkinfix $1 "&&" $3 }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus $1 $2 }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexp(Pexp_setfield($1, $3, $5)) }
  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "set")),
                         [$1; $4; $7])) }
  | simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Charray" "set")),
                         [$1; $4; $7])) }
  | simple_expr DOT LBRACE expr RBRACE LESSMINUS expr
      { bigarray_set $1 $4 $7 }
  | ASSERT simple_expr %prec prec_appl
      { mkassert $2 }
  | LAZY simple_expr %prec prec_appl
      { mklazy $2 }
;
simple_expr:
    val_longident
      { mkexp(Pexp_ident $1) }
  | constant
      { mkexp(Pexp_constant $1) }
  | constr_longident
      { mkexp(Pexp_construct($1, None, false)) }
  | LPAREN seq_expr RPAREN
      { mkexp (Pexp_paren $2) }
  | LPAREN seq_expr error
      { unclosed "(" 1 ")" 3 }
  | BEGIN seq_expr END
      { mkexp (Pexp_paren $2) }
  | BEGIN END
      { mkexp (Pexp_construct (Lident "()", None, false)) }
  | BEGIN seq_expr error
      { unclosed "begin" 1 "end" 3 }
  | LPAREN seq_expr type_constraint RPAREN
      { mkexp(Pexp_constraint($2, $3)) }
  | simple_expr DOT label_longident
      { mkexp(Pexp_field($1, $3)) }
  | simple_expr DOT LPAREN seq_expr RPAREN
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "get")),
                         [$1; $4])) }
  | simple_expr DOT LPAREN seq_expr error
      { unclosed "(" 3 ")" 5 }
  | simple_expr DOT LBRACKET seq_expr RBRACKET
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Charray" "get")),
                         [$1; $4])) }
  | simple_expr DOT LBRACKET seq_expr error
      { unclosed "[" 3 "]" 5 }
  | simple_expr DOT LBRACE expr RBRACE
      { bigarray_get $1 $4 }
  | simple_expr DOT LBRACE expr_comma_list error
      { unclosed "{" 3 "}" 5 }
  | LBRACE record_expr RBRACE
      { let (exten, fields) = $2 in mkexp(Pexp_record(fields, exten)) }
  | LBRACE record_expr error
      { unclosed "{" 1 "}" 5 }
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { mkexp(Pexp_array(List.rev $2)) }
  | LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LBRACKETBAR BARRBRACKET
      { mkexp(Pexp_array []) }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { mkexp (mktailexp (List.rev $2)).pexp_desc }
  | LBRACKET expr_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
  | PREFIXOP simple_expr
      { mkexp(Pexp_apply(mkoperator $1 1, [$2])) }
;
simple_expr_list:
    simple_expr
      { [$1] }
  | simple_expr_list simple_expr
      { $2 :: $1 }
;
let_bindings:
    let_binding                                 { [$1] }
  | let_bindings AND let_binding                { $3 :: $1 }
;
let_binding:
    val_ident fun_binding
      { ({ppat_desc = Ppat_var $1; ppat_loc = rhs_loc 1}, $2) }
  | pattern EQUAL seq_expr %prec prec_let
      { ($1, $3) }
;
fun_binding:
    EQUAL seq_expr %prec prec_let
      { $2 }
  | type_constraint EQUAL seq_expr %prec prec_let
      { mkexp(Pexp_constraint($3, $1)) }
  | simple_pattern fun_binding
      { mkexp(Pexp_function([$1, None, $2])) }
;
match_cases:
    pattern match_action
      { [$1, fst $2, snd $2] }
  | match_cases BAR pattern match_action
      { ($3, fst $4, snd $4) :: $1 }
;
try_cases:
    try_pattern MINUSGREATER seq_expr %prec prec_reraise
      { [$1, $3, Throw] }
  | try_cases BAR try_pattern MINUSGREATER seq_expr %prec prec_reraise
      { ($3, $5, Throw) :: $1 }
  | try_pattern MINUSGREATER seq_expr PROPAGATE
      { [$1, $3, Propagate (rhs_loc 4)] }
  | try_cases BAR try_pattern MINUSGREATER seq_expr PROPAGATE
      { ($3, $5, Propagate (rhs_loc 6)) :: $1 }
;
fun_def:
    match_action
      { $1 }
  | simple_pattern fun_def
      { None, mkexp(Pexp_function([$1, fst $2, snd $2])) }
;
match_action:
    MINUSGREATER seq_expr                       { None, $2 }
  | WHEN seq_expr MINUSGREATER seq_expr         { Some $2, $4 }
;
expr_comma_list:
    expr_comma_list COMMA expr                  { $3 :: $1 }
  | expr COMMA expr                             { [$3; $1] }
;
record_expr:
    simple_expr WITH lbl_expr_list opt_semi     { (Some $1, List.rev $3) }
  | lbl_expr_list opt_semi                      { (None, List.rev $1) }
;
lbl_expr_list:
    label_longident EQUAL expr %prec prec_list
      { [$1,$3] }
  | lbl_expr_list SEMI label_longident EQUAL expr %prec prec_list
      { ($3, $5) :: $1 }
;
expr_semi_list:
    expr %prec prec_list                        { [$1] }
  | expr_semi_list SEMI expr %prec prec_list    { $3 :: $1 }
;
type_constraint:
    COLON core_type_scheme                      { $2 }
  | COLON error                                 { syntax_error() }
;



/* -------------------------------------------------------------------------
   Patterns
 */

pattern:
    simple_pattern
      { $1 }
  | pattern AS val_ident
      { mkpat(Ppat_alias($1, $3)) }
  | pattern_comma_list
      { mkpat(Ppat_tuple(List.rev $1)) }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct($1, Some $2, false)) }
  | pattern COLONCOLON pattern
      { mkpat(Ppat_construct(Lident "::", Some(ghpat(Ppat_tuple[$1;$3])),
                             false)) }
  | pattern BAR pattern
      { mkpat(Ppat_or($1, $3)) }
;
simple_pattern:
    val_ident
      { mkpat(Ppat_var $1) }
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | CHAR DOTDOT CHAR
      { mkrangepat $1 $3 }
  | constr_longident
      { mkpat(Ppat_construct($1, None, false)) }
  | LBRACE lbl_pattern_list opt_semi RBRACE
      { mkpat(Ppat_record(List.rev $2)) }
  | LBRACE lbl_pattern_list opt_semi error
      { unclosed "{" 1 "}" 4 }
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { mkpat (mktailpat (List.rev $2)).ppat_desc }
  | LBRACKET pattern_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
  | LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
      { mkpat(Ppat_array(List.rev $2)) }
  | LBRACKETBAR BARRBRACKET
      { mkpat(Ppat_array []) }
  | LBRACKETBAR pattern_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LPAREN pattern RPAREN
      { mkpat (Ppat_paren $2) }
  | LPAREN pattern error
      { unclosed "(" 1 ")" 3 }
  | LPAREN pattern COLON core_type_scheme RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
  | LPAREN pattern COLON core_type_scheme error
      { unclosed "(" 1 ")" 5 }
;
pattern_comma_list:
    pattern_comma_list COMMA pattern            { $3 :: $1 }
  | pattern COMMA pattern                       { [$3; $1] }
;
pattern_semi_list:
    pattern                                     { [$1] }
  | pattern_semi_list SEMI pattern              { $3 :: $1 }
;
lbl_pattern_list:
    label_longident EQUAL pattern               { [($1, $3)] }
  | lbl_pattern_list SEMI label_longident EQUAL pattern { ($3, $5) :: $1 }
;



/* -------------------------------------------------------------------------
   Patterns
 */

try_pattern:
    UNDERSCORE                                  { mktry Ptry_any }
  | try_pattern_list                            { mktry (Ptry_list $1) }
;
try_pattern_list:
    try_pattern_item                            { $1 :: [] }
  | try_pattern_item BAR try_pattern_list       { $1 :: $3 }
;
try_pattern_item:
    simple_exception_longident                  { mktryi $1 None }
  | simple_exception_longident simple_pattern   { mktryi $1 (Some $2) }
;



/* -------------------------------------------------------------------------
   Primitive declarations
 */

primitive_declaration:
    STRING                                      { [$1] }
  | STRING primitive_declaration                { $1 :: $2 }
;



/* -------------------------------------------------------------------------
   Type declarations
 */

type_declarations:
    type_declaration                            { [$1] }
  | type_declarations AND type_declaration      { $3 :: $1 }
;
type_declaration:
    opt_fun type_parameters LIDENT type_kind constraints
      { let repr, manifest = $4 in
        ($3, { ptype_params = $2;
	       ptype_fun = $1;
               ptype_cstrs = List.rev $5;
               ptype_repr = repr;
               ptype_manifest = manifest;
               ptype_loc = symbol_rloc();
	       ptype_loc' = rhs_loc 3;
	       ptype_topo = 0;
	       ptype_pred = []
	     }) }
;
opt_fun:
    /*empty*/ { false }
  | NONEQ { true }
;
constraints:
        constraints CONSTRAINT constrain        { $3 :: $1 }
      | /* empty */                             { [] }
;
constrain:
        core_type EQUAL core_type          { $1, $3, symbol_rloc () }
;
type_kind:
    /*empty*/
      { (Ptype_abstract, None) }
  | EQUAL core_type %prec prec_type_def
      { (Ptype_abstract, Some $2) }
  | EQUAL type_kind_variant
      { $2, None }
  | EQUAL BAR type_kind_variant
      { $3, None }
  | EQUAL type_kind_record
      { $2, None }
  | EQUAL core_type EQUAL type_kind_variant %prec prec_type_def
      { $4, Some $2 }
  | EQUAL core_type EQUAL type_kind_record %prec prec_type_def
      { $4, Some $2 }

;
type_kind_variant:
    constructor_declarations opt_annot_level
      { Ptype_variant(List.rev $1, $2) }
;
type_kind_record:
    LBRACE label_declarations opt_semi RBRACE opt_annot_level
      { Ptype_record(List.rev $2, $5) }
;
opt_annot_level:
    /*empty*/                                   { None }
  | SHARP simple_core_type                      { Some ($2, symbol_rloc ()) }
;
type_parameters:
    /*empty*/                                   { [] }
  | type_parameter                              { [$1] }
  | LPAREN type_parameter_list RPAREN           { List.rev $2 }
;
type_parameter:
    opt_variance QUOTE ident opt_kind
      { ($3, ($4, $1)) }
;
type_parameter_list:
    type_parameter
      { [$1] }
  | type_parameter_list COMMA type_parameter
      { $3 :: $1 }
;
opt_kind:
    /*empty*/
      { Pak_var }
  | COLON LEVEL
      { Pak_level }
  | COLON TYPE
      { Pak_type }
  | COLON ROW LBRACKET opt_rowlabel_list RBRACKET
      { Pak_row $4 }
;
opt_variance:
    /*empty*/                                   { Pvar_none }
  | PLUS                                        { Pvar_covariant }
  | EQUAL                                       { Pvar_invariant }
  | MINUS                                       { Pvar_contravariant }
  | SHARP                                       { Pvar_guarded }
;
opt_rowlabel_list:
    /*empty*/                                   { [] }
  | rowlabel_list                               { $1 }
;
rowlabel_list:
    mod_longident
      { [ $1 ] }
  | mod_longident COMMA rowlabel_list
      { $1 :: $3 }
;
constructor_declarations:
    constructor_declaration                     { [$1] }
  | constructor_declarations BAR constructor_declaration { $3 :: $1 }
;
constructor_declaration:
    constr_ident constructor_arguments          { ($1, $2) }
;
constructor_arguments:
    /*empty*/                                   { [] }
  | OF core_type_list                           { List.rev $2 }
;
label_declarations:
    label_declaration                           { [$1] }
  | label_declarations SEMI label_declaration   { $3 :: $1 }
;
label_declaration:
    mutable_flag label COLON core_type          { ($2, $1, $4) }
;
label:
    LIDENT                                      { $1 }
;



/* -------------------------------------------------------------------------
   Extended core types (for schemes)
 */


extcore_type:
    exception_longident COLON simple_extcore_type SEMI extcore_type
      { mktyp(Ptyp_row($1, $3, $5)) }
  | exception_longident COLON simple_extcore_type
      { mktyp(Ptyp_row($1, $3, mktyp(ptyp_any()))) }
  | extcore_type2
      { $1 }
;
extcore_type2:
    simple_extcore_type_or_tuple
      { $1 }
  | extcore_type2 extcore_type_arrow extcore_type2 %prec prec_type_arrow
      { let x1, x2, x3 = $2 in
	mktyp(Ptyp_arrow($1, x1, x2, $3, x3)) }
  | extcore_type_args EQUALGREATER simple_extcore_type_or_tuple
      { mktyp(Ptyp_arrow_abbrev(List.rev $1,
				mktyp(ptyp_any()), mktyp(ptyp_any()), $3)) }
  | extcore_type_args extcore_type_arrow_abbrev simple_extcore_type_or_tuple
      { let x1, x2 = $2 in
        mktyp(Ptyp_arrow_abbrev(List.rev $1, x1, x2, $3)) }
;
extcore_type_arrow:
    MINUSGREATER
      { mktyp(ptyp_any()), mktyp(ptyp_any()), mktyp(ptyp_any()) }
  | MINUSBRACE BRACEMINUSGREATER
      { mktyp(ptyp_any()), mktyp(ptyp_any()), mktyp(ptyp_any()) }
  | MINUSBRACE opt_extcore_type BAR opt_extcore_type BAR opt_extcore_type
      BRACEMINUSGREATER
      { $2, $4, $6 }
  | MINUSBRACE opt_extcore_type BARBAR opt_extcore_type BRACEMINUSGREATER
      { $2, mktyp(ptyp_any()), $4 }
;
extcore_type_args:
    simple_extcore_type_or_tuple
      { $1 :: [] }
  | extcore_type_args EQUALGREATER simple_extcore_type_or_tuple
      { $3 :: $1 }
;
extcore_type_arrow_abbrev:
    EQUALBRACE BRACEEQUALGREATER
      { mktyp(ptyp_any()), mktyp(ptyp_any()) }
  | EQUALBRACE opt_extcore_type BAR opt_extcore_type BRACEEQUALGREATER
      { $2, $4 }

;
opt_extcore_type:
    /*epsilon*/
      { mktyp(ptyp_any()) }
  | extcore_type
      { $1 }
;
simple_extcore_type:
    simple_extcore_type2
      { $1 }
  | LPAREN extcore_type_comma_list RPAREN
      {
	match $2 with
	  [sty] -> mktyp (Ptyp_paren sty)
        | _ -> raise Parse_error
      }
;
simple_extcore_type2:
    QUOTE ident
      { mktyp(Ptyp_var $2) }
  | UNDERSCORE
      { mktyp(ptyp_any ()) }
  | core_type_bounds
      { let lb, ub = $1 in mktyp(Ptyp_bounds (false, lb, ub)) }
  | core_type_row_bounds
      { let lb, ub = $1 in mktyp(Ptyp_bounds (true, lb, ub)) }
  | type_longident %prec prec_constr_appl
      { mktyp(Ptyp_constr($1, [])) }
  | simple_extcore_type2 type_longident %prec prec_constr_appl
      { mktyp(Ptyp_constr($2, [$1])) }
  | LPAREN extcore_type_comma_list RPAREN type_longident %prec prec_constr_appl
      { mktyp(Ptyp_constr($4, List.rev $2)) }
;
simple_extcore_type_or_tuple:
    simple_extcore_type
      { $1 }
  | simple_extcore_type STAR extcore_type_list
      { mktyp(Ptyp_tuple($1 :: List.rev $3)) }
;
extcore_type_comma_list:
    extcore_type                                      { [$1] }
  | extcore_type_comma_list COMMA extcore_type        { $3 :: $1 }
;
extcore_type_list:
    simple_extcore_type                               { [$1] }
  | extcore_type_list STAR simple_extcore_type        { $3 :: $1 }
;



/* -------------------------------------------------------------------------
   Core types (for type declarations)
 */


core_type:
    exception_longident COLON simple_core_type SEMI core_type
      { mktyp(Ptyp_row($1, $3, $5)) }
  | core_type2
      { $1 }
;
core_type2:
    simple_core_type_or_tuple
      { $1 }
  | core_type2
      MINUSBRACE core_type BAR core_type BAR core_type BRACEMINUSGREATER
      core_type2 %prec prec_type_arrow
      { mktyp(Ptyp_arrow($1, $3, $5, $9, $7)) }
;
simple_core_type:
    simple_core_type2
      { $1 }
  | LPAREN core_type_comma_list RPAREN
      {
	match $2 with
	  [sty] -> mktyp (Ptyp_paren sty)
        | _ -> raise Parse_error
      }
;
simple_core_type2:
    QUOTE ident
      { mktyp(Ptyp_var $2) }
  | UNDERSCORE
      { mktyp(ptyp_any ()) }
  | type_longident %prec prec_constr_appl
      { mktyp(Ptyp_constr($1, [])) }
  | simple_core_type2 type_longident %prec prec_constr_appl
      { mktyp(Ptyp_constr($2, [$1])) }
  | LPAREN core_type_comma_list RPAREN type_longident %prec prec_constr_appl
      { mktyp(Ptyp_constr($4, List.rev $2)) }
;
simple_core_type_or_tuple:
    simple_core_type                            { $1 }
  | simple_core_type STAR core_type_list
      { mktyp(Ptyp_tuple($1 :: List.rev $3)) }
;
core_type_comma_list:
    core_type                                   { [$1] }
  | core_type_comma_list COMMA core_type        { $3 :: $1 }
;
core_type_list:
    simple_core_type                            { [$1] }
  | core_type_list STAR simple_core_type        { $3 :: $1 }
;



/* -------------------------------------------------------------------------
   Type schemes
 */

core_type_scheme:
    extcore_type
       { mktys $1 [] }
  | extcore_type WITH core_type_constraints_list
       { mktys $1 (List.rev $3) }
;
core_type_constraints_list:
    core_type_constraint                                 { [$1] }
  | core_type_constraints_list AND core_type_constraint  { $3 :: $1 }
;
core_type_constraint:
    left_hand_side LESS right_hand_side
       { mktyc(Ptyc_leq($1, $3)) }
  | core_type_skeleton
      { mktyc(Ptyc_ssk($1)) }
;
core_type_skeleton:
    extcore_type TILDE extcore_type                   { [$3; $1] }
  | core_type_skeleton TILDE extcore_type             { $3 :: $1 }
;
left_hand_side:
    extcore_type                                      { (NoDestr, $1) :: [] }
  | CONTENT extcore_type                              { (Destr, $2) :: [] }
  | CONTENT TILDE extcore_type                        { (SkDestr, $3) :: [] }
  | extcore_type COMMA left_hand_side                 { (NoDestr, $1) :: $3 }
  | CONTENT extcore_type COMMA left_hand_side         { (Destr, $2) :: $4 }
  | CONTENT TILDE extcore_type COMMA left_hand_side   { (SkDestr, $3) :: $5 }
;
right_hand_side:
    extcore_type                                      { (NoDestr, $1) :: [] }
  | LEVEL extcore_type                                { (Destr, $2) :: [] }
  | LEVEL TILDE extcore_type                          { (SkDestr, $3) :: [] }
  | extcore_type COMMA right_hand_side                { (NoDestr, $1) :: $3 }
  | LEVEL extcore_type COMMA right_hand_side          { (Destr, $2) :: $4 }
  | LEVEL TILDE extcore_type COMMA right_hand_side    { (SkDestr, $3) :: $5 }
;



/* -------------------------------------------------------------------------
   Bounds in core types
 */

core_type_bounds:
    LBRACKETLESS level_comma_list RBRACKET
      { [], $2 }
  | LBRACKETGREATER level_comma_list RBRACKET
      { $2, [] }
  | LBRACKETGREATER level_comma_list INFIXOP0 level_comma_list RBRACKET
      { if $3 <> "|<" then
	raise (Syntaxerr.Error (Syntaxerr.Other (rhs_loc 3)));
        $2, $4 }
  | LBRACKETLESS level_comma_list INFIXOP0 level_comma_list RBRACKET
      { if $3 <> "|>" then
	raise (Syntaxerr.Error (Syntaxerr.Other (rhs_loc 3)));
        $4, $2 }
  | level
      { [$1], [$1] }
;
core_type_row_bounds:
    STAR COLON core_type_bounds
      { $3 }
;



/* -------------------------------------------------------------------------
   Level declaration
 */

level_declaration:
    UIDENT
      { $1, mklvd ([], []) }
  | UIDENT level_repr
      { $1, mklvd $2 }
;
level_repr:
    GREATERWORD THAN level_comma_list
      { $3, [] }
  | GREATERWORD THAN level_comma_list LESSWORD THAN level_comma_list
      { $3, $6 }
  | LESSWORD THAN level_comma_list
      { [], $3 }
  | EQUAL level
      { [$2], [$2] }
;
level_comma_list:
    /*empty*/                                   { [] }
  | level                                       { [$1] }
  | level_comma_list COMMA level                { $3 :: $1 }
;
level:
    level_longident                             { mklvl (Plvl_ident $1) }
  | principal                                   { mklvl (Plvl_principal $1) }
;



/* -------------------------------------------------------------------------
   Constants
 */

constant:
    INT                                { Const_int $1 }
  | CHAR                               { Const_char $1 }
  | STRING                             { Const_string $1 }
  | FLOAT                              { Const_float $1 }
  | CHARVECT                           { Const_charray ($1, symbol_rloc()) }
;
signed_constant:
    constant                                    { $1 }
  | MINUS INT                                   { Const_int(- $2) }
  | subtractive FLOAT                           { Const_float("-" ^ $2) }
;



/* -------------------------------------------------------------------------
   Identifiers and long identifiers
 */

ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
;
val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
;
val_ident_colon:
    LIDENT COLON                                { $1 }
  | LPAREN operator RPAREN COLON                { $2 }
  | LABEL                                       { $1 }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | PLUS                                        { "+" }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
;
constr_ident:
    UIDENT                                      { $1 }
  | LBRACKET RBRACKET                           { "[]" }
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;
val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident                               { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;
label_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;
type_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
simple_exception_longident:
    mod_longident                               { $1 }
;
exception_longident:
    UIDENT                                      { Lident $1 }
  | LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
;
level_longident:
    UIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
;
level_strict_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN
                                                { Lapply($1, $3) }
;
mty_longident:
    ident                                       { Lident $1 }
  | mod_ext_longident DOT ident                 { Ldot($1, $3) }
;
principal:
    PREFIXOP LIDENT
      { if $1 <> "!" then
	raise (Syntaxerr.Error (Syntaxerr.Other (rhs_loc 1)));
	$2 }
;



/* -------------------------------------------------------------------------
   Toplevel directives
 */

toplevel_directive:
    SHARP ident                 { Ptop_dir($2, Pdir_none) }
  | SHARP ident STRING          { Ptop_dir($2, Pdir_string $3) }
  | SHARP ident INT             { Ptop_dir($2, Pdir_int $3) }
  | SHARP ident val_longident   { Ptop_dir($2, Pdir_ident $3) }
  | SHARP ident FALSE           { Ptop_dir($2, Pdir_bool false) }
  | SHARP ident TRUE            { Ptop_dir($2, Pdir_bool true) }
;



/* -------------------------------------------------------------------------
   Miscellaneous
 */

rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;




%%
