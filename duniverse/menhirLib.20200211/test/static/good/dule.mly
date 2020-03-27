/* Copyright (C) 2003 Mikolaj Konarski
 *
 * based on parser of Objective Caml http://caml.inria.fr/
 * syntax influenced among others by Camlp4
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: parser.mly,v 1.49 2005/03/06 02:18:12 mikon Exp $
 */

%{

open Tools
open Tools.IList
open Core_front.BCore
open Mod_front.EDule

let symbol_rloc () =
  Error_rep.Location.int2t (Parsing.symbol_start()) (Parsing.symbol_end())

let rhs_loc n =
  Error_rep.Location.int2t (Parsing.rhs_start n) (Parsing.rhs_end n)

let mktyp f = (symbol_rloc(), f)
let mkvalu t = (symbol_rloc(), t)
let mksp p = (symbol_rloc(), p)
let mkdule m = (symbol_rloc(), m)
let mkstart li = (symbol_rloc(), li)
let mkwild () = IdIndex.loc2wild (Error_rep.Location.t2string (symbol_rloc ()))
let mkpatid () = IdIndex.loc2patt (Error_rep.Location.t2string (symbol_rloc ()))
let mkpat lt = let f = mktyp(F_x) in
	       let lf = vmap (fun _ -> f) lt in
	       mktyp (F_pp lf)
let mkint n =
  if n < 0 then failwith "Negative integers not implemented!"
  else
    let i2pr i =
      mkvalu(T_comp (mkvalu(T_pr AtIndex.mNat),
		     mkvalu(T_pr i)))
    in
    let rec n2con n =
      if n = 0 then
	i2pr AtIndex.zero
      else
	let acc = n2con (n - 1) in
	mkvalu(T_appl (cons (AtIndex.arg_of_succ, acc) nil,
		       i2pr AtIndex.succ))
    in
    n2con n

let unclosed opening_name opening_num closing_name closing_num =
  raise(Error_rep.TextualError.TextualError
	  (Error_rep.TextualError.makeUnclosed
	     (rhs_loc opening_num) opening_name
	     (rhs_loc closing_num) closing_name))

%}

/* tokens, most are from OCaml and not in use, for now
   most of these token definitions are taken verbatim from the OCaml parser
   (the only other two such cases are in lexer.mll)
   with the oral permission obtained on ETAPS 2003 from Xavier Leroy
   to take small snippets, if necessary (sigh, those non-free licences): */
%token AMPERAMPER
%token AMPERSAND
%token AND
%token ASSERT
%token BACKQUOTE
%token BAR
%token BARBAR
%token BARRBRACKET
%token <char> CHAR
%token COIND
%token COLON
%token COLONCOLON
%token COLONGREATER
%token COMMA
%token CON
%token DE
%token DOT
%token DOTDOT
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token FAIL
%token <string> FLOAT
%token FOLD
%token FUN
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token IND
%token <int> INT
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LESS
%token LET
%token LIBRARY
%token <string> LIDENT
%token LINK
%token LOAD
%token LPAREN
%token MAP
%token MATCH
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token OF
%token OPEN
%token PLUS
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
%token SPEC
%token STAR
%token <string> STRING
%token STRUCT
%token THEN
%token TILDE
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token UNCON
%token UNDE
%token UNFOLD
%token VALUE
%token WHEN
%token WITH

/* Precedences and associativities, where lower precedences come first. */
/* This has to be thought of and improved basing on OCaml parser */

%nonassoc COLONGREATER
%left     WITH
%left     TILDE
%nonassoc prec_argument
%nonassoc LPAREN
%left     BAR DOT

/* entry points */

%start start
%type <Error_rep.Location.t * Mod_front.EDule.link_item list> start
%start core
%type <Error_rep.Location.t * Core_front.BCore.valu'> core

%%

/* the Dule language */

/* labels */

type_label:
    LIDENT                                      { IdIndex.s2type $1 }
;
value_label:
    LIDENT                                      { IdIndex.s2value $1 }
;
case_label:
    UIDENT                                      { IdIndex.s2case $1 }
  | LIDENT                                      { IdIndex.s2case $1 }
;
dule_label:
    UIDENT                                      { IdIndex.s2dule $1 }
;
sp_label:
    UIDENT                                      { IdIndex.s2sp $1 }
;

/* types */

list0_field_typ:
    /* empty */                                 { nil }
  | list1_field_typ                             { $1 }
;
list1_field_typ:
    field_typ                                   { cons $1 nil }
  | list1_field_typ SEMI field_typ              { econs $3 $1 }
;
field_typ:
    value_label COLON typ                       { ($1, $3) }
;
list0_case_typ:
    /* empty */                                 { nil }
  | list1_case_typ                              { $1 }
;
list1_case_typ:
    case_typ                                    { cons $1 nil }
  | list1_case_typ BAR case_typ                 { econs $3 $1 }
;
case_typ:
  | BACKQUOTE case_label typ                    { ($2, $3) }
  | BACKQUOTE case_label                        { ($2, mktyp(F_pp nil)) }
;
list0_param_typ:
    /* empty */                                 { nil }
  | list0_param_typ param_typ                   { econs $2 $1 }
;
param_typ:
    TILDE value_label COLON typ                 { ($2, $4) }
;
typ:
  | typ DOT typ
      { mktyp(F_COMP ($1, $3)) }
  | type_label
      { mktyp(F_PR $1) }
  | dule_label
      { mktyp(F_PR $1) }
  | LBRACE list0_field_typ RBRACE
      { mktyp(F_pp $2) }
  | LBRACE list0_field_typ error
      { unclosed "{" 1 "}" 3 }
  | LBRACKET list0_case_typ RBRACKET
      { mktyp(F_ss $2) }
  | LBRACKET list0_case_typ error
      { unclosed "[" 1 "]" 3 }
  | list0_param_typ MINUSGREATER typ
      { mktyp(F_ee ($1, $3)) }
  | IND type_label COLON typ
      { mktyp(F_ii ($2, $4)) }
  | COIND type_label COLON typ
      { mktyp(F_tt ($2, $4)) }
  | LPAREN typ RPAREN
      { $2 }
  | LPAREN typ error
      { unclosed "(" 1 ")" 3 }
;

/* values */

list0_field:
    /* empty */                                 { nil }
  | list1_field                                 { $1 }
;
list1_field:
    field                                       { cons $1 nil }
  | list1_field SEMI field                      { econs $3 $1 }
;
field:
    value_label EQUAL valu                      { ($1, $3) }
  | value_label                                 { ($1, mkvalu(T_pr $1)) }
;
list0_case:
    /* empty */                                 { nil }
  | list1_case                                  { $1 }
;
list1_case:
    case                                        { cons $1 nil }
  | list1_case BAR case                         { econs $3 $1 }
;
case:
  | BACKQUOTE case_label embedding                { ($2, $3) }
;
embedding:
  | valu
      { $1 }
  | pattern MINUSGREATER valu
      { mkvalu(T_curry (cons (AtIndex.it, $1) nil, $3)) }
  | MINUSGREATER valu
      { mkvalu(T_curry (cons (AtIndex.it, (mkwild(), mktyp(F_x))) nil, $2)) }
;
list1_argument:
    argument                                    { cons $1 nil }
  | list1_argument argument                     { econs $2 $1 }
;
argument:
    TILDE value_label COLON valu %prec prec_argument
      { ($2, $4) }
  | TILDE value_label
      { ($2, mkvalu(T_pr $2)) }
;
list0_declaration:
    /* empty */                                 { nil }
  | list0_declaration declaration               { econs $2 $1 }
;
declaration:
    pattern EQUAL valu
      { let (k, f) = $1 in
      (k, (f, $3)) }
  | REC pattern EQUAL valu
      { let (k, f) = $2 in
      (k, (f, mkvalu(T_fix (k, $4)))) }
;
list0_param:
    /* empty */                                 { nil }
  | list0_param param                           { econs $2 $1 }
;
param:
    TILDE value_label                           { ($2, ($2, mktyp(F_x))) }
  | TILDE value_label COLON pattern             { ($2, $4) }
  | TILDE LPAREN value_label COLON typ RPAREN   { ($3, ($3, $5)) }
  | TILDE LPAREN value_label COLON typ error    { unclosed "(" 2 ")" 6 }
;
pattern:
    value_label COLON typ                       { ($1, $3) }
  | value_label                                 { ($1, mktyp(F_x)) }
  | UNDERSCORE                                  { (mkwild(), mktyp(F_x)) }
  | LBRACE list0_field RBRACE                   { (mkpatid(), mkpat($2)) }
  | LBRACE list0_field error                    { unclosed "{" 1 "}" 3 }
;
partial_arguments:
    LPAREN list1_argument RPAREN
      { $2 }
  | LPAREN list1_argument error
      { unclosed "(" 1 ")" 3 }
;
valu:
  | COLON typ
      { mkvalu(T_id $2) }
  | valu DOT valu
      { mkvalu(T_comp ($1, $3)) }
  | value_label
      { mkvalu(T_pr $1) }
  | dule_label
      { mkvalu(T_pr $1) }
  | LBRACE list0_field RBRACE
      { mkvalu(T_record $2) }
  | LBRACE list0_field error
      { unclosed "{" 1 "}" 3 }
  | valu DOT BACKQUOTE case_label
      { mkvalu(T_comp ($1, mkvalu(T_in $4))) }
  | BACKQUOTE case_label
      { mkvalu(T_comp (mkvalu(T_record nil), mkvalu(T_in $2))) }
  | LBRACKET list0_case RBRACKET
      { mkvalu(T_case $2) }
  | LBRACKET list0_case error
      { unclosed "[" 1 "]" 3 }
  | MAP embedding
      { mkvalu(T_map $2) }
  | CON
      { mkvalu(T_con) }
  | FOLD embedding
      { mkvalu(T_fold $2) }
  | DE
      { mkvalu(T_de) }
  | UNCON
      { mkvalu(T_uncon) }
  | UNFOLD embedding
      { mkvalu(T_unfold $2) }
  | UNDE
      { mkvalu(T_unde) }
  | valu list1_argument
      { mkvalu(T_appl ($2, $1)) }
  | valu TILDE
      { mkvalu(T_appl (nil, $1)) }
  | MATCH valu WITH valu
      { mkvalu(T_appl (cons (AtIndex.it, $2) nil, $4)) }
  | LET list0_declaration IN valu
      { mkvalu(let lift = $2 in
              let lt = vmap (fun (f, t) -> t) lift in
              let lif = bmap (fun (k, (f, t)) -> (k, f)) lift in
	      T_appl (lt, (mkvalu(T_curry (lif, $4))))) }
  | FUN list0_param MINUSGREATER valu
      { mkvalu(T_curry ($2, $4)) }
  | valu partial_arguments
      { mkvalu(T_pappl ($2, $1)) }
  | IF valu THEN valu ELSE valu
      { let case =
	  cons (AtIndex.tt,
		mkvalu(T_curry (cons (AtIndex.it,
				      (mkwild(), mktyp(F_x))) nil, $4)))
	 (cons (AtIndex.ff,
		mkvalu(T_curry (cons (AtIndex.it,
				      (mkwild(), mktyp(F_x))) nil, $6)))
	  nil)
        in
        mkvalu(T_appl (econs (AtIndex.it, $2) nil,
		       mkvalu(T_case case))) }
  | ASSERT valu IN valu
      { mkvalu(T_assert ($2, $4)) }
  | FAIL
      { mkvalu(T_fail) }
  | INT
      { mkint($1) }
  | LPAREN valu RPAREN
      { $2 }
  | LPAREN valu error
      { unclosed "(" 1 ")" 3 }
;

/* specifications */

list0_field_sp:
    /* empty */                                 { nil }
  | list1_field_sp                              { $1 }
;
list1_field_sp:
    field_sp                                    { econs $1 nil }
  | list1_field_sp SEMI field_sp                { econs $3 $1 }
;
field_sp:
    dule_label COLON sp
      { ($1, $3) }
  | dule_label
      { ($1, mksp(S_Ii (AtIndex.dule2sp $1))) }
;
list0_param_sp:
    /* empty */                                 { nil }
  | list0_param_sp param_sp                     { econs $2 $1 }
;
param_sp:
    TILDE field_sp                              { $2 }
;
list0_bb_item:
    /* empty */                                 { nil }
  | list0_bb_item bb_item                       { econs $2 $1 }
;
bb_item:
    TYPE type_label                             { ($2, Bb_type) }
  | VALUE value_label COLON typ                 { ($2, Bb_value $4) }
;
sp:
  | LBRACE list0_field_sp RBRACE
      { mksp(S_Aa $2) }
  | LBRACE list0_field_sp error
      { unclosed "{" 1 "}" 3 }
  | LBRACE LBRACE list0_field_sp RBRACE RBRACE
      { mksp(S_Cc $3) }
  | LBRACE LBRACE list0_field_sp error
      { unclosed "{{" 1 "}}" 4 }
  | list0_param_sp MINUSGREATER sp
      { mksp(S_Ee ($1, $3)) }
  | SIG list0_bb_item END
      { mksp(S_Bb $2) }
  | SIG list0_bb_item error
      { unclosed "sig" 1 "end" 3 }
  | sp WITH dule
      { mksp(S_Ww ($3, $1)) }
  | sp_label
      { mksp(S_Ii $1) }
  | LPAREN sp RPAREN
      { $2 }
  | LPAREN sp error
      { unclosed "(" 1 ")" 3 }
;

/* modules */

list0_field_dule:
    /* empty */                                 { nil }
  | list1_field_dule                            { $1 }
;
list1_field_dule:
    field_dule                                  { econs $1 nil }
  | list1_field_dule SEMI field_dule            { econs $3 $1 }
;
field_dule:
    dule_label EQUAL dule                       { ($1, $3) }
  | dule_label                                  { ($1, mkdule(M_Pr $1)) }
;
list0_base_item:
    /* empty */                                 { nil }
  | list0_base_item base_item                   { econs $2 $1 }
;
base_item:
    TYPE type_label EQUAL typ
      { ($2, Base_type $4) }
  | VALUE value_label EQUAL valu
      { ($2, Base_value $4) }
  | VALUE REC value_label EQUAL valu
      { ($3, Base_value (mkvalu(T_fix ($3, $5)))) }
;
def_dule:
    dule_label EQUAL dule                       { ($1, $3) }
;
def_sp:
    sp_label EQUAL sp                           { ($1, $3) }
;
one_dule:
    def_dule                                    { econs $1 nil }
  | one_dule AND def_dule                       { econs $3 $1 }
;
one_sp:
    def_sp                                      { econs $1 nil }
  | one_sp AND def_sp                           { econs $3 $1 }
;
list0_link_item:
    /* empty */                                 { [] }
  | list0_link_item link_item                   { $2 :: $1 }
;
link_item:
    def_dule
      { let (i, m) = $1 in Link_Dule (i, m) }
  | MODULE def_dule
      { let (i, m) = $2 in Link_Dule (i, m) }
  | MODULE IND one_dule                         { Link_Ind_Dule $3 }
  | MODULE COIND one_dule                       { Link_CoInd_Dule $3 }
  | SPEC def_sp
      { let (i, p) = $2 in Link_Sp (i, p) }
  | SPEC REC one_sp                             { Link_Rec_Sp $3 }
  | LIBRARY def_dule
      { let (i, m) = $2 in Link_Lib (i, m) }
  | LIBRARY IND one_dule                        { Link_Ind_Lib $3 }
  | LIBRARY COIND one_dule                      { Link_CoInd_Lib $3 }
;
dule:
    COLON LBRACE LBRACE list0_field_sp RBRACE RBRACE
      { mkdule(M_Id $4) }
  | COLON LBRACE LBRACE sp error
      { unclosed ": {{" 2 "}}" 5 }
  | dule DOT dule
      { mkdule(M_Comp ($1, $3)) }
  | dule_label
      { mkdule(M_Pr $1) }
  | LBRACE list0_field_dule RBRACE
      { mkdule(M_Accord $2) }
  | LBRACE list0_field_dule error
      { unclosed "{" 1 "}" 3 }
  | LBRACE LBRACE list0_field_dule RBRACE RBRACE
      { mkdule(M_Concord $3) }
  | LBRACE LBRACE list0_field_dule error
      { unclosed "{{" 1 "}}" 4 }
  | COLONCOLON sp dule
      { mkdule(M_Spec ($3, $2)) }
  | STRUCT list0_base_item END
      { mkdule(M_Base $2) }
  | STRUCT list0_base_item error
      { unclosed "struct" 1 "end" 3 }
  | dule BAR dule
      { mkdule(M_Inst ($1, $3)) }
  | dule WITH dule
      { mkdule(M_With ($3, $1)) }
  | dule COLONGREATER sp
      { mkdule(M_Trim ($1, $3)) }
  | LINK list0_link_item END
      { mkdule(M_Link $2) }
  | LINK list0_link_item error
      { unclosed "link" 1 "end" 3 }
  | LOAD dule_label
      { mkdule(M_Load $2) }
  | LPAREN dule RPAREN
      { $2 }
  | LPAREN dule error
      { unclosed "(" 1 ")" 3 }
;

/* entry points */

start:
    list0_link_item EOF                         { mkstart($1) }
;

core:
    valu EOF                                    { $1 }
;

%%
