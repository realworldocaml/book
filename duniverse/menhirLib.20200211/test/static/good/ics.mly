/*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *
 * Author: Harald Ruess
 */

/*s Module [Parser]: parser for ICS syntactic categories. */

%{

(** Command parser. *)

  open Mpa
  open Tools

let name_of_slack = Name.of_string "k"
let name_of_zero_slack = Name.of_string "k0"
let name_of_rename = Name.of_string "v"


%}

%token DROP CAN SIMPLIFY ASSERT EXIT SAVE RESTORE REMOVE FORGET RESET SYMTAB SIG VALID UNSAT
%token TYPE SIGMA
%token SOLVE HELP DEF PROP TOGGLE SET GET TRACE UNTRACE CMP FIND USE INV SOLUTION PARTITION MODEL
%token SHOW SIGN DOM SYNTAX COMMANDS SPLIT SAT ECHO CHECK UNDO
%token DISEQ CTXT
%token IN NOTIN TT FF
%token EOF QUOTE

%token <string> IDENT
%token <string> STRING
%token <int> INTCONST
%token <Mpa.Q.t> RATCONST

%token BOT INT REAL BV TOP
%token INF NEGINF
%token ALBRA ACLBRA CLBRA

%token LPAR RPAR LBRA RBRA LCUR RCUR PROPLPAR PROPRPAR UNDERSCORE KLAMMERAFFE
%token COLON COMMA DOT DDOT ASSIGN TO ENDMARKER BACKSLASH
%token EMPTY FULL UNION INTER COMPL DIFF

%token <string> BVCONST
%token <string * int> FRESH
%token <int> FREE

%token CONC SUB BWITE BWAND BWOR BWXOR BWIMP BWIFF BWNOT
%token BVCONC
%token EQUAL SUBSET
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE EXPT
%token LESS GREATER LESSOREQUAL GREATEROREQUAL
%token UNSIGNED APPLY LAMBDA S K I C
%token WITH CONS CAR CDR NIL
%token INL INR OUTL OUTR
%token INJ OUT
%token HEAD TAIL LISTCONS
%token PROPVAR DISJ XOR IMPL BIIMPL CONJ NEG
%token IF THEN ELSE END
%token PROJ
%token CREATE
%token SUP

%right DISJ XOR IMPL
%left BIIMPL CONJ
%nonassoc EQUAL DISEQ SUBSET LESS GREATER LESSOREQUAL GREATEROREQUAL
%left APPLY
%right UNION
%right INTER, DIFF
%nonassoc COMPL
%left MINUS PLUS
%left DIVIDE
%left TIMES
%right EXPT
%right LISTCONS
%right BVCONC
%right BWOR BWXOR BWIMP
%left BWAND BWIFF
%nonassoc TO
%nonassoc IN NOTIN
%nonassoc LCUR
%nonassoc LBRA
%nonassoc COLON
%nonassoc prec_unary

%type <Term.t> termeof
%type <Atom.t> atomeof
%type <Prop.t> propeof
%type <unit> commands
%type <unit> commandseof
%type <unit> commandsequence

%start termeof
%start atomeof
%start propeof
%start commands
%start commandseof
%start commandsequence


%%

termeof : term EOF           { $1 }
atomeof : atom EOF           { $1 }
propeof : prop EOF           { $1 }
commandseof : command EOF    { () }

commands :
  command DOT     { () }
| EOF             { raise End_of_file }
;

commandsequence :
  command DOT commandsequence    { () }
| command DOT                    { () }
| EOF                            { raise End_of_file }


prop:
  LBRA prop RBRA                  { $2 }
/* | LPAR prop RPAR                  { $2 } */
| name                            { try Istate.prop_of $1 with Not_found -> Prop.mk_var $1 }
| atom                            { Prop.mk_poslit $1 }
| prop CONJ prop                  { Prop.mk_conj [$1; $3] }
| prop DISJ prop                  { Prop.mk_disj [$1; $3] }
| prop BIIMPL prop                { Prop.mk_iff $1 $3 }
| prop XOR prop                   { Prop.mk_neg (Prop.mk_iff $1 $3) }
| prop IMPL prop                  { Prop.mk_disj [Prop.mk_neg $1; $3] }
| NEG prop %prec prec_unary       { Prop.mk_neg $2 }
| IF prop THEN prop ELSE prop END { Prop.mk_ite $2 $4 $6 }
;

int: INTCONST  { $1 }

rat:
  int       { Q.of_int $1 }
| RATCONST  { $1 }
;

name: IDENT  { Name.of_string $1 }

namelist:
  name                { [$1] }
| namelist COMMA name { $3 :: $1 }
;


term:
  var              { $1 }
| app              { $1 }
| LPAR term RPAR   { $2 }
| arith            { $1 }     /* infix/mixfix syntax */
| array            { $1 }
| bv               { $1 }
| product          { $1 }
| boolean          { $1 }
| coproduct        { $1 }
| list             { $1 }
| apply            { $1 }
| propset          { $1 }
;

var:
  name  { try
	    match Istate.entry_of $1 with
	      | Symtab.Def(Symtab.Term(a)) -> a
	      | Symtab.Type(c)  -> Term.Var.mk_var $1 c
	      | _ -> Term.Var.mk_var $1 Var.Cnstrnt.Unconstrained
	  with
	      Not_found -> Term.Var.mk_var $1 Var.Cnstrnt.Unconstrained }
| name LCUR cnstrnt RCUR
         { Term.Var.mk_var $1 $3 }
;


app: funsym LPAR termlist RPAR     { Term.App.mk_app $1 (List.rev $3) }

funsym: name                       { Sym.Uninterp.make $1 }

list:
  term LISTCONS term            { Coproduct.mk_inj 1 (Product.mk_cons $1 $3) }
| HEAD LPAR term RPAR           { Product.mk_car (Coproduct.mk_out 1 $3) }
| TAIL LPAR term RPAR           { Product.mk_cdr (Coproduct.mk_out 1 $3) }
| NIL                           { Coproduct.mk_inj 0 (Bitvector.mk_eps()) }
;

apply:
  term APPLY term               { Apply.mk_apply $1 $3 }
| S                             { Apply.mk_s () }
| K                             { Apply.mk_k () }
| I                             { Apply.mk_i () }
| C                             { Apply.mk_c () }
| LAMBDA namelist COLON term    { let nl = $2 in   (* in reverse order! *)
				  let body = $4 in
				  let rec abstract_star acc = function
				    | [] -> assert false
				    | [n] -> Apply.abstract n acc
				    | n :: nl -> abstract_star (Apply.abstract n acc) nl
				  in
				  abstract_star body nl }
;

boolean:
  TRUE                          { Boolean.mk_true() }
| FALSE                         { Boolean.mk_false() }
;

arith:
  rat                           { Arith.mk_num $1 }
| term PLUS term                { Arith.mk_add $1 $3 }
| term MINUS term               { Arith.mk_sub $1 $3 }
| MINUS term %prec prec_unary   { Arith.mk_neg $2 }
| term TIMES term               { Pprod.mk_mult $1 $3 }
| term EXPT int                 { Pprod.mk_expt $1 $3 }
;

product:
  CONS LPAR term COMMA term RPAR { Product.mk_cons $3 $5 }
| CAR LPAR term RPAR             { Product.mk_car $3 }
| CDR LPAR term RPAR             { Product.mk_cdr $3 }
;

coproduct:
  INL LPAR term RPAR                    { Coproduct.mk_inl $3 }
| INR LPAR term RPAR                    { Coproduct.mk_inr $3 }
| OUTL LPAR term RPAR                   { Coproduct.mk_outl $3 }
| OUTR LPAR term RPAR                   { Coproduct.mk_outr $3 }
| INJ LBRA INTCONST RBRA LPAR term RPAR { Coproduct.mk_inj $3 $6 }
| OUT LBRA INTCONST RBRA LPAR term RPAR { Coproduct.mk_out $3 $6 }
;

array:
  CREATE LPAR term RPAR      { Funarr.mk_create $3 }
| term LBRA term ASSIGN term RBRA { Funarr.mk_update Term.is_equal $1 $3 $5 }
| term LBRA term RBRA        { Funarr.mk_select Term.is_equal $1 $3 }
;


propset:
  EMPTY                          { Propset.mk_empty() }
| FULL                           { Propset.mk_full() }
| term UNION term                { Propset.mk_union $1 $3 }
| term INTER term                { Propset.mk_inter $1 $3 }
| COMPL term %prec prec_unary    { Propset.mk_compl $2 }
;


bv:
  BVCONST                     { Bitvector.mk_const (Bitv.from_string $1)  }
| CONC LBRA INTCONST COMMA INTCONST RBRA LPAR term COMMA term RPAR
                              { Bitvector.mk_conc $3 $5 $8 $10 }
| SUB LBRA INTCONST COMMA INTCONST COMMA INTCONST RBRA LPAR term RPAR
                              { Bitvector.mk_sub $3 $5 $7 $10 }
| term BVCONC term
     { match Istate.width_of $1, Istate.width_of $3 with
	 | Some(n), Some(m) ->
	     if n < 0 then
	       raise (Invalid_argument ("Negative length of " ^ Term.to_string $1))
	     else if m < 0 then
	       raise (Invalid_argument ("Negative length of " ^ Term.to_string $3))
	     else
	       Bitvector.mk_conc n m $1 $3
	 | Some _, _ ->
	     raise (Invalid_argument (Term.to_string $3 ^ " not a bitvector."))
	 | _ ->
	     raise (Invalid_argument (Term.to_string $1 ^ " not a bitvector.")) }
| term LBRA INTCONST COLON INTCONST RBRA
     { match Istate.width_of $1 with
	 | Some(n) ->
	     if n < 0 then
	       raise(Invalid_argument ("Negative length of " ^ Term.to_string $1))
	     else if not(0 <= $3 && $3 <= $5 && $5 < n) then
	       raise(Invalid_argument ("Invalid extraction from " ^ Term.to_string $1))
	     else
	       Bitvector.mk_sub n $3 $5 $1
	 | None ->
	     raise (Invalid_argument (Term.to_string $1 ^ " not a bitvector.")) }
;


atom:
  FF                       { Atom.mk_false }
| TT                       { Atom.mk_true }
| term EQUAL term          { Atom.mk_equal ($1, $3) }
| term DISEQ term          { Atom.mk_diseq ($1, $3) }
| term LESS term           { Atom.mk_pos (Arith.mk_sub $3 $1) }   /* [a < b] iff [b - a > 0] */
| term GREATER term        { Atom.mk_pos (Arith.mk_sub $1 $3) }   /* [a > b] iff [a - b > 0] */
| term LESSOREQUAL term    { Atom.mk_nonneg (Arith.mk_sub $3 $1) }/* [a <= b] iff [b - a >= 0] */
| term GREATEROREQUAL term { Atom.mk_nonneg (Arith.mk_sub $1 $3) }/* [a >= b] iff [a - b >= 0] */
| term SUBSET term         { Atom.mk_equal (Propset.mk_inter $1 $3, $1) }
;

dom:
  INT          { Dom.Int }
| REAL         { Dom.Real }
;

cnstrnt:
  dom          { Var.Cnstrnt.Real($1) }
| signature    { Var.Cnstrnt.Bitvector($1) }
;

termlist:             { [] }
| term                { [$1] }
| termlist COMMA term { $3 :: $1 }
;

atomlist:             { [] }
| atom                { [$1] }
| atomlist COMMA atom { $3 :: $1 }
;

signature:
  BV LBRA INTCONST RBRA     { $3 }
;

command:
  CAN term                  { Istate.do_can $2 }
| SIMPLIFY atom             { Istate.do_simplify $2 }
| ASSERT optname atom       { Istate.do_process1 ($2, $3) }
| ASSERT optname atom COMMA atomlist { Istate.do_process ($2, $3 :: $5) }
| DEF name ASSIGN term      { Istate.do_def ($2, (Symtab.Term($4))) }
| PROP name ASSIGN prop     { Istate.do_prop ($2, $4) }
| SIG name COLON cnstrnt    { Istate.do_typ ([$2], $4) }
| SIG namelist COLON cnstrnt{ Istate.do_typ ($2, $4) }
| RESET                     { Istate.do_reset () }
| SAVE name                 { Istate.do_save(Some($2)) }
| SAVE                      { Istate.do_save(None) }
| RESTORE name              { Istate.do_restore $2 }
| REMOVE name               { Istate.do_remove $2 }
| FORGET                    { Istate.do_forget() }
| VALID optname atomlist    { Istate.do_valid ($2, $3) }
| UNSAT optname atomlist    { Istate.do_unsat ($2, $3) }
| EXIT                      { raise End_of_file }
| DROP                      { failwith "drop" }
| SYMTAB                    { Istate.do_symtab None }
| SYMTAB name               { Istate.do_symtab (Some($2)) }
| CTXT optname              { Istate.do_ctxt $2 }
| SIGMA term                { Istate.do_sigma $2 }
| term CMP term             { Istate.do_cmp ($1, $3) }
| SHOW optname              { Istate.do_show $2 None }
| SHOW optname eqth         { Istate.do_show $2 (Some($3)) }
| FIND optname eqth term    { Istate.do_find ($2, $3, $4) }
| INV optname th term       { Istate.do_inv ($2, $3, $4) }
| USE optname th term       { Istate.do_dep ($2, $3, $4) }
| DOM optname term          { Istate.do_dom ($2, $3) }
| DISEQ optname term        { Istate.do_diseq ($2, $3) }
| SPLIT optname             { Istate.do_split $2 }
| SOLVE th term EQUAL term  { Istate.do_solve ($2, ($3, $5)) }
| TRACE identlist           { Istate.do_trace $2 }
| UNTRACE                   { Istate.do_untrace None }
| UNTRACE identlist         { Istate.do_untrace (Some($2)) }
| SAT optname prop          { Istate.do_sat ($2, $3) }
| CHECK optname             { Istate.do_check_sat $2 }
| MODEL optname optvarspecs { Istate.do_model ($2, List.rev $3) }
| ECHO STRING               { Format.eprintf "%s@." $2 }
| GET varname               { Istate.do_get $2 }
| varname ASSIGN value      { Istate.do_set ($1, $3)}
| GET                       { Istate.do_show_vars () }
| SUP optname term          { Istate.do_sup ($2, $3) }
| INF optname term          { Istate.do_inf ($2, $3) }
| UNDO                      { Istate.do_undo () }
| help                      { $1 }
;

optvarspecs:               { [] }

varname : IDENT            { Istate.Parameters.of_string $1 }

value : IDENT              { $1 }
| TRUE                     { "true" }
| FALSE                    { "false" }
;

identlist :
  IDENT                     { [$1] }
| identlist COMMA IDENT     { $3 :: $1 }
;

th: IDENT  { Th.of_string $1 } /* may raise [Invalid_argument]. */

eqth : IDENT
  { try Some(Th.of_string $1)
    with exc -> if $1 = "v" then None else raise exc }

help:
  HELP                      { Istate.do_help Istate.All }
| HELP CAN                  { Istate.do_help (Istate.Command("can")) }
| HELP HELP                 { Istate.do_help (Istate.Command("help")) }
| HELP SIMPLIFY             { Istate.do_help (Istate.Command("simplify")) }
| HELP ASSERT               { Istate.do_help (Istate.Command("assert")) }
| HELP DEF                  { Istate.do_help (Istate.Command("def")) }
| HELP PROP                 { Istate.do_help (Istate.Command("prop")) }
| HELP SIG                  { Istate.do_help (Istate.Command("sig")) }
| HELP RESET                { Istate.do_help (Istate.Command("reset")) }
| HELP SAVE                 { Istate.do_help (Istate.Command("save")) }
| HELP RESTORE              { Istate.do_help (Istate.Command("restore")) }
| HELP REMOVE               { Istate.do_help (Istate.Command("remove")) }
| HELP FORGET               { Istate.do_help (Istate.Command("forget")) }
| HELP VALID                { Istate.do_help (Istate.Command("valid")) }
| HELP UNSAT                { Istate.do_help (Istate.Command("unsat")) }
| HELP EXIT                 { Istate.do_help (Istate.Command("exit")) }
| HELP DROP                 { Istate.do_help (Istate.Command("drop")) }
| HELP SYMTAB               { Istate.do_help (Istate.Command("symtab")) }
| HELP CTXT                 { Istate.do_help (Istate.Command("ctxt")) }
| HELP SIGMA                { Istate.do_help (Istate.Command("sigma")) }
| HELP CMP                  { Istate.do_help (Istate.Command("cmp")) }
| HELP SHOW                 { Istate.do_help (Istate.Command("show")) }
| HELP FIND                 { Istate.do_help (Istate.Command("find")) }
| HELP INV                  { Istate.do_help (Istate.Command("inv")) }
| HELP USE                  { Istate.do_help (Istate.Command("use")) }
| HELP DOM                  { Istate.do_help (Istate.Command("dom")) }
| HELP DISEQ                { Istate.do_help (Istate.Command("diseq")) }
| HELP SPLIT                { Istate.do_help (Istate.Command("split")) }
| HELP SOLVE                { Istate.do_help (Istate.Command("solve")) }
| HELP TRACE                { Istate.do_help (Istate.Command("trace")) }
| HELP UNTRACE              { Istate.do_help (Istate.Command("untrace")) }
| HELP SAT                  { Istate.do_help (Istate.Command("sat")) }
| HELP MODEL                { Istate.do_help (Istate.Command("model")) }
| HELP CHECK                { Istate.do_help (Istate.Command("check")) }
| HELP ECHO                 { Istate.do_help (Istate.Command("echo")) }
| HELP GET                  { Istate.do_help (Istate.Command("get")) }
| HELP SUP                  { Istate.do_help (Istate.Command("sup")) }
| HELP INF                  { Istate.do_help (Istate.Command("inf")) }
| HELP ASSIGN               { Istate.do_help (Istate.Command("set")) }
| HELP LESS IDENT GREATER   { Istate.do_help (Istate.Nonterminal($3)) }
| HELP UNDO                 { Istate.do_help (Istate.Command("undo")) }
;

optname:                    { None }
| KLAMMERAFFE name          { Some($2) }
;

%%
