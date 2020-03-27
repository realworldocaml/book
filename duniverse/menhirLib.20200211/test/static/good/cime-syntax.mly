/***************************************************************************

Input file parser

$Id: parser.mly,v 1.6 2001/04/20 13:42:27 marche Exp $

***************************************************************************/

%{

open Abstract_syntax;;
open C_declare_operator;;
open Symbols;;
open Terms;;
open Equations;;
open Equations_as_obj;;
open Term_orderings;;
open Rpo;;
(*
open Norm_theory;;
*)
open Poly_interp;;
open Errors;;

let current_poly_vars = ref ([] : string list)
;;

let monome_de_variable var =
  try
    IntPolynomials.var (Listutils.index var !current_poly_vars)
  with Not_found -> semantical_error "Undeclared polynomial variable"
;;

%}


%token <string> IDENT PREFIX_IDENT INFIX_IDENT POSTFIX_IDENT STRING
%token VIRGULE POINT_VIRGULE DEUX_POINTS GUILLEMET
%token PARGAUCHE PARDROITE CROGAUCHE CRODROIT
%token EGAL FLECHE
%token TOKEN_USER
%token TERMINAISON DP_TERMINATION
%token OPERATEURS THEORIE UNIFICATION EQUATIONS ORDRE PROBLEMES CONJECTURES
%token KW_END EOF
%token MATCHING
%token REDUIRE UNIFIER MATCHER COMPARER WITH LIBRE_UNIFIER
%token TOKEN_VARIABLE TOKEN_AC TOKEN_C CONSTANTE UNAIRE BINAIRE
%token PREFIXE INFIXE POSTFIXE
%token TOKEN_RPO TOKEN_RPS TOKEN_MAPO TOKEN_POLY
%token TOKEN_INTERACTIF SUPERIEUR INFERIEUR
%token TOKEN_SEMI_INTERACTIF MUL TOKEN_LEXICO LRLEX RLLEX
%token ONLY COMPLETE PLAIN TYPE
%token TOKEN_INCLUDE
%token INDUCTIVE

%token PLUS MINUS EXP MULT
%token <string> POLY_VAR
%token <int> INT

%token SORTES AS SUBSORTES

%start donnees
%type  <Abstract_syntax.abstract_syntax list> donnees

%start term_eof
%type <Terms.term> term_eof

%start signature_eof
%type <unit> signature_eof

%left INFIX_IDENT TERMLIST
%nonassoc PARDROITE

%left PLUS MINUS
%left MULT
%nonassoc UMINUS
%right EXP

%%

donnees:
  EOF                    { [] }
| KW_END                 { [] }
| declaration donnees    { $1::$2 }
;
declaration:
  TOKEN_INCLUDE STRING {Abstract_include $2}
| keyword_operators operateurs  	{ Abstract_operators $2 }
| keyword_theory theorie     	{ Abstract_theory $2 }
| UNIFICATION keyword_theory theorie   { Abstract_unification_theory $3 }
| UNIFICATION TYPE unif_type    { Abstract_unification_type $3 }
| keyword_axioms equations		{ Abstract_axioms $2 }
| keyword_order ordre_plus		{ Abstract_ordering $2 }
| keyword_problems  problemes		{ Abstract_problems $2 }
| keyword_conjectures equations		{ Abstract_conjectures $2 }
| keyword_sortes sortes                 { Abstract_sortes $2}
| keyword_subsortes subsortes           { Abstract_subsortes $2}
| keyword_inductive equations           { Abstract_inductive $2 }
;
keyword_operators:
  OPERATEURS {
  etat_analyse.top_allowed <- false;
  etat_analyse.lexer_state <- Ident_lexer }
;
keyword_conjectures:
  CONJECTURES {
  etat_analyse.top_allowed <- false;
  etat_analyse.lexer_state <- Term_lexer }
;
keyword_inductive:
  INDUCTIVE {
  etat_analyse.top_allowed <- false;
  etat_analyse.lexer_state <- Term_lexer }
;
keyword_theory:
  THEORIE {
  etat_analyse.top_allowed <- false;
  etat_analyse.lexer_state <- Ident_lexer }
;
keyword_axioms:
  EQUATIONS {
  etat_analyse.top_allowed <- false;
  etat_analyse.lexer_state <- Term_lexer }
;
keyword_problems:
  PROBLEMES {
  etat_analyse.top_allowed <- false;
  etat_analyse.lexer_state <- Term_lexer }
;
keyword_sortes:
  SORTES { etat_analyse.lexer_state <- Ident_lexer }
;
keyword_subsortes:
    SUBSORTES {etat_analyse.lexer_state <- Ident_lexer}
;
subsortes:
    base_sorte INFERIEUR base_sorte {[(see_as_base_sort $1,see_as_base_sort $3)]}
| base_sorte INFERIEUR base_sorte VIRGULE subsortes {(see_as_base_sort $1,see_as_base_sort $3):: $5 }
;
sortes:
    base_sorte {make_new_sort $1}
| base_sorte VIRGULE sortes {make_new_sort $1 ; $3}
;
keyword_order:
  ORDRE { etat_analyse.lexer_state <- Standard_lexer }
;
base_sorte:
    IDENT {$1}
;
signature_eof:
    operateurs EOF      { () }
;
operateurs:
  decl_arite             { [$1] }
| decl_arite operateurs  { $1::$2 }
| decl_arite POINT_VIRGULE             { [$1] }
| decl_arite POINT_VIRGULE operateurs  { $1::$3 }
;
profile_list:
    profile {[$1]}
|   profile VIRGULE profile_list {$1::$3};

profile:
  base_sorte {[$1]}
| base_sorte  FLECHE profile {$1::$3}

;
decl_arite:
  op_list_colon fix arity AS profile_list
    { let t,a = $3 in
        if $2=INFIX & a<>2
          then raise (Erreur_de_syntaxe "Infix symbols must be binary")
          else
	  if (List.exists (fun x -> (List.length x-1)<>a) $5)
	  then raise (Erreur_de_syntaxe "Profile must be compatible with arity")
	  else
	    begin
	      (List.iter (definir_operateur t a $2 (List.map see_as_functional_sort $5)) $1);
	      ($1,$2,$3)
	    end
	    }
| op_list_colon fix arity
    { let t,a = $3 in
        if $2=INFIX & a<>2
          then raise (Erreur_de_syntaxe "Infix symbols must be binary")
          else
(* a virer *)
	  if (List.exists (fun x -> (List.length x-1)<>a) [])
	  then raise (Erreur_de_syntaxe "Profile must be compatible with arity")
	  else
	    begin
	      (List.iter (definir_operateur t a $2 (List.map see_as_functional_sort [])) $1);
(* jusque la *)
	      ($1,$2,$3)
	    end
	    }
;
fix:
  PREFIXE { PREFIX }
| INFIXE  { INFIX  }
| POSTFIXE { POSTFIX }
|         { DEFAULT }
;
arity:
  arityaux       { etat_analyse.lexer_state <- Ident_lexer;$1 }
;
arityaux:
  TOKEN_C        { (C,2) }
| TOKEN_AC       { (AC,2) }
| TOKEN_VARIABLE { (VARIABLE,0) }
| CONSTANTE      { (FREE,0) }
| UNAIRE         { (FREE,1) }
| BINAIRE        { (FREE,2) }
| INT            { (FREE,$1) }
;
op_list_colon:
 op_list DEUX_POINTS  { etat_analyse.lexer_state <- Standard_lexer; $1 }
;
op_list:
  ident                  { [$1] }
| ident VIRGULE op_list  { $1::$3 }
;
ident:
  IDENT               { $1 }
/*
| PREFIX_IDENT        { $1 }
| INFIX_IDENT         { $1 }
| POSTFIX_IDENT        { $1 }
*/
;
/*
top_ident:
  ident                         { $1 }
| TOP PARGAUCHE ident PARDROITE { string_of_symbol_id (top_copy (get_symbol_id $3)) }
;
*/

unif_type:
  TOKEN_AC ONLY       { Unif_type_AC_only }
| TOKEN_AC COMPLETE   { Unif_type_AC_complete }
| PLAIN               { Unif_type_plain }
;
theorie:
  decl_theorie         { [$1] }
| decl_theorie theorie { $1::$2 }
;
decl_theorie:
  ident PARGAUCHE id_list_end { PRDF($1::$3) }
| keyword_user PARGAUCHE regle_list PARDROITE { etat_analyse.lexer_state <- Ident_lexer;USR($3) }
;
id_list_end:
  ident PARDROITE            { [$1] }
| ident VIRGULE id_list_end  { $1::$3 }
;
keyword_user:
  TOKEN_USER { etat_analyse.lexer_state <- Term_lexer }
;
regle_list:
  regle                          { [$1] }
| regle POINT_VIRGULE regle_list { $1::$3}
;
regle:
  term FLECHE term                  { make_basic_rule ((flatten_term $1),
                                                       (flatten_term $3)) }
;
equations:
  equation POINT_VIRGULE            { [$1] }
| equation POINT_VIRGULE equations  { $1::$3 }
;
equation:
  term EGAL term                    { new pair_of_terms (flatten_term $1)
                                      	(flatten_term $3) }
;

ordre_plus:
  ordre { $1 }
| TOKEN_SEMI_INTERACTIF PARGAUCHE ordre PARDROITE
        { SEMI_INTERACTIF($3) }
;

ordre:
  keyword_rpo PARGAUCHE precedence POINT_VIRGULE statuts PARDROITE
    { RPO($3,$5) }
| keyword_rpo PARGAUCHE precedence PARDROITE
    { RPO($3,[]) }
| keyword_mapo PARGAUCHE precedence PARDROITE
    { MAPO($3,[]) }
| keyword_mapo PARGAUCHE precedence POINT_VIRGULE statuts PARDROITE
    { MAPO($3,$5) }
| keyword_poly PARGAUCHE mu_value poly_decl_list PARDROITE
    { POLY($3,$4) }
| TOKEN_INTERACTIF
    { ORDRE_INTERACTIF }
| TOKEN_LEXICO PARGAUCHE ordre_List
    { LEXICO($3) }
| rps_header regle_list PARDROITE
    { Rps($1,$2) }
;
rps_header:
    TOKEN_RPS PARGAUCHE ordre POINT_VIRGULE
    {
  etat_analyse.top_allowed <- true;
  etat_analyse.lexer_state <- Term_lexer;$3 }
;
keyword_rpo:
  TOKEN_RPO {
  etat_analyse.top_allowed <- true;
  etat_analyse.lexer_state <- Ident_lexer }
;
keyword_mapo:
  TOKEN_MAPO {
  etat_analyse.top_allowed <- true;
  etat_analyse.lexer_state <- Ident_lexer }
;
mu_value:
  INT POINT_VIRGULE
             {
  etat_analyse.top_allowed <- true;
  etat_analyse.lexer_state <- Ident_lexer;$1 }
| /* epsilon */
             {
  etat_analyse.top_allowed <- true;
  etat_analyse.lexer_state <- Ident_lexer;0 }
;
keyword_poly:
  TOKEN_POLY { etat_analyse.lexer_state <- Standard_lexer }
;
ordre_List:
  ordre PARDROITE			{ [$1] }
| ordre POINT_VIRGULE ordre_List  	{ $1::$3 }
;
precedence:
  /* epsilon */                         { [] }
| liste_ordonnee  			{ [$1] }
| liste_ordonnee VIRGULE precedence	{ $1::$3 }
;
liste_ordonnee:
  ident  				{ One_symbol (get_symbol_id $1) }
| ident EGAL liste_ordonnee 		{ Equal_to (get_symbol_id $1,$3) }
| ident SUPERIEUR liste_ordonnee 	{ Greater_than (get_symbol_id $1,$3) }
| ident INFERIEUR liste_ordonnee 	{ Lower_than (get_symbol_id $1,$3) }
;
poly_decl_list:
  poly_decl 			{ [$1] }
| poly_decl POINT_VIRGULE poly_decl_list{ $1::$3 }
;
statuts:
  statut                  { [$1]   }
| statut VIRGULE statuts  { $1::$3 }
;
statut:
  ident MUL
    { (get_symbol_id $1),MULTISET
    }
| ident lexstat
    { let f=(get_symbol_id $1) in
      	if not (is_free f)
      	  then semantical_error	"Only free symbols can have a lexicographic status"
      	  else f,$2
    }
;
lexstat:
  LRLEX { LR_LEXICO }
| RLLEX { RL_LEXICO }
;
problemes:
  probleme POINT_VIRGULE            { [$1] }
| probleme POINT_VIRGULE problemes  { $1::$3 }
;
probleme:
  term EGAL term
    { EQUATION_A_PROUVER(flatten_term $1,flatten_term $3) }
| REDUIRE term
    { TERME_A_REDUIRE(flatten_term $2) }
| unif_type UNIFIER term EGAL term
    { EQUATION_A_RESOUDRE($1,flatten_term $3,flatten_term $5) }
| UNIFIER term EGAL term
    { EQUATION_A_RESOUDRE(Unif_type_default,flatten_term $2,flatten_term $4) }
| MATCHER term EGAL term
    { EQUATION_A_MATCHER(flatten_term $2,flatten_term $4) }
| COMPARER term WITH term
    { TERMES_A_COMPARER(flatten_term $2,flatten_term $4) }
/*
| TERMINAISON PARGAUCHE regle_list PARDROITE
    { TERMINAISON_A_PROUVER($3) }
| DP_TERMINATION PARGAUCHE regle_list PARDROITE
    { TERMINAISON_DP($3) }
*/
/*
| COMPARER keyword_poly poly_decl WITH poly_decl
    { etat_analyse.lexer_state <- Term_lexer;
      POLYS_A_COMPARER(snd $3,snd $5) }
*/
;
poly_decl:
  entete_poly EGAL poly
    {
  etat_analyse.top_allowed <- true;
  etat_analyse.lexer_state <- Ident_lexer;
      ($1,$3)
      }
;
entete_poly:
  CROGAUCHE poly_ident CRODROIT
    { current_poly_vars := [];
      $2 }
| CROGAUCHE poly_ident CRODROIT PARGAUCHE poly_var_list_end
    { current_poly_vars := $5;

      let test_simple list =
      	let rec build_simple = function
      	    ([],l)	-> l
      	  | ((a::s), l)	-> build_simple (s,Listutils.union l [a])
      	in
	  if ( (List.length list)=(List.length (build_simple (list,[]))) )
	  then ()
	  else
	    semantical_error
	      "Same variable symbols in polynomial declaration"
      in
      	test_simple !current_poly_vars;

      	let f = $2 in
	  if (arity f)<>(List.length !current_poly_vars)
	  then semantical_error
	    ((string_of_symbol_id $2)^" is supposed to be a "
      	     ^(string_of_int (arity f))^"-ary operator")
	  else f
    }
;
poly_ident:
  ident { etat_analyse.lexer_state <- Poly_lexer;
      	  (get_symbol_id $1) }
;
poly_var_list_end:
  POLY_VAR PARDROITE                  { [$1] }
| POLY_VAR VIRGULE poly_var_list_end  { $1::$3 }
;
poly:
  POLY_VAR
    { monome_de_variable $1 }
| INT
    { IntPolynomials.cte (Num.Int $1) }
| PARGAUCHE poly PARDROITE
    { $2 }
| poly PLUS poly
    { IntPolynomials.add $1 $3 }
| poly MINUS poly
    { IntPolynomials.sub $1 $3 }
| MINUS poly %prec UMINUS
    { IntPolynomials.minus $2 }
| poly MULT poly
    { IntPolynomials.mult $1 $3 }
| poly EXP INT
    { IntPolynomials.power $1 $3 }
;
term :
  PREFIX_IDENT
   { (* printf "forme a\n"; *)
      try
        VAR (var_id_of_string $1)
      with Not_found ->
        let f=(get_symbol_id $1) in
        if (arity f)=0
        then TERM(f,[])
        else semantical_error ("Bad number of arguments for " ^ $1)
     }
| PARGAUCHE term PARDROITE
     { $2
     }
| PREFIX_IDENT PARGAUCHE term_list PARDROITE
     { (* printf "forme f(...)\n"; *)
        let f=(get_symbol_id $1) in
        if (arity f)=(List.length $3)
        then TERM(f,$3)
        else semantical_error ("Bad number of arguments for " ^ $1)
    }
| PARGAUCHE term PARDROITE POSTFIX_IDENT
    { (* printf "forme f(...)\n"; *)
      let f=(get_symbol_id $4) in
        if (arity f)=1
          then TERM(f,[$2])
          else semantical_error ("Bad number of arguments for " ^ $4)
    }
| PARGAUCHE term_list PARDROITE POSTFIX_IDENT
    { (* printf "forme f(...)\n"; *)
      let f=(get_symbol_id $4) in
        if (arity f)=(List.length $2)
          then TERM(f,$2)
          else semantical_error ("Bad number of arguments for " ^ $4)
    }
| term INFIX_IDENT term
    { let f = (get_symbol_id $2) in
      TERM(f,[$1;$3])
    }
;

term_list:
  term %prec TERMLIST { [$1] }
| term VIRGULE term_list { $1 :: $3 }
;
term_eof:
  term EOF { $1 }
;
