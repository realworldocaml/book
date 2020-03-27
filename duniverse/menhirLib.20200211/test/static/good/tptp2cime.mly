%{

open Abs_syntax;;
open Symbols;;
open Terms;;
open Clauses;;

%}


%token <string> IDENT VRB FILE_NAME
%token PLUS MOINS VIRGULE POINT POINT_VIRGULE QUOTE
%token PARGAUCHE PARDROITE CROGAUCHE CRODROIT
%token TOKEN_INCLUDE TOKEN_INPUT_CLAUSE EGAL
%token FIN

%start donnees
%type  <Abs_syntax.abstract_syntax list> donnees

%start term_line
%type <Terms.term> term_line

%left TERMLIST
%nonassoc PARDROITE

%left PLUS MINUS
%left MULT
%left EXP

%%

donnees:
  FIN                    { [] }
| declaration donnees    { $1::$2 }
;

declaration:
  keyword_include PARGAUCHE f_name PARDROITE POINT
      { Abstract_include $3 }
| keyword_input_clause PARGAUCHE ident VIRGULE ident VIRGULE clause PARDROITE POINT
      { Abstract_clause $7 }
;
keyword_include:
  TOKEN_INCLUDE { etat_analyse.lexer_state <- Include_lexer }
;
keyword_input_clause:
  TOKEN_INPUT_CLAUSE { etat_analyse.lexer_state <- Input_clause_lexer }
;
f_name:
  FILE_NAME { $1 }
;
clause:
  CROGAUCHE literal_list CRODROIT { transform_clause $2 }
;
literal_list:
  literal { $1::[] }
| literal VIRGULE literal_list { $1::$3 }
;
literal:
  PLUS atom { P($2) }
| MOINS atom { M($2) }
;
atom:
  EGAL PARGAUCHE term VIRGULE term PARDROITE { ($3,$5) }
;
ident:
  IDENT               { $1 }
;
term :
  VRB { add_operator_to_cime_pb VARIABLE 0 DEFAULT $1;
        Var (var_id_of_string $1) }
| IDENT { add_operator_to_cime_pb FREE 0 DEFAULT $1;
          Term ((get_symbol_id $1),[]) }
| PARGAUCHE term PARDROITE { $2 }
| IDENT PARGAUCHE term_list PARDROITE
     {  let ar_f = (List.length $3) in
      	add_operator_to_cime_pb FREE ar_f DEFAULT $1;
      	let f=(get_symbol_id $1) in
        Term(f,$3)
    }
;
term_list:
  term %prec TERMLIST { [$1] }
| term VIRGULE term_list { $1 :: $3 }
;
term_line:
  term POINT_VIRGULE { $1 }
;


