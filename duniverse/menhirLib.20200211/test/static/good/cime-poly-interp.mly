/***************************************************************************

parser for polynomial interpretations

$Id: poly_interp_parser.mly,v 1.3 2003/07/11 15:42:48 contejea Exp $

***************************************************************************/

%{

  open Signatures;;
  open Poly_interp;;

  exception Syntax_error of string

  let var s =
    try
      let n = Listutils.index s !current_poly_vars
      in IntPolynomials.var n
    with
	Not_found -> raise (Syntax_error ("undefined variable "^s))
;;

%}


%token <string> VAR
%token <User_signatures.symbol_id> INTERP
%token LEFT_BRA RIGHT_BRA
%token LEFT_PAR RIGHT_PAR SEMICOLON EQUAL COMMA EOF
%token PLUS MINUS EXP MULT
%token <Numbers.t> INT

%start poly_interp_entry
%type <(User_signatures.symbol_id, Poly_interp.IntPolynomials.poly) Signatures.SymbolMap.t > poly_interp_entry

%start weight_entry
%type <(User_signatures.symbol_id, int) Signatures.SymbolMap.t > weight_entry

%left PLUS MINUS
%left MULT
%nonassoc UMINUS
%right EXP

%%

poly_interp_entry :
  interp EOF { $1 }
;

interp :
  /* epsilon */            { SymbolMap.empty }
| symbol_interp EQUAL poly SEMICOLON interp
                           { SymbolMap.add $1 $3 $5 }
;

symbol_interp:
  INTERP                    { current_poly_vars := []; $1 }
| INTERP LEFT_PAR vars     { current_poly_vars := $3; $1 }
;

vars:
  VAR RIGHT_PAR            { [$1] }
| VAR COMMA vars           { $1::$3 }
;


poly:
  VAR                      { var $1 }
| INT                      { IntPolynomials.cte $1 }
| LEFT_PAR poly RIGHT_PAR  { $2 }
| poly PLUS poly           { IntPolynomials.add $1 $3 }
| poly MINUS poly          { IntPolynomials.sub $1 $3 }
| MINUS poly %prec UMINUS  { IntPolynomials.minus $2 }
| poly MULT poly           { IntPolynomials.mult $1 $3 }
| poly EXP INT
    { try
	IntPolynomials.power $1 (Numbers.to_int $3)
      with
	Failure("int_of_big_int") ->
	  failwith "Exponent too large"
    }
;


weight_entry:
  weight EOF { $1 }
;

weight:
   /* epsilon */             { SymbolMap.empty }
| INTERP EQUAL INT SEMICOLON weight { SymbolMap.add $1 (Numbers.to_int $3) $5 }




