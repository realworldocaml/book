/***************************************************************************

parser for generic polynomials and polynomial interpretations

$Id: genpoly_parser.mly,v 1.5 2002/05/27 12:07:35 contejea Exp $

***************************************************************************/

%{

  open Signatures;;
  open Generic_polynomials;;
  open Genpoly_syntax;;
  open Non_linear_solving;;
  open Finite_domains;;
  open Poly_interp;;

  exception Syntax_error of string

  let gen_var s =
    try
      let n = Listutils.index s !current_poly_vars
      in GenericPolynomials.var n
    with
	Not_found ->
          GenericPolynomials.cte
	    (Fd_polynomials.var (fd_var_id_of_string s))
;;

  let var s =
    try
      let n = Listutils.index s !current_poly_vars
      in IntPolynomials.var n
    with
	Not_found -> raise (Syntax_error ("undefined variable "^s))
;;

%}


%token <string> VAR
%token <User_signatures.symbol_id Marked_dp_criteria.dupl> INTERP
%token PARGAUCHE PARDROITE SEMICOLON EQUAL COMMA EOF
%token PLUS MINUS EXP MULT
%token <Num.num> INT

%start gen_poly_entry
%type  <Generic_polynomials.GenericPolynomials.poly> gen_poly_entry

%start poly_interp_entry
%type <(User_signatures.symbol_id Marked_dp_criteria.dupl, Poly_interp.IntPolynomials.poly) Signatures.SymbolMap.t > poly_interp_entry

%left PLUS MINUS
%left MULT
%nonassoc UMINUS
%right EXP

%%

gen_poly_entry:
  gen_poly EOF { $1 }
;


gen_poly:
  VAR { gen_var $1 }
| INT { GenericPolynomials.cte (Fd_polynomials.cte $1) }
| PARGAUCHE gen_poly PARDROITE { $2 }
| gen_poly PLUS gen_poly   { GenericPolynomials.add $1 $3 }
| gen_poly MINUS gen_poly  { GenericPolynomials.sub $1 $3 }
| MINUS gen_poly %prec UMINUS  { GenericPolynomials.minus $2 }
| gen_poly MULT gen_poly   { GenericPolynomials.mult $1 $3 }
| gen_poly EXP INT
    { try
	GenericPolynomials.power $1 (Num.int_of_num $3)
      with
	Failure("int_of_big_int") ->
	  failwith "Exponent too large"
    }
;

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
| INTERP PARGAUCHE vars     { current_poly_vars := $3; $1 }
;

vars:
  VAR PARDROITE            { [$1] }
| VAR COMMA vars           { $1::$3 }
;


poly:
  VAR                      { var $1 }
| INT                      { IntPolynomials.cte $1 }
| PARGAUCHE poly PARDROITE { $2 }
| poly PLUS poly           { IntPolynomials.add $1 $3 }
| poly MINUS poly          { IntPolynomials.sub $1 $3 }
| MINUS poly %prec UMINUS  { IntPolynomials.minus $2 }
| poly MULT poly           { IntPolynomials.mult $1 $3 }
| poly EXP INT
    { try
	IntPolynomials.power $1 (Num.int_of_num $3)
      with
	Failure("int_of_big_int") ->
	  failwith "Exponent too large"
    }
;
