/***************************************************************************

parser for user signatures

$Id: signature_parser.mly,v 1.2 2001/04/20 13:42:28 marche Exp $

***************************************************************************/

%{

  open Symbols
  open C_declare_operator

%}


%token <string> IDENT INT
%token COMMA COLON SEMICOLON
%token KW_PREFIX KW_INFIX KW_POSTFIX
%token KW_C KW_AC KW_CONSTANT KW_UNARY KW_BINARY
%token EOF

%start signature
%type <(string list * int * Symbols.fix_type * C_declare_operator.symbol_type) list> signature


%%

signature:
  EOF                       { [] }
| decl SEMICOLON signature  { $1::$3 }
;


decl:
  op_list COLON fix arity
  { let t,a = $4
    in
      if $3=INFIX & a<>2
      then Errors.semantical_error "Infix symbols must be binary"
      else ($1,a,$3,t)
  }
/*
| op_list_colon fix arity AS profile_list
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
*/
;

/*
profile_list:
    profile {[$1]}
|   profile COMMA profile_list {$1::$3}
;

profile:
  base_sorte {[$1]}
| base_sorte ARROW profile {$1::$3}
;
*/

fix:
  KW_PREFIX     { PREFIX }
| KW_INFIX      { INFIX  }
| KW_POSTFIX    { POSTFIX }
| /* epsilon */ { DEFAULT }
;
arity:
  KW_C         { (C,2) }
| KW_AC        { (AC,2) }
| KW_CONSTANT  { (FREE,0) }
| KW_UNARY     { (FREE,1) }
| KW_BINARY    { (FREE,2) }
| INT          { (FREE,int_of_string $1) }
;
op_list:
  ident                { [$1] }
| ident COMMA op_list  { $1::$3 }
;
ident:
  IDENT { $1 }
| INT   { $1 }

