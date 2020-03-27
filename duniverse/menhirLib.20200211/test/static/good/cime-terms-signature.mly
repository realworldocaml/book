/***************************************************************************

parser for user signatures

CiME Project - Démons research team - LRI - Université Paris XI

$Id: signature_parser.mly,v 1.6 2001/04/20 13:42:31 marche Exp $

***************************************************************************/

%{

  open Signatures
  open Signature_syntax

%}


%token <string> IDENT INT
%token COMMA COLON SEMICOLON
%token KW_PREFIX KW_INFIX KW_POSTFIX
%token KW_C KW_AC KW_CONSTANT KW_UNARY KW_BINARY
%token EOF
%token AS ARROW

%start signature
%type <(string list * int * Signatures.symbol_fix * Signature_syntax.symbol_theory) list> signature


%start sorted_signature
%type <((string list * int * Signatures.symbol_fix * Signature_syntax.symbol_theory) * ((string list * string)list)) list > sorted_signature


%%

signature:
  EOF                       { [] }
| decl                      { [$1] }
| decl SEMICOLON signature  { $1::$3 }
;

sorted_signature:
  EOF                                     { [] }
| sorted_decl                             { [$1] }
| sorted_decl SEMICOLON sorted_signature  { $1::$3 }
;


decl:
  op_list COLON fix arity
  { let t,a = $4
    in
      if $3=Infix & a<>2
      then raise (Syntax_error "Infix symbols must be binary")
      else ($1,a,$3,t)
  }
;

sorted_decl:
 op_list COLON fix arity AS profile_list
    { let t,a = $4 in
        if $3=Infix & a<>2
          then raise (Syntax_error "Infix symbols must be binary")
          else
	    if (List.exists (fun (x,y) -> (List.length x)<>a) $6)
	    then raise (Syntax_error "Profile must be compatible with arity")
	    else
	      (($1,a,$3,t),$6)
	    }
;

profile_list:
    profile                    { [$1]   }
|   profile COMMA profile_list { $1::$3 }
;

profile:
  sort_list ARROW sort   { ($1,$3) }
;

sort_list:
  /* epsilon */         { [] }
| sort sort_list        { $1::$2 }
;
sort:
  IDENT                 { $1 }
;

fix:
  KW_PREFIX     { Prefix }
| KW_INFIX      { Infix  }
| KW_POSTFIX    { Postfix }
| /* epsilon */ { Default }
;
arity:
  KW_C         { (Commutative,2) }
| KW_AC        { (Ac,2) }
| KW_CONSTANT  { (Free,0) }
| KW_UNARY     { (Free,1) }
| KW_BINARY    { (Free,2) }
| INT          { (Free,int_of_string $1) }
;
op_list:
  ident                { [$1] }
| ident COMMA op_list  { $1::$3 }
;
ident:
  IDENT { $1 }
| INT   { $1 }
;
