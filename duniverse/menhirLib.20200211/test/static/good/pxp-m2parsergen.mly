/* $Id: parser.mly,v 1.4 2000/05/09 00:03:22 gerd Exp $
 * ----------------------------------------------------------------------
 *
 */

%{
  open Ast

%}

%token Space
%token Token
%token Type
%token <string> Lname
%token <string> Uname
%token Separator
%token Lparen
%token Rparen
%token Comma
%token Colon
%token <string * int * int> Code
%token Error
%token Alt
%token Loop_plus
%token Loop_star
%token Dollar
%token Lbracket
%token Rbracket%token Eof

%start text
%type <Ast.text> text

%%

text:
  declarations rules
    { { text_decls = $1; text_rules = $2; } }

declarations:
  declaration declarations
    { $1 :: $2 }
| Separator
    { [] }

declaration:
  Token Uname
    { D_token $2 }
| Token Type Uname
    { D_typed_token $3 }

rules:
  rule rules
    { $1 :: $2 }
| Separator
    { [] }

rule:
  Lname Lparen formal_arguments Colon branches
    { { rule_name = $1;
	rule_arguments = $3;
	rule_branches = $5;
      }
    }

formal_arguments:
  Rparen
    { [] }
| Lname comma_formal_arguments
    { $1 :: $2 }

comma_formal_arguments:
  Comma Lname comma_formal_arguments
    { $2 :: $3 }
| Rparen
    { [] }

branches:
  branch alt_branches
    { $1 :: $2 }

alt_branches:
  Alt branch alt_branches
    { $2 :: $3 }
|
    { [] }

branch:
  simple_branch
    { $1 }
| Dollar Code simple_branch
    { { $3 with branch_early_code = $2 } }

simple_branch:
  symbol Dollar Code patterns Code opt_error_handler
    { { branch_selector = $1;
        branch_early_code = ("",0,0);
	branch_binding_code = $3;
	branch_pattern = $4;
	branch_result_code = $5;
	branch_error_code = $6;
      }
    }
| symbol patterns Code opt_error_handler
    { { branch_selector = $1;
        branch_early_code = ("",0,0);
	branch_binding_code = ("", 0, 0);
	branch_pattern = $2;
	branch_result_code = $3;
	branch_error_code = $4;
      }
    }

patterns:
  pattern patterns
    { $1 :: $2 }
|
    { [] }

pattern:
  symbol Loop_star
    { { pat_symbol = $1;
	pat_modifier = Repetition;
      }
    }
| symbol Error
    { { pat_symbol = $1;
	pat_modifier = Option;
      }
    }
| symbol
    { { pat_symbol = $1;
	pat_modifier = Exact;
      }
    }

symbol:
  Lname Colon Uname
    { U_symbol($3, Some $1) }
| Lname Colon Lname Lparen actual_arguments
    { L_symbol($3, $5, Some $1) }
| Lname Colon Lbracket Lname Rbracket Lparen actual_arguments
    { L_indirect($4, $7, Some $1) }
| Uname
    { U_symbol($1, None) }
| Lname Lparen actual_arguments
    { L_symbol($1, $3, None) }
| Lbracket Lname Rbracket Lparen actual_arguments
    { L_indirect($2, $5, None) }


actual_arguments:
  Rparen
    { [] }
| Lname comma_actual_arguments
    { $1 :: $2 }

comma_actual_arguments:
  Rparen
    { [] }
| Comma Lname comma_actual_arguments
    { $2 :: $3 }

opt_error_handler:
  Error Code
    { Some $2 }
|
    { None }

%%

(* ======================================================================
 * History:
 *
 * $Log: parser.mly,v $
 * Revision 1.4  2000/05/09 00:03:22  gerd
 * 	Added [ ml_name ] symbols, where ml_name is an arbitrary
 * OCaml identifier.
 *
 * Revision 1.3  2000/05/08 22:03:01  gerd
 * 	It is now possible to have a $ {{ }} sequence right BEFORE
 * the first token. This code is executed just after the first token
 * has been recognized.
 *
 * Revision 1.2  2000/05/06 21:51:46  gerd
 * 	New Dollar tag.
 *
 * Revision 1.1  2000/05/06 17:36:17  gerd
 * 	Initial revision.
 *
 *
 *)
