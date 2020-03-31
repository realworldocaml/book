/***************************************************************************

parser for user signatures

$Id: term_parser.mly,v 1.2 2001/04/20 13:42:28 marche Exp $

***************************************************************************/

%{

  open C_declare_operator
  open Symbols
  open Terms
  open Errors
  open Equations

%}


%token <string> PREFIX_IDENT POSTFIX_IDENT INFIX_IDENT
%token COMMA SEMICOLON OPENPAR CLOSEPAR ARROW
%token EOF

%left INFIX_IDENT TERMLIST
%nonassoc CLOSEPAR

%start term_eof
%type <Terms.term> term_eof

%start rule_set_eof
%type <Equations.regle list> rule_set_eof


%%

term_eof:
  term EOF { $1 }
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
| OPENPAR term CLOSEPAR
     { $2
     }
| PREFIX_IDENT OPENPAR term_list CLOSEPAR
     { (* printf "forme f(...)\n"; *)
        let f=(get_symbol_id $1) in
        if (arity f)=(List.length $3)
        then TERM(f,$3)
        else semantical_error ("Bad number of arguments for " ^ $1)
    }
| OPENPAR term CLOSEPAR POSTFIX_IDENT
    { (* printf "forme f(...)\n"; *)
      let f=(get_symbol_id $4) in
        if (arity f)=1
          then TERM(f,[$2])
          else semantical_error ("Bad number of arguments for " ^ $4)
    }
| OPENPAR term_list CLOSEPAR POSTFIX_IDENT
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
| term COMMA term_list { $1 :: $3 }
;

rule_set_eof:
  rule_set EOF   { $1 }
;

rule_set:
  rule                    { [$1]   }
| rule SEMICOLON rule_set { $1::$3 }
;
rule:
  term ARROW term         { make_basic_rule ((flatten_term $1),
                                             (flatten_term $3)) }

