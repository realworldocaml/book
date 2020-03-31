%{

  open Syntax

%}

%token TOKEN TYPE LEFT RIGHT NONASSOC START PREC COLON BAR EOF
%token <string> LID UID
%token <Syntax.stretch> HEADER OCAMLTYPE
%token <Syntax.offset> PERCENTPERCENT
%token <Syntax.action> ACTION

%start grammar
%type <Syntax.grammar> grammar

%{

  let error i msg =
    Error.error2 (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i) msg

%}

%%

grammar:
  declarations PERCENTPERCENT rules trailer
    { List.rev $1, $3, $4 }

trailer:
  EOF
    { None }
| PERCENTPERCENT /* followed by actual trailer */
    { Some $1 }

declarations:
  /* epsilon */
    { [] }
| declarations declaration
    { $2 @ $1 }

declaration:
| HEADER /* lexically delimited by %{ ... %} */
    { [ DCode $1 ] }
| TOKEN terminals
    { List.map (fun x -> DToken (None, x)) $2 }
| TOKEN OCAMLTYPE /* lexically delimited by angle brackets */ terminals
    { List.map (fun x -> DToken (Some $2, x)) $3 }
| START nonterminals
    { List.map (fun x -> DStart x) $2 }
| TYPE OCAMLTYPE symbols
    { [] } /* TEMPORARY */
| LEFT symbols
    { [] } /* TEMPORARY */
| RIGHT symbols
    { [] } /* TEMPORARY */
| NONASSOC symbols
    { [] } /* TEMPORARY */

symbols:
  /* epsilon */
    { [] }
| symbols symbol
    { $2 :: $1 }

/* One would like to require nonterminal symbols to begin with a lowercase
   letter, so as to lexically distinguish them for terminal symbols, which
   must begin with an uppercase letter. However, for compatibility with
   ocamlyacc, this is impossible. It can be required only for nonterminal
   symbols that are also start symbols. */

symbol:
  LID
    { $1 }
| UID
    { $1 }

terminals:
  /* epsilon */
    { [] }
| terminals UID
    { $2 :: $1 }
| terminals LID
    { error 2 "Terminal symbols must begin with an uppercase letter." }

nonterminals:
  /* epsilon */
    { [] }
| nonterminals LID
    { $2 :: $1 }
| nonterminals UID
    { error 2 "Nonterminal start symbols must begin with a lowercase letter." }

rules:
  /* epsilon */
    { [] }
| rules rule
    { $2 :: $1 }

rule:
  symbol COLON optional_bar production productions
    { $1, $4 :: $5 }

optional_bar:
  /* epsilon */
    { () }
| BAR
    { () }

productions:
  /* epsilon */
    { [] }
| productions BAR production
    { $3 :: $1 }

production:
  symbols precedence ACTION /* lexically delimited by braces */
  /* TEMPORARY any symbol can be ERROR here */
    { List.rev $1, $3, $2 }

precedence:
  /* epsilon */
    { None }
| PREC symbol
    { Some $2 }
