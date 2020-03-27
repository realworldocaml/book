%{
  open Prolog
%}

%token LPARENT RPARENT
%token DOT COMMA INFERS EOF
%token <string> IDENTIFIER VARIABLE

%start <Prolog.clause list> clauses
%%
clauses: clauses = separated_list( DOT, clause ) EOF { clauses }

clause:
    tm = term INFERS ts = separated_list( COMMA, term ) { (tm, ts) }
  | fact = term  { (fact, []) }

term:
  name = IDENTIFIER p = option(params)  {
    let ts = match p with None -> [] | Some l -> l in
      Struct (name, ts)
  }
  | v = VARIABLE { Var (0, v) }

params:  terms = delimited( LPARENT, separated_list( COMMA, term ) ,
RPARENT) { terms }
