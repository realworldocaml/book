(* Original file: datalog.0.5.2/datalog-0.5.2/src/topDownParser.mly *)
/*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

%{
  let remove_quotes s =
    let n = String.length s in
    if (s.[0] = '\'' && s.[n-1] = '\'') ||
       (s.[0] = '"' && s.[n-1] = '"')
      then String.sub s 1 (n-2)
      else s
%}

%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token DOT
%token COLON
%token IF
%token NOT
%token COMMA
%token AGGR_EQUAL
%token EOI
%token <string> SINGLE_QUOTED
%token <string> DOUBLE_QUOTED
%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> INT
%token <string> OPERATOR  /* infix operator */

%start parse_term
%type <TopDownAst.term> parse_term

%start parse_literal
%type <TopDownAst.literal> parse_literal

%start parse_literals
%type <TopDownAst.literal list> parse_literals

%start parse_query
%type <TopDownAst.term list * TopDownAst.literal list> parse_query

%start parse_clause
%type <TopDownAst.clause> parse_clause

%start parse_file
%type <TopDownAst.file> parse_file

%%

parse_file:
  | clauses EOI { $1 }

parse_term:
  | term EOI { $1 }

parse_literal:
  | literal EOI { $1 }

parse_literals:
  | literals EOI { $1 }

parse_query:
  | tuple IF literals { $1, $3 }

parse_clause:
  | clause EOI { $1 }

clauses:
  | clause { [$1] }
  | clause clauses { $1 :: $2 }

clause:
  | term DOT { ($1, []) }
  | term IF literals DOT { ($1, $3) }

literals:
  | literal { [$1] }
  | literal COMMA literals { $1 :: $3 }

literal:
  | atom { TopDownAst.LitPos $1 }
  | NOT atom { TopDownAst.LitNeg $2 }
  | subterm AGGR_EQUAL LOWER_WORD UPPER_WORD COLON term
    { TopDownAst.(LitAggr
      { ag_left=$1; ag_constructor= $3; ag_var= $4; ag_guard= $6}
      )
    }

atom:
  | term { $1 }
  | subterm OPERATOR subterm { TopDownAst.Apply($2, [$1; $3]) }

term:
  | LOWER_WORD { TopDownAst.Apply ($1, []) }
  | SINGLE_QUOTED { TopDownAst.Apply (remove_quotes $1, []) }
  | DOUBLE_QUOTED { TopDownAst.Apply (remove_quotes $1, []) }
  | LOWER_WORD LEFT_PARENTHESIS args RIGHT_PARENTHESIS
    { TopDownAst.Apply ($1, $3) }

subterm:
  | term { $1 }
  | UPPER_WORD { TopDownAst.Var $1 }
  | INT { TopDownAst.Int( int_of_string $1) }

args:
  | subterm { [$1] }
  | subterm COMMA args  { $1 :: $3 }

tuple:
  | LEFT_PARENTHESIS args RIGHT_PARENTHESIS { $2 }
