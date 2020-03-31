%{
(* (c) Microsoft Corporation 2005-2006.  *)

open Fslexast

%}

%type <Fslexast.spec> spec
%token <string> STRING IDENT
%token <Fslexast.code> CODE
%token <char> CHAR
%token RULE PARSE LET  AND LPAREN RPAREN
%token EOF BAR DOT PLUS STAR QMARK EQUALS UNDERSCORE LBRACK RBRACK HAT DASH
%start spec
%left BAR
%left regexp_alt
%left regexp_seq
%nonassoc regexp_opt
%nonassoc regexp_plus regexp_star
%%

spec: codeopt macros RULE rules codeopt { { topcode=$1;macros=$2;rules=$4;botcode=$5 } }
codeopt: CODE { $1 } | { "", Parsing.symbol_start_pos () }
macros:  { [] } | macro macros { $1 :: $2 }
macro: LET IDENT EQUALS regexp { ($2, $4) }
rules: rule AND rules { $1 :: $3 } | rule { [$1] }
rule: IDENT args EQUALS PARSE optbar clauses { ($1,$2,$6) }
args: { [] } | IDENT args { $1 :: $2 }
optbar: { } | BAR { }
clauses: clause BAR clauses {$1 :: $3 } | clause { [$1] }
clause: regexp CODE { $1, $2 }
regexp:
  CHAR { Inp(LChar $1) }
| EOF { Inp(LEof) }
| UNDERSCORE { Alt(mapi (fun n -> Inp(LChar (Char.chr n))) 255) }
| STRING { Seq(mapi (fun n -> Inp(LChar (String.get $1 n))) (String.length $1 - 1)) }
| IDENT { Macro($1) }
| regexp regexp %prec regexp_seq  { Seq[$1;$2] }
| regexp PLUS %prec regexp_plus  { Seq[$1;Star $1] }
| regexp STAR %prec regexp_star  { Star $1 }
| regexp QMARK %prec regexp_opt  { Alt[Seq[];$1] }
| regexp BAR regexp %prec regexp_alt  { Alt[$1;$3] }
| LPAREN regexp RPAREN  { $2 }
| LBRACK charset RBRACK   { Alt (List.map (fun c -> Inp(LChar c)) $2) }
| LBRACK HAT charset RBRACK   { Alt(foldi(fun n l -> if List.mem (Char.chr n) $3 then l else Inp(LChar (Char.chr n))::l) 255 []) }

charset:
 | CHAR { [$1] }
 | CHAR DASH CHAR { mapi(fun n -> Char.chr (n+Char.code $1)) (Char.code $3 - Char.code $1) }
 | charset charset { $1 @ $2 }


