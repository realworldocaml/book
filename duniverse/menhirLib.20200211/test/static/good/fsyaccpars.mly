%{
(* (c) Microsoft Corporation 2005-2006.  *)

open Fsyaccast

%}

%type <Fsyaccast.spec> spec
%token <string>  IDENT
%token <Fsyaccast.code> HEADER CODE
%token BAR PERCENT_PERCENT  START LEFT RIGHT NONASSOC LESS GREATER COLON PREC SEMI EOF ERROR
%token <string> TYPE
%token <string option> TOKEN
%start spec
%left BAR
%%

spec: headeropt decls PERCENT_PERCENT rules { List.fold_right (fun f x -> f x) $2 { header=$1;tokens=[];types=[];assoc=[];starts=[];rules=$4 } }
headeropt: HEADER { $1 } | { "", Parsing.symbol_start_pos () }
decls:  { [] } | decl decls { $1 :: $2 }
decl:
    TOKEN idents { (fun x -> {x with tokens = x.tokens @ (List.map (fun x -> (x,$1)) $2)}) }
  | TYPE idents   { (fun x -> {x with types = x.types @ (List.map (fun x -> (x,$1)) $2)} ) }
  | START idents   { (fun x -> {x with starts = x.starts @ $2} ) }
  | LEFT idents   { (fun x -> {x with assoc = x.assoc @ [(List.map (fun x -> (x,LeftAssoc)) $2)]} ) }
  | RIGHT idents   { (fun x -> {x with assoc = x.assoc @ [(List.map (fun x -> (x,RightAssoc)) $2)]} ) }
  | NONASSOC idents   { (fun x -> {x with assoc = x.assoc @ [(List.map (fun x -> (x,NonAssoc)) $2)]} ) }

idents: IDENT idents { $1 :: $2 } | { [] }
rules: rule rules { $1 :: $2 } | rule { [$1] }
rule: IDENT COLON optbar clauses optsemi { ($1,$4) }
optbar: { } | BAR { }
optsemi: { } | SEMI { }
clauses: clause BAR clauses {$1 :: $3 } | clause { [$1] }
clause: syms optprec CODE { Rule($1,$2,Some $3) }
syms: IDENT syms { $1 :: $2 } | ERROR syms { "error" :: $2 } | { [] }
optprec: { None } | PREC IDENT { Some $2 }


