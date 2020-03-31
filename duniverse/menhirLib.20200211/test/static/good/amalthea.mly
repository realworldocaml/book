%{
  open Io_types ;;
  open Io_messages ;;
  open Io_parser_state ;;
  let unpos = snd
  let pos = fst
  let ct1 = fun c (cs, bs) -> (c::cs, bs)
  let ct2 = fun b (cs, bs) -> (cs, b::bs)
  let parse_error = function msg ->
    raise (Io_module_error (Io_parser_error !last_pos))

%}

%token <Io_types.pos * int> Lint
%token <Io_types.pos * string> Lstring
%token <Io_types.pos * string> Lident
%token Llpar Lrpar Lsemi
%token Lcolon Lperiod
%token <Io_types.pos> Llambda
%token <Io_types.pos> Lputvar
%token <Io_types.pos> Lgetvar
%token Lend
%token <Io_types.pos> Ldeclare
%token <Io_types.pos> Lprimitive
%token <Io_types.pos> Lvariable
%token <Io_types.pos> Lexport
%token <Io_types.pos> Limport

%start parse_io_module
%type <Io_types.io_ast> parse_io_module

%%

parse_io_module:
  | imports export decll expr Lend             { ($1,$2,$3,$4) }

imports:
  | Limport Lident importlist Lperiod imports  { last_pos := pos $2;(String.lowercase (unpos $2), $3)::$5 }
  |                                            { [] }

importlist:
  | Lcolon idlist                              { Some $2 }
  |                                            { None }

export:
  | Lexport idlist Lperiod                     { $2 }
  |                                            { [] }

decll:
  | Ldeclare Lident Lcolon eatom Lperiod decll    { last_pos := pos $2;ct1 ($1,unpos $2,$4) $6 }
  | Lprimitive Lident Lcolon Lident Lperiod decll { last_pos := pos $2;ct1 ($1,unpos $2,Eprimitive (unpos $4)) $6 }
  | Lvariable Lident Lcolon eatom Lperiod decll   { last_pos := pos $2;ct2 ($1,unpos $2,$4) $6 }
  |                                               { ([], []) }

expr:
  | Lident paramlist                           { Eappl (pos $1, unpos $1, $2, EFall) }
  | Lident                                     { Eid (pos $1, unpos $1) }
  | Lident Lputvar patom stmttail              { Eputvar (pos $1, unpos $1, $3, $4, EFall) }
  | Lident Lgetvar Lident stmttail             { Egetvar (pos $1, unpos $1, unpos $3, $4, EFall) }
  | stmt                                       { $1 }

paramlist:
  | patom paramlisttail                        { $1::$2 }
  | Lsemi eatom                                { [$2]   }
  | stmt                                       { [$1] }

paramlisttail:
  | patom paramlisttail                        { $1::$2 }
  | Lsemi eatom                                { [$2]   }
  | stmt                                       { [$1] }
  |                                            { [] }

stmt:
  | Llambda idlist stmttail                    { Elambda ($1,$2,$3,EFall) }

stmttail:
  | Lsemi eatom                                { $2 }
  | atom                                       { $1 }

idlist:
  | Lident idlist                              { last_pos := pos $1;(unpos $1)::$2 }
  |                                            { [] }

eatom:
  | expr                                       { $1 }
  | atom                                       { $1 }

patom:
  | Lident                                     { last_pos := pos $1; Eid (pos $1,unpos $1) }
  | atom                                       { $1 }

atom:
  | Lint                                       { last_pos := pos $1; Eint (pos $1,unpos $1) }
  | Lstring                                    { last_pos := pos $1; Estring (pos $1,unpos $1) }
  | Llpar expr Lrpar                           { $2 }
