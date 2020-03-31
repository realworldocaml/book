(* Original file: kremlin.0.9.6.0/kremlin-0.9.6.0/parser/parser.mly *)
%{
  open Bundle
%}

%token<int>     INT
%token<string>  UIDENT LIDENT
%token          PLUS MINUS STAR AT DOT EOF COMMA EQUALS PUBLIC LPAREN RPAREN

%start <(Flags.flag * (int * int)) list> warn_error_list
%start <Bundle.t> bundle
%start <Bundle.pat list> drop
%start <Ast.lident> lid

(** Parsing of command-line error/warning/silent flags. *)

%%

warn_error_list:
| ws = warn_error+ EOF
  { ws }

warn_error:
| f = flag r = range
  { f, r }

flag:
| AT
  { Flags.CError }
| MINUS
  { Flags.CSilent }
| PLUS
  { Flags.CWarning }

range:
| i = INT
  { i, i }
| i = INT DOT DOT j = INT
  { i, j }


(** Parsing of -bundle options *)

pat:
| STAR
  { Prefix [ ] }
| u = UIDENT
  { Module [ u ] }
| u = UIDENT DOT p = pat
  { match p with
    | Module m ->
        Module (u :: m)
    | Prefix m ->
        Prefix (u :: m) }

%inline
uident:
| u = UIDENT
  { u }

%inline
lident:
| l = LIDENT
  { l }

mident:
| l = separated_list(DOT, uident)
  { l }

api:
| m = mident
  { m, AsIs }
| PUBLIC LPAREN m = mident RPAREN
  { m, Public }

drop:
| p = separated_list(COMMA, pat) EOF
  { p }

bundle:
| apis = separated_nonempty_list(PLUS, api)
  EQUALS
  l = separated_nonempty_list(COMMA, pat) EOF
  { apis, l }
| l = separated_nonempty_list(COMMA, pat) EOF
  { [], l }

lid:
| l = lident
  { [], l }
| m = UIDENT DOT l = lid
  { let m', l = l in m :: m', l }
