/*  A modular module system.
    The parser for mini-ML.

    Copyright 1999 Xavier Leroy.
    This file is distributed under the GNU Public Licence. */

%{

open Modules
open MiniML
open MLTyping

let variables = ref ([] : (string * ML.type_variable) list)

let reset_type_variables () =
  variables := []

let find_type_variable name =
  try
    List.assoc name !variables
  with Not_found ->
    let v = newvar() in
    variables := (name, v) :: !variables;
    v

let binop op arg1 arg2 =
  ML.Apply(ML.Apply(ML.Longident(Pident(Ident.create op)), arg1), arg2)
let ternop op arg1 arg2 arg3 =
  ML.Apply(ML.Apply(ML.Apply(ML.Longident(Pident(Ident.create op)), arg1), arg2), arg3)

%}

%token <string> IDENT
%token <int> INT

%token ARROW
%token COLON
%token COMMA
%token DOT
%token ELSE
%token END
%token EOF
%token EQUAL
%token EQUALEQUAL
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATEREQUAL
%token IF
%token IN
%token LESS
%token LESSEQUAL
%token LESSGREATER
%token LET
%token LIDENT
%token LPAREN
%token MINUS
%token MODULE
%token PLUS
%token QUOTE
%token RPAREN
%token SEMICOLON
%token SEMISEMI
%token SIG
%token SLASH
%token STAR
%token STRUCT
%token THEN
%token TYPE
%token VALUE

%right ARROW
%right COMMA
%right LESSGREATER LESS LESSEQUAL GREATER GREATEREQUAL
%right PLUS MINUS
%right STAR SLASH

%start implementation
%type <MiniML.MLMod.mod_term> implementation
%start phrase
%type <MiniML.MLMod.definition> phrase

%%

/* Paths */

path:
    IDENT           { Pident(Ident.create $1) }
  | path DOT IDENT  { Pdot($1, $3) }
;

/* Value expressions for the core language */

valexpr:
    valexpr1                          { $1 }
  | valexpr COMMA valexpr             { binop "," $1 $3 }
  | valexpr PLUS valexpr              { binop "+" $1 $3 }
  | valexpr MINUS valexpr             { binop "-" $1 $3 }
  | valexpr STAR valexpr              { binop "*" $1 $3 }
  | valexpr SLASH valexpr             { binop "/" $1 $3 }
  | valexpr EQUALEQUAL valexpr        { binop "==" $1 $3 }
  | valexpr LESSGREATER valexpr       { binop "<>" $1 $3 }
  | valexpr LESS valexpr              { binop "<" $1 $3 }
  | valexpr LESSEQUAL valexpr         { binop "<=" $1 $3 }
  | valexpr GREATER valexpr           { binop ">" $1 $3 }
  | valexpr GREATEREQUAL valexpr      { binop ">=" $1 $3 }
  | FUNCTION IDENT ARROW valexpr      { ML.Function(Ident.create $2, $4) }
  | LET IDENT valbind IN valexpr      { ML.Let(Ident.create $2, $3, $5) }
  | IF valexpr THEN valexpr ELSE valexpr { ternop "conditional" $2 $4 $6 }
;
valexpr1:
    valexpr0 { $1 }
  | valexpr1 valexpr0 { ML.Apply($1, $2) }
;
valexpr0:
    path { ML.Longident($1) }
  | INT  { ML.Constant $1 }
  | LPAREN valexpr RPAREN { $2 }
;
valbind:
    EQUAL valexpr     { $2 }
  | IDENT valbind     { ML.Function(Ident.create $1, $2) }
;

/* Type expressions for the core language */

simpletype:
    QUOTE IDENT             { ML.Var(find_type_variable $2) }
  | simpletype ARROW simpletype { ML.Typeconstr(path_arrow, [$1; $3]) }
  | simpletype STAR simpletype  { ML.Typeconstr(path_star, [$1; $3]) }
  | path                    { ML.Typeconstr($1, []) }
  | simpletype path         { ML.Typeconstr($2, [$1]) }
  | LPAREN simpletypelist RPAREN path { ML.Typeconstr($4, List.rev $2) }
;
simpletypelist:
    simpletype { [$1] }
  | simpletypelist COMMA simpletype { $3::$1 }
;

valuedecl:
    colon_begin_scheme simpletype
            { reset_type_variables(); end_def(); generalize $2 }
;
colon_begin_scheme: /* Hack to perform side effects before reading the type */
    COLON   { begin_def(); reset_type_variables() }
;

/* Type definitions and declarations */

typedecl:
    typeparams IDENT        { ($2, {ML.arity = List.length $1}) }
;
typedef:
    typeparams IDENT EQUAL simpletype
      { reset_type_variables();
        ($2, {ML.arity = List.length $1}, {ML.params = $1; ML.defbody = $4}) }
;
typeparams:
    /* nothing */               { [] }
  | typeparam                   { [$1] }
  | LPAREN typeparamlist RPAREN { List.rev $2 }
;
typeparamlist:
    typeparam                       { [$1] }
  | typeparamlist COMMA typeparam   { $3 :: $1 }
;
typeparam:
    QUOTE IDENT { find_type_variable $2 }
;
typeinfo:
    typedef   { let (id, kind, def) = $1 in
                (id, {MLMod.kind = kind; MLMod.manifest = Some def})}
  | typedecl  { let (id, kind) = $1 in
                (id, {MLMod.kind = kind; MLMod.manifest = None}) }
;

/* Value expressions for the module language */

modulexpr:
    path                              { MLMod.Longident $1 }
  | STRUCT structure END              { MLMod.Structure(List.rev $2) }
  | FUNCTOR LPAREN IDENT COLON moduletype RPAREN modulexpr
                                      { MLMod.Functor(Ident.create $3, $5, $7) }
  | modulexpr LPAREN modulexpr RPAREN { MLMod.Apply($1, $3) }
  | LPAREN modulexpr RPAREN           { $2 }
  | modulexpr COLON moduletype        { MLMod.Constraint($1, $3) }
;
structure:
    /*nothing*/                       { [] }
  | structure structure_item opt_semi { $2 :: $1 }
;
structure_item:
    VALUE IDENT valbind           { MLMod.Value_str(Ident.create $2, $3) }
  | TYPE typedef                  { let (id, kind, def) = $2 in
                                    MLMod.Type_str(Ident.create id, kind, def) }
  | MODULE IDENT COLON moduletype EQUAL modulexpr
                     { MLMod.Module_str(Ident.create $2, MLMod.Constraint($6, $4)) }
  | MODULE IDENT EQUAL modulexpr   { MLMod.Module_str(Ident.create $2, $4) }
;
opt_semi:
    /* nothing */ { () }
  | SEMICOLON { () }
;

/* Type expressions for the module language */

moduletype:
    SIG signature END               { MLMod.Signature(List.rev $2) }
  | FUNCTOR LPAREN IDENT COLON moduletype RPAREN moduletype
                                    { MLMod.Functor_type(Ident.create $3, $5, $7) }
  | LPAREN moduletype RPAREN        { $2 }
;
signature:
    /*nothing*/                       { [] }
  | signature signature_item opt_semi { $2 :: $1 }
;
signature_item:
    VALUE IDENT valuedecl             { MLMod.Value_sig(Ident.create $2, $3) }
  | TYPE typeinfo    { let (id, def) = $2 in MLMod.Type_sig(Ident.create id, def) }
  | MODULE IDENT COLON moduletype     { MLMod.Module_sig(Ident.create $2, $4) }
;

/* Toplevel entry point */

phrase:
    structure_item SEMISEMI           { $1 }
  | EOF                               { raise End_of_file }
;

/* Sep. comp. entry point */

implementation:
    modulexpr EOF                     { $1 }
;
