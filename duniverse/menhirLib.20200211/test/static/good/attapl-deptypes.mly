/*
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
let rec addbinders tyT l = match l with
   [] -> tyT
 | (tyX,k)::rest -> tyT
%}   /* Removed TyAbs here. Dunno what this fn
does anyway. */

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> TYPE
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> PI
%token <Support.Error.info> PRF
%token <Support.Error.info> ALL
%token <Support.Error.info> PROP
%token <Support.Error.info> NORMAL
%token <Support.Error.info> SIGMA
%token <Support.Error.info> ONE
%token <Support.Error.info> TWO

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> ARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> Normal
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

%right COMMA

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context)
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).

*/

%start toplevel
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun ctx -> [],ctx }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds,ctx }

/* A top-level command */
Command :
    IMPORT STRINGV { fun ctx -> (Import($2.v)),ctx }
  | Term
      { fun ctx -> (let t = $1 ctx in Whred(tmInfo t,t)),ctx }
  | LPAREN NORMAL Term RPAREN
      { fun ctx -> (let t = $3 ctx in Eval(tmInfo t,t)),ctx }
  | UCID TyBinder
      { fun ctx -> ((Bind($1.i, $1.v, $2 ctx)), addname ctx $1.v) }
  | LCID Binder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }

/* Right-hand sides of top-level bindings */
Binder :
    COLON Type
      { fun ctx -> VarBind ($2 ctx)}
  | EQ Term
      { fun ctx -> TmAbbBind($2 ctx, None) }
  | EQ Term COLON Type
      {fun ctx -> TmAbbBind($2 ctx, Some($4 ctx)) }
/* All kind expressions */
Kind :
     PI  LCID COLON Type DOT Kind {fun ctx -> let ctx1 = addname ctx $2.v in
          KnPi($2.v,$4 ctx,$6 ctx1)}
  | ArrowKind {$1}

ArrowKind :
  AppType ARROW ArrowKind   { fun ctx -> KnPi("_",$1 ctx, $3 ctx) }
| AKind {$1 }

AKind :
    STAR { fun ctx -> KnStar }
  | LPAREN Kind RPAREN  { $2 }

/* All type expressions */
Type :
  | AppType { $1 }
  | AppType ARROW Type       { fun ctx ->
          let ctx1 = addname ctx "_" in
          TyPi("_",$1 ctx,$3 ctx1) }
  | PI LCID COLON Type DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyPi($2.v,$4 ctx,$6 ctx1) }
  | SIGMA LCID COLON Type DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TySigma($2.v,$4 ctx,$6 ctx1) }

AppType :
    AppType ATerm { fun ctx -> TyApp($1 ctx,$2 ctx) }
  | AType { $1 }

/* Atomic types are those that never need extra parentheses */
AType :
    LPAREN Type RPAREN
           { $2 }
  | PROP {fun ctx -> TyProp}
  | PRF LPAREN Term RPAREN {fun ctx -> TyPrf($3 ctx)}
  | UCID
      { fun ctx ->
          if isnamebound ctx $1.v then
            TyVar(name2index $1.i ctx $1.v, ctxlength ctx)
          else
            TyId($1.v) }


Term :
  | AppTerm
      { $1 }
  | LAMBDA LCID COLON Type DOT Term
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | ALL LCID COLON Type DOT Term
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAll($1, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA USCORE COLON Type DOT Term
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx, $6 ctx1) }

PathTerm :
  | PathTerm DOT TWO
      { fun ctx ->
          TmProj2($2, $1 ctx) }
  | PathTerm DOT ONE
      { fun ctx ->
          TmProj1($2, $1 ctx) }
  | ATerm { $1 }

AppTerm :
    PathTerm {$1}
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN
      { $2 }
  | LPAREN Term COMMA TermList COLON Type RPAREN { fun ctx ->
          TmList($1,$2 ctx :: $4 ctx ,$6 ctx)}
  | LCID
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }

TermList :
   Term { fun ctx -> [$1 ctx] }
 | Term COMMA TermList { fun ctx -> $1 ctx :: $3 ctx}


TyBinder :
  | COLON Kind
      { fun ctx -> TyVarBind($2 ctx) }
  | EQ Type
      { fun ctx -> TyAbbBind($2 ctx, None) }


/*   */
