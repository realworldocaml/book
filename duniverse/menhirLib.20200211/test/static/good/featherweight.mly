/* Ghulam Mujtaba Lashari.
   Final Project CSC-535 Spring 2001.
   FeatherWeight Java
*/

/*
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

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
%token <Support.Error.info> CLASS
%token <Support.Error.info> NEW
%token <Support.Error.info> EXTENDS
%token <Support.Error.info> THIS
%token <Support.Error.info> SUPER
%token <Support.Error.info> RETURN
%token <Support.Error.info> COMMAND
/* End keyword tokens */


/* Identifier and constant value tokens */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV
%token <string Support.Error.withinfo> ID

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
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

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
   Syntax.command list.
*/
%start toplevel
%type < Syntax.program > toplevel
%%
/* ---------------------------------------------------------------------- */

/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */

toplevel :
	classDecs COLON termDef
	  { Program($1,$3) }

classDecs :
    COMMAND
      { [] }
  | classDec classDecs
      {  let decl = $1 in
          let decls = $2 in
	      decl::decls }

/* we are cheating on two places to make parser work,
   In Java File,
   1--you have to put a ";" after fields (if any)
   2--you have to put a ";" after method definitions (if any)
   Also you have to put a keyword "command" followed by a ":" after class
   definitions and then put a term to execute.  */

classDec :
    CLASS ID EXTENDS ID LCURLY fieldDefs constructorDef methodDefs RCURLY
      { Class (Name($2.v), SuperName($4.v), $6, $7, $8) }
  | CLASS ID EXTENDS ID LCURLY fieldDefs constructorDef RCURLY
      { Class (Name($2.v), SuperName($4.v), $6, $7, []) }
  | CLASS ID EXTENDS ID LCURLY constructorDef methodDefs RCURLY
      { Class (Name($2.v), SuperName($4.v), [], $6, $7) }
  | CLASS ID EXTENDS ID LCURLY constructorDef RCURLY
      { Class (Name($2.v), SuperName($4.v), [], $6, []) }

fieldDefs :
	SEMI
	  { [] }
  |	fieldDef SEMI fieldDefs
	  { let fDef = $1 in
		  let fDefs = $3 in
		  fDef::fDefs }

fieldDef :
	ID ID
	  { Field(Type($1.v), Name($2.v)) }

constructorDef :
	ID LPAREN paramDefs RPAREN LCURLY SUPER LPAREN argDefs RPAREN SEMI fieldAssignmentDefs RCURLY
	  { Constructor(Name($1.v), $3, SuperConstructorCall($8), $11) }

paramDefs :
    /* empty */
     { [] }
  | paramDef
      { let pDef = $1 in
          let pDefs = [] in
          pDef::pDefs }
  | paramDef COMMA paramDefs
      { let pDef = $1 in
          let pDefs = $3 in
          pDef::pDefs }

paramDef :
    ID ID
      { Param (Type($1.v), Name($2.v)) }

argDefs :
    /* empty */
      {	[] }
  | argDef
      {	let aDef = $1 in
	  let aDefs = [] in
	  aDef::aDefs }
  | argDef COMMA argDefs
      {	let aDef = $1 in
	  let aDefs = $3 in
	  aDef::aDefs }

argDef :
    ID
      { Arg($1.v) }

fieldAssignmentDefs :
   /* empty */
      { [] }
 | fieldAssignmentDef fieldAssignmentDefs
      { let faDef = $1 in
          let faDefs = $2 in
	  faDef::faDefs }

fieldAssignmentDef :
    THIS DOT ID EQ ID SEMI
      { FieldAssignment (Name($3.v), Value($5.v)) }


methodDefs :
     SEMI
      { [] }
  | methodDef methodDefs
      { let mDef = $1 in
          let mDefs = $2 in
	      mDef::mDefs }

methodDef :
    ID ID LPAREN paramDefs RPAREN LCURLY RETURN termDef SEMI RCURLY
	  { Method(ReturnType($1.v),Name($2.v),$4,$8) }

/* All terms except variable can be parenthesized or non-parenthesized. */
termDef :
    TmVarDef
      { $1 }
  | TmFieldAccessDef
      { $1 }
  | TmMethodInvocationDef
      { $1 }
  | TmObjectCreationDef
      { $1 }
  | TmCastDef
      { $1 }
  | TmThis
      { $1 }
  | LPAREN TmFieldAccessDef RPAREN
      { $2 }
  | LPAREN TmMethodInvocationDef RPAREN
      { $2 }
  | LPAREN TmObjectCreationDef RPAREN
      { $2 }
  | LPAREN TmCastDef RPAREN
      { $2 }

TmThis :
   THIS
     { TmVar("this") }

TmVarDef :
	ID
	  { TmVar($1.v) }

TmFieldAccessDef :
	termDef DOT ID
	  { TmFieldAccess($1,$3.v) }

TmMethodInvocationDef :
	termDef DOT ID LPAREN termDefs RPAREN
	  { TmMethodInvocation($1,$3.v,$5) }

TmObjectCreationDef :
	NEW ID LPAREN termDefs RPAREN
	  { TmObjectCreation($2.v,$4) }

TmCastDef :
	LPAREN ID RPAREN termDef
	  { TmCast($2.v,$4) }

termDefs :
    /* empty */
      { [] }
  | termDef
      { let tDef = $1 in
          let tDefs = [] in
          tDef::tDefs }
  | termDef COMMA termDefs
      { let tDef = $1 in
          let tDefs = $3 in
          tDef::tDefs }

/*   */





