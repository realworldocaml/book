%{

open Syntax.Raw

%}

%token <string> VAR
%token <int> CONST
%token EQUAL PLUS EOF FAIL

%left PLUS

%type <Syntax.Raw.declarations> declarations
%start declarations

%%

expr:
| VAR
    { EVar $1 }
| CONST
    { EConst $1 }
| expr PLUS expr
    { EAdd ($1, $3) }
| FAIL
    { EFail }

declarations:
| VAR EQUAL expr EOF
    { D ($1, $3, Suspension.create (fun () -> !ParserFix.declarations())) }

    /* We read just one declaration, of the form <variable> = <expression>,
       and expect to find no more input, because the token stream represents
       just one line of input and we expect one declaration per line.

       The rest of the declarations is represented by a suspension, which,
       when forced, will do whatever is necessary to read more input. The
       function that defines this suspension needs to call the parser, so
       there is a circularity. We break this circularity by going through
       a reference in module [ParserFix]. */

