/*
    $Id: rc_parse.mly,v 1.1 1999/08/09 17:10:00 lindig Exp $
*/

%{
open Rc_ast
%}

/* tokens */

%token <string> STR
%token <string> ID
%token <bool>   BOOL
%token <float>  FLOAT
%token <int>    INT

%token COMMA EQUAL TRUE FALSE EOF


%start                  rcfile
%type  <Rc_ast.rcdict>  rcfile

%%

rcfile          : rclines EOF                   { $1            }

rclines         : /**/                          { empty         }
                | rclines rcline                { let (id,rc) = $2 in
                                                  add id rc $1
                                                }

rcline          : ID EQUAL value                { ($1,$3)       }
                | ID EQUAL values               { ($1,RClist(List.rev $3))  }

value           : ID                            { RCstr($1)     }
                | STR                           { RCstr($1)     }
                | TRUE                          { RCbool(true)  }
                | FALSE                         { RCbool(false) }
                | INT                           { RCint($1)     }
                | FLOAT                         { RCfloat($1)   }

values          : value  COMMA value            { [$3  ; $1]    }
                | values COMMA value            {  $3 :: $1     }
