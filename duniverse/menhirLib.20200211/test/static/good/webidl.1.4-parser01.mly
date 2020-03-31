%{
    (* Web IDL parser
     * The below rules are based on Editor’s Draft, 1 June 2017
     * https://heycam.github.io/webidl/#idl-grammar 
    *)
    open Ast
    open Keyword
%}

%start ext_main
%type < Ast.extended_attribute > ext_main

%%

ext_main :
    | extendedAttribute EOF { $1 }

%public extendedAttributeList :
    | LBRACKET extendedAttribute extendedAttributes RBRACKET { ($2 :: $3) }
    |  { [] }

extendedAttributes :
    | COMMA extendedAttribute extendedAttributes { $2 :: $3 }
    |  { [] }

extendedAttribute :
    | extendedAttributeNoArgs { $1 }
    | extendedAttributeArgList { $1 }
    | extendedAttributeIdent { $1 }
    | extendedAttributeIdentList { $1 }
    | extendedAttributeNamedArgList { $1 }

identifierList :
    | IDENTIFIER identifiers { $1 :: $2 }

extendedAttributeNoArgs :
    | IDENTIFIER { `NoArgs $1 }

extendedAttributeArgList :
    | IDENTIFIER LPAR argumentList RPAR { `ArgumentList($1, $3) }

extendedAttributeIdent :
    | IDENTIFIER EQUAL IDENTIFIER { `Ident($1, $3) }

extendedAttributeIdentList :
    | IDENTIFIER EQUAL LPAR identifierList RPAR { `IdentList($1, $4) }

extendedAttributeNamedArgList :
    | IDENTIFIER EQUAL IDENTIFIER LPAR argumentList RPAR 
    { `NamedArgList($1, $3, $5) }