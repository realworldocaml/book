(* Original file: ccss.1.6/ccss-1.6/src/parser.mly *)
(********************************************************************************)
(*  Parser.mly
    Copyright (c) 2010-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

%{
let nelist = function
    | hd :: tl -> (hd, tl)
    | []       -> failwith "nelist"
%}


(********************************************************************************)
(* Token declarations.                                                          *)
(********************************************************************************)

%token EOF
%token S

%token <string option> CHARSET
%token <string option> IMPORT
%token <string option> MEDIA
%token <string option> PAGE
%token <string option> FONTFACE
%token <string option> KEYFRAMES

%token ONLY NOT AND

%token OPEN_CURLY CLOSE_CURLY
%token OPEN_ROUND CLOSE_ROUND
%token OPEN_SQUARE CLOSE_SQUARE
%token SEMICOLON COLON DOUBLE_COLON COMMA PERIOD SLASH
%token ASTERISK QUOTIENT PLUS MINUS
%token TILDE GT IMPORTANT

%token ATTR_EQUALS
%token ATTR_INCLUDES
%token ATTR_DASHMATCH
%token ATTR_PREFIX
%token ATTR_SUFFIX
%token ATTR_SUBSTRING

%token URI
%token <string> STRING
%token <string> IDENT
%token <string> NTH
%token <string> HASH
%token <string> URANGE
%token <string> VAR

%token <string> SEL_FUNC
%token <string> TERM_FUNC

%token <Ast.quantity> QUANTITY


(********************************************************************************)
(* Associativity and precedence declarations.                                   *)
(********************************************************************************)

%left PLUS MINUS
%left ASTERISK QUOTIENT


(********************************************************************************)
(* Top-level statements.                                                        *)
(********************************************************************************)

%type <Ast.t> stylesheet

%start stylesheet

%%

stylesheet:
    | S* statement* EOF                                         {$2}

statement:
    | atrule                                                    {`Atrule $1}
    | rule                                                      {`Rule $1}
    | VAR COLON var_decl                                        {`Vardecl (($startpos($1), $1), $3)}

atrule:
    | CHARSET STRING SEMICOLON                                  {($1, `Charset $2)}
    | IMPORT source S? media_query_list? SEMICOLON              {($1, `Import ($2, $4))}
    | MEDIA media_query_list OPEN_CURLY rule+ CLOSE_CURLY       {($1, `Media ($2, $4))}
    | PAGE pseudo_page? declaration_block                       {($1, `Page ($2, $3))}
    | FONTFACE declaration_block                                {($1, `Fontface $2)}
    | KEYFRAMES IDENT OPEN_CURLY keyframe_block+ CLOSE_CURLY    {($1, `Keyframes ($2, $4))}

rule:
    | selector_list declaration_block                           {($1, $2)}

var_decl:
    | expr SEMICOLON                                            {`Expr $1}
    | declaration_block                                         {`Mixin $1}

source:
    | STRING                                                    {`String $1}
    | URI STRING CLOSE_ROUND                                    {`Uri $2}

media_query_list:
    | separated_nonempty_list(COMMA, media_query)               {$1}

media_query:
    | media_type media_expression_list?                         {`Typed ($1, $2)}
    | media_expression_nelist                                   {`Untyped $1}

media_type:
    | media_prefix? IDENT                                       {($1, $2)}

media_prefix:
    | ONLY                                                      {`Only}
    | NOT                                                       {`Not}

media_expression_list:
    | AND separated_nonempty_list(AND, media_expression)        {$2}

media_expression_nelist:
    | separated_nonempty_list(AND, media_expression)            {$1}

media_expression:
    | OPEN_ROUND media_feature media_sentence? CLOSE_ROUND      {($2, $3)}

media_sentence:
    | COLON sentence                                            {$2}

media_feature:
    | IDENT                                                     {$1}

pseudo_page:
    | COLON IDENT                                               {$2}

keyframe_block:
    | keyframe_sel declaration_block                            {($1, $2)}

keyframe_sel:
    | IDENT                                                     {`Ident $1}
    | calc                                                      {`Calc $1}


(********************************************************************************)
(* Selectors.                                                                   *)
(********************************************************************************)

selector_list:
    | separated_nonempty_list(COMMA, selector)      {$1}

selector:
    | simple_selector combination*                  {($1, $2)}

combination:
    | combinator simple_selector                    {($1, $2)}

combinator:
    | S                                             {`Descendant}
    | TILDE                                         {`General_sibling}
    | PLUS                                          {`Adjacent_sibling}
    | GT                                            {`Child}

simple_selector:
    | element qualifier*                            {`Explicit ($1, $2)}
    | qualifier+                                    {`Generic (nelist $1)}

element:
    | IDENT                                         {`Tag $1}
    | ASTERISK                                      {`Universal}

qualifier:
    | HASH                                          {`Id $1}
    | PERIOD IDENT                                  {`Class $2}
    | OPEN_SQUARE IDENT attr_operation CLOSE_SQUARE {`Attr ($2, $3)}
    | COLON IDENT                                   {`Pseudo_class $2}
    | DOUBLE_COLON IDENT                            {`Pseudo_element $2}
    | SEL_FUNC function_args CLOSE_ROUND            {`Sel_func ($1, $2)}

function_args:
    | qualifier+                                    {`Qualified $1}
    | NTH                                           {`Nth $1}
    | IDENT                                         {`Nth $1}

attr_operation:
    | (* empty *)                                   {`Attr_exists}
    | ATTR_EQUALS attr_operand                      {`Attr_equals $2}
    | ATTR_INCLUDES attr_operand                    {`Attr_includes $2}
    | ATTR_DASHMATCH attr_operand                   {`Attr_dashmatch $2}
    | ATTR_PREFIX attr_operand                      {`Attr_prefix $2}
    | ATTR_SUFFIX attr_operand                      {`Attr_suffix $2}
    | ATTR_SUBSTRING attr_operand                   {`Attr_substring $2}

attr_operand:
    | IDENT                                         {$1}
    | STRING                                        {$1}


(********************************************************************************)
(* Declarations.                                                                *)
(********************************************************************************)

declaration_block:
    | OPEN_CURLY declaration+ CLOSE_CURLY           {$2}

declaration:
    | IDENT COLON expr boption(IMPORTANT) SEMICOLON {`Property ($1, $3, $4)}
    | VAR SEMICOLON                                 {`Varref ($startpos($1), $1)}

expr:
    | separated_nonempty_list(COMMA, sentence)      {$1}

sentence:
    | separated_nonempty_list (S?, term)            {$1}

term:
    | calc                                          {`Calc $1}
    | STRING                                        {`String $1}
    | IDENT                                         {`Ident $1}
    | URI STRING CLOSE_ROUND                        {`Uri $2}
    | HASH                                          {`Hash $1}
    | URANGE                                        {`Urange $1}
    | TERM_FUNC expr CLOSE_ROUND                    {`Term_func ($1, $2)}
    | SLASH                                         {`Slash}

calc:
    | VAR                                           {`Varref ($startpos($1), $1)}
    | QUANTITY                                      {`Quantity $1}
    | calc ASTERISK calc                            {`Mul ($startpos($2), $1, $3)}
    | calc QUOTIENT calc                            {`Div ($startpos($2), $1, $3)}
    | calc PLUS calc                                {`Sum ($startpos($2), $1, $3)}
    | calc MINUS calc                               {`Sub ($startpos($2), $1, $3)}
    | OPEN_ROUND calc CLOSE_ROUND                   {$2}
