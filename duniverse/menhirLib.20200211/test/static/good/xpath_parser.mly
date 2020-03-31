%{
  open Xpath_syntax
  open Xpath_tree
  open Axis
%}

%token SLASH LPAREN RPAREN LBRACKET RBRACKET
%token DOUBLESLASH DOUBLEDOT AT DOT COMMA PIPE
%token OR AND EQUAL NOTEQUAL LT GT LTE GTE
%token PLUS MINUS MUL DIV MOD
%token <string> VAR
%token <string> NAME_TEST AXIS FUNCTION_NAME NODE_TYPE
%token <string> LITERAL
%token <float> NUMBER
%token EOF

%type <Xpath_syntax.expr> expr
%start expr


%type <Xpath_syntax.path_expr> location_path
%start location_path


%%

location_path:
   relative_location_path      { $1 }
 | absolute_location_path      { $1 }

absolute_location_path:
   SLASH   { Root }
 | SLASH relative_location_path  { Slash (Root,$2) }
 | DOUBLESLASH relative_location_path { double_slash Root $2 }

relative_location_path:
   step    { $1 }
 | relative_location_path SLASH step { Slash ($1,$3) }
 | relative_location_path DOUBLESLASH step { double_slash $1 $3 }

step:
 | axis_specifier step2 { Axis ($1,$2) }
 | step2 { $1 }
 | DOT   { dot  }
 | DOUBLEDOT  { dotdot }

step2:
 | step2 LBRACKET expr RBRACKET { Condition ($1,$3) }
 | node_test { $1 }

axis_specifier:
 | AXIS {
     try Axis.of_string $1
     with Not_found -> failwith ("Unknown axis :"^$1)
   }
 | AT { Attribute }


node_test:
 | NAME_TEST  { Name $1 }
 | NODE_TYPE RPAREN {
     try TypeTest (Type_test.of_string $1)
     with Not_found -> failwith ("Unknown node type :"^$1)
   }
 | NODE_TYPE LITERAL RPAREN {
     match $1 with
       | "processing-instruction" ->
	   TypeTest (Type_test.Pi_test (Some $2))
       | _ -> failwith "Only processing-instruction tests accept argument"
   }



primary_expr:
   function_call   { $1 }
 | VAR             { Var $1 }
 | LPAREN expr RPAREN  { $2 }
 | LITERAL         { String_literal $1 }
 | NUMBER          { Number_literal $1 }

function_call:
   FUNCTION_NAME RPAREN  { Function ($1,[]) }
 | FUNCTION_NAME arguments RPAREN  { Function ($1,$2) }

arguments:
   arguments COMMA expr   { $1@[$3] }
 | expr { [$1] }

union_expr:
   path_expr        { $1 }
 | union_expr PIPE path_expr  { path_expr (Pipe(expr $1, expr $3)) }

path_expr:
   location_path    { path_expr $1 }
 | filter_expr      { $1 }
 | filter_expr SLASH relative_location_path
     {  path_expr (Slash (expr $1,$3)) }
 | filter_expr DOUBLESLASH relative_location_path
     {  path_expr (double_slash (expr $1) $3) }


filter_expr:
   primary_expr { $1 }
 | filter_expr LBRACKET expr RBRACKET { path_expr (Condition (expr $1,$3)) }


expr:
   or_expr         { $1 }

or_expr:
   and_expr        { $1 }
 | or_expr OR and_expr { Or($1,$3) }

and_expr:
   equality_expr   { $1 }
 | and_expr AND equality_expr { And($1,$3) }

equality_expr:
   relational_expr  { $1 }
 | equality_expr EQUAL relational_expr { Equal($1,$3) }
 | equality_expr NOTEQUAL relational_expr { NotEqual($1,$3) }

relational_expr:
   additive_expr    { $1 }
 | relational_expr LT additive_expr { Lower($1,$3) }
 | relational_expr GT additive_expr { Greater($1,$3) }
 | relational_expr LTE additive_expr { LowerEqual($1,$3) }
 | relational_expr GTE additive_expr { GreaterEqual($1,$3) }

additive_expr:
   multiplicative_expr  { $1 }
 | additive_expr PLUS multiplicative_expr  { Plus($1,$3) }
 | additive_expr MINUS multiplicative_expr  { Minus($1,$3) }

multiplicative_expr:
   unary_expr  { $1 }
 | multiplicative_expr MUL unary_expr  { Mul ($1,$3) }
 | multiplicative_expr DIV unary_expr  { Div ($1,$3) }
 | multiplicative_expr MOD unary_expr  { Mod ($1,$3) }

unary_expr:
 | union_expr  { $1 }
 | MINUS unary_expr { UnaryMinus $2 }

