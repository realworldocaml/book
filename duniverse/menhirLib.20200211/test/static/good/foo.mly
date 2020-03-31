%start exp
%token<int> INT
%token PLUS
%token TIMES
%left PLUS
%left TIMES
%type <int> exp
%%
exp:
| lhs = exp TIMES rhs = exp
{
let op =                   ( fun x y -> x * y ) in
                                    ( op lhs rhs )
}
| lhs = exp PLUS rhs = exp
{
let op =                  ( fun x y -> x + y ) in
                                    ( op lhs rhs )
}
| x = INT
{
                     ( x )
}

