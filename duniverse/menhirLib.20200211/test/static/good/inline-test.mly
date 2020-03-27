%token PLUS TIMES EOF
%left PLUS
%left TIMES
%token<int> INT
%start<int> prog
%%

prog: x=exp EOF { x }

exp: x = INT         { x }
|    lhs = exp; op = op; rhs = exp  { op lhs rhs }

%inline op: PLUS { fun x y -> x + y }
          | TIMES { fun x y -> x * y }
