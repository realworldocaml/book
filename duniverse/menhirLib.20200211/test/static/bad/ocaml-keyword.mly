%type <int * int> constraint
%start constraint
%start type
%token EQ

%%

constraint:
  type EQ type
    { $1, $3 }

type:
  INT
    { TypInt }

