%token A B
%start<unit> main

%%

main:
  option1(A);
  option2(B);
    {}

let option1(x) :=
  |        {  None  }
  | x = x; { Some x }

let option2(x) :=
  |        { None }
  | ~ = x; < Some >
