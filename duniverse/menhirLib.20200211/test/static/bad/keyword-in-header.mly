%{

  let x = $1

%}

%token A B
%start foo

foo: A B {}
