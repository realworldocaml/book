%start<unit> s
%token<unit> A
%%

s: x=a A y=b {
  x; y
}

%inline a: A {
  $1
}

%inline b: A {
  $1
}
