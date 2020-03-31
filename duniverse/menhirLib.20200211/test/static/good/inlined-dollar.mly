%token<int> A B
%start<unit> t
%%

t : B y=y {
  y
}

%inline y: A
{
  $1
}
