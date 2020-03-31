%token A B C
%{ let x = 2
%%
foo: A { () }
bar: B { () }
