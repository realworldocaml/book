%token A B C D E
%start<unit> main
%%
%inline callee: B C a = D { $startpos(a), $startpos }
main: A a = callee e = E { a, $startpos(a), $startpos, $startpos(e) }
