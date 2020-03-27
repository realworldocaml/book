%token A B C D L R
%type <unit> s
%start s

/* Cf. mon journal au 19 septembre 2005. Si on résoud le conflit situé
   dans l'état LA en donnant priorité à la réduction, alors la chaîne
   RABC, qui aurait dû être acceptée sans ambiguïté, est rejetée. */

%nonassoc B
%nonassoc A /* gives higher precedence to production b -> A over token B */

%%

s:
  L l { () }
| R r { () }

l:
  a C { () }
| b B { () }

r:
  a C { () }
| b D { () }

a:
  A B { () }

b:
  A   { () }

