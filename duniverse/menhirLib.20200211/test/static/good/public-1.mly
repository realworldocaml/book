/* Vérifie le fonctionnemenent de %public. Cas valides. */
%token T
%start<int> s
%%

%public a: T {}

s: T {}
