%token A EOF
%start<unit> main
%%
%inline eps: {}
%public liste(X): eps {}
%public liste(X): X liste(X) {}
main: liste(A) EOF {}
/* The definition of liste(X) is split in two.
   Note: liste(X) cannot be renamed list(X),
   as it would then collide with the standard
   library. */
