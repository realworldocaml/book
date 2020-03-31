/* Taken from Pager's paper. Sent by Laurence Tratt.
   The LALR automaton has 21 states.
   Pager writes that his construction leads to 23 states.
   Yet, Menhir's version of Pager's algorithm leads to 38 states.
   Not sure why. Deserves study. */

%start<unit> x
%token A B C D E T U

%%

x : A y D {} | A z C {} | A t {} | B y E {} | B z D {} | B t {}
y : T w {} | U x {}
z : T U {}
t : U x A {}
w : U v {}
v : {}
