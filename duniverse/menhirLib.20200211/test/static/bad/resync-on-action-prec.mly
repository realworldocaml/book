/* This example was supposed to illustrate resynchronization in Menhir's
   own fancy-parser, but we no longer do it. */

%token A B C
%%
bof:
  erreur1: { action } %prec A
| erreur2: { action }

baz:
    quux {}

