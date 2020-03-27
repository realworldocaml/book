%token A B C D

%%

main:
  A B         { () }
| A ( x = B C { () }
  /* due to anonymous rules, this actually looks good so far... */

/* but here, it no longer makes sense: */
bar:
  A           { () }

baz:
  bar main    { () }

