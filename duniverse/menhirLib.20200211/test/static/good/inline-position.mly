%{
  let f x y z t = ()
  let g x y z t = ()
%}

%token A B C
%start prog
%type<int> prog
%%

prog: a1 a2 a3 a4 a5 a6 a7 a8
{()}

/* case: host is almost empty, inlined too. */
a1: x=b
{
  f $startpos $endpos $startpos(x) $endpos(x)
}

%inline b:
{ g $startpos $endpos $startpos $endpos }

/* case: host is not empty, inlined rule is empty. */
a2: a=A x=b b=B
{
  f $startpos $endpos $startpos(x) $endpos(x)
}

/* case: host is almost empty, inlined rule is not empty. */
%inline c: a=A b=B
{
  g $startpos $endpos $startpos(a) $endpos(a)
}

a3: x=c
{
  f $startpos $endpos $startpos(x) $endpos(x)
}

/* case: host is not empty, inlined rule is not empty. */

a4: a=A x=c b=B
{
  f $startpos $endpos $startpos(x) $endpos(x)
}

/* case: host is not empty but unnamed, inlined rule is empty. */
a5: A x=b C
{
  f $startpos $endpos $startpos(x) $endpos(x)
}

/* case: host is empty, inlined rule is not empty but unnamed. */
a6: x=d
{
  f $startpos $endpos $startpos(x) $endpos(x)
}

%inline d: A B
{
  g $startpos $endpos $startpos $endpos
}

/* case: host is not empty, inlined_rule is empty but we do not use startpos. */
%inline f:
{
  g $endpos $endpos $endpos $endpos
}
a7: A x=f B
{
  f $endpos $endpos $endpos(x) $endpos(x)
}

/* case: host is not empty, inlined_rule is empty but we do not use endpos. */
%inline g:
{
  g $startpos $startpos $startpos $startpos
}
a8: A x=g B
{
  f $startpos $startpos $startpos(x) $startpos(x)
}
