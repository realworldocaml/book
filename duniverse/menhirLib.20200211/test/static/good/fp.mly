%{
open Fp;;
%}

  %token <string> Lprim

  %token Leval
  %token Lpar Rpar
  %token Lang Rang
  %token Lsqu Rsqu
  %token Lcom Lscl Leol

  %token Lapplytoall Lins Lo Lcond Lcst Lbu Lwhile

  %token LT LF

  %token Laff LDef LUndef LShow LQuit LLoad LSave

  %token <int> Ls
  %token <int> Lr
  %token <int> Lint
  %token <string> Lident
  %token <string> Lvar
  %token <string> Lstr


  %right Leval
  %left Lprim
  %right Lcond
  %right Lo
  %left Lcst


  %start cmd
  %type <Fp.cmd> cmd

  %start exp
  %type <Fp.expr> exp

  %start fct
  %type <Fp.fct> fct

  %%

cmd :
  LDef Lident Laff fct Leol { Def($2,$4) }
| LUndef Lident Leol        { Undef $2 }
| LShow Lident Leol         { Show $2 }
| exp Leol                  { Exp $1 }
| LQuit Leol                { Quit }
| LLoad Lstr Leol           { Load $2 }
| LSave Lstr Leol           { Save $2 }
| Leol                      { None }
    ;

exp :
  LT                { T }
| LF                { F }
| Lint              { Int (Num.Int $1) }
| Lvar              { Var $1 }
| Lpar exp Rpar     { $2 }
| Lang Rang         { Seq [] }
| Lang list Rang    { Seq $2 }
| fct Leval exp     { App($1, $3) }
    ;

fatom :
    Lprim           { Prim $1 }
|   Ls              { Sel $1 }
|   Lr              { RSel $1 }

|   Lident          { User $1 }

|   Lapplytoall fatom    { ApplyToAll $2 }
|   Lins fatom           { Insert $2 }
|   Lcst exp             { Constant $2 }
|   Lsqu fctlist Rsqu    { Construction $2 }
|   Lbu fatom exp        { Bu($2,$3) }
|   Lwhile fatom fatom   { While($2,$3) }
|   Lpar fct Rpar        { $2 }
    ;

fct :
    comp Lcond comp Lscl fct { Condition($1,$3,$5) }
|   comp                     { $1 }
    ;

comp :
    fatom Lo comp  { Composition($1,$3) }
|   fatom          { $1 }

list :
    exp Lcom list  { $1 :: $3 }
|   exp            { [ $1 ] }
    ;

fctlist :
    fct Lcom fctlist { $1 :: $3 }
|   fct              { [ $1 ] }
    ;

  %%

