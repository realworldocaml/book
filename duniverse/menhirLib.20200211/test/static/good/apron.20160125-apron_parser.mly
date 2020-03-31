(* Original file: apron.20160125/apron-20160125/mlapronidl/apron_parser.mly *)
/* $Id$ */

/* Syntaxical analysis to convert strings to objects. */

%{
(* This file is part of the APRON Library, released under LGPL license
   with an exception allowing the redistribution of statically linked
   executables.

  Please read the COPYING file packaged in the distribution  *)

let rec neg acc = function
  | [] -> acc
  | (var,coeff)::l ->
      let nacc =
      if Coeff.is_zero coeff then
	acc
      else
	(var,(Coeff.neg coeff))::acc
      in
      neg nacc l

%}

/* \section{Lexems} %======================================================== */

%token TK_EOF

%token TK_VERTEX TK_RAY TK_LINE TK_RAYMOD TK_LINEMOD

%token TK_SUPEG TK_INFEG TK_SUP TK_INF TK_EG TK_DISEG TK_MOD

%token TK_LBRACKET TK_RBRACKET TK_SEMICOLON TK_LPAR TK_RPAR

%token <(Texpr1.typ * Texpr1.round)> TK_MUL
%token <(Texpr1.typ * Texpr1.round)> TK_ADD
%token <(Texpr1.typ * Texpr1.round)> TK_SUB
%token <(Texpr1.typ * Texpr1.round)> TK_DIV
%token <(Texpr1.typ * Texpr1.round)> TK_MODULO
%token <(Texpr1.typ * Texpr1.round)> TK_POW
%token <(Texpr1.typ * Texpr1.round)> TK_CAST
%token <(Texpr1.typ * Texpr1.round)> TK_SQRT


%token <Mpqf.t> TK_MPQF
%token <float> TK_FLOAT
%token <string> TK_VAR


%start lincons generator linexpr tcons texpr

%type <Lincons0.typ * (string*Coeff.t) list> lincons
%type <Generator0.typ * (string*Coeff.t) list> generator
%type <(string*Coeff.t) list> linexpr
%type <Tcons0.typ * Texpr1.expr> tcons
%type <Texpr1.expr> texpr

%%

/* \section{Rules} %========================================================= */

lincons:
  linexpr0 TK_EG linexpr0 TK_EOF { (Lincons0.EQ, neg $1 $3) }
| linexpr0 TK_EG linexpr0 TK_MOD scalar0 TK_EOF { (Lincons0.EQMOD($5), neg $1 $3) }
| linexpr0 TK_DISEG linexpr0 TK_EOF { failwith "!= not yet supported" }
| linexpr0 TK_SUP linexpr0 TK_EOF { (Lincons0.SUP, neg $1 $3) }
| linexpr0 TK_SUPEG linexpr0 TK_EOF { (Lincons0.SUPEQ, neg $1 $3) }
| linexpr0 TK_INFEG linexpr0 TK_EOF { (Lincons0.SUPEQ, neg $3 $1) }
| linexpr0 TK_INF linexpr0 TK_EOF { (Lincons0.SUP, neg $3 $1) }

generator:
  TK_VERTEX linexpr0 TK_EOF { (Generator0.VERTEX,$2) }
| TK_RAY linexpr0 TK_EOF { (Generator0.RAY,$2) }
| TK_LINE linexpr0 TK_EOF { (Generator0.LINE,$2) }
| TK_RAYMOD linexpr0 TK_EOF { (Generator0.RAYMOD,$2) }
| TK_LINEMOD linexpr0 TK_EOF { (Generator0.LINEMOD,$2) }

linexpr:
  linexpr0 TK_EOF { $1 }

linexpr0:
  linexpr0 TK_ADD term
    { $3::$1 }
| linexpr0 TK_SUB term
{ let (var,coeff) = $3 in (var,Coeff.neg coeff)::$1 }
| term { [$1] }
term:
  coeff id { ($2,$1) }
| coeff TK_MUL id { ($3,$1) }
| coeff { ("",$1) }
| id { ($1, Coeff.s_of_int 1) }
| TK_SUB id { ($2, Coeff.s_of_int (-1)) }

tcons:
  tcons0 TK_EOF { $1 }

tcons0:
  texpr0 TK_EG texpr0 { (Tcons0.EQ, (Texpr1.Binop (Texpr1.Sub,$1,$3,Texpr1.Real,Texpr1.Rnd))) }
| texpr0 TK_EG texpr0 TK_MOD scalar0 { (Tcons0.EQMOD($5), (Texpr1.Binop (Texpr1.Sub,$1,$3,Texpr1.Real,Texpr1.Rnd))) }
| texpr0 TK_DISEG texpr0 { failwith "!= not yet supported" }
| texpr0 TK_SUP texpr0 { (Tcons0.SUP, (Texpr1.Binop (Texpr1.Sub,$1,$3,Texpr1.Real,Texpr1.Rnd))) }
| texpr0 TK_SUPEG texpr0 { (Tcons0.SUPEQ, (Texpr1.Binop (Texpr1.Sub,$1,$3,Texpr1.Real,Texpr1.Rnd))) }
| texpr0 TK_INFEG texpr0 { (Tcons0.SUPEQ, (Texpr1.Binop (Texpr1.Sub,$3,$1,Texpr1.Real,Texpr1.Rnd))) }
| texpr0 TK_INF texpr0 { (Tcons0.SUP, (Texpr1.Binop (Texpr1.Sub,$3,$1,Texpr1.Real,Texpr1.Rnd))) }

texpr:
  texpr0 TK_EOF { $1 }

texpr0:
  texpr0 TK_ADD texpr0_1
    { let (t,r) = $2 in Texpr1.Binop(Texpr1.Add,$1,$3,t,r) }
| texpr0 TK_SUB texpr0_1
    { let (t,r) = $2 in Texpr1.Binop(Texpr1.Sub,$1,$3,t,r) }
| texpr0_1
    { $1 }

texpr0_1:
  texpr0_1 TK_MUL texpr0_2
    { let (t,r) = $2 in Texpr1.Binop(Texpr1.Mul,$1,$3,t,r) }
| texpr0_1 TK_DIV texpr0_2
    { let (t,r) = $2 in Texpr1.Binop(Texpr1.Div,$1,$3,t,r) }
| texpr0_1 TK_MODULO texpr0_2
    { let (t,r) = $2 in Texpr1.Binop(Texpr1.Mod,$1,$3,t,r) }
| texpr0_2
    { $1 }
texpr0_2:
| texpr0_3 TK_POW texpr0_2
    { let (t,r) = $2 in Texpr1.Binop(Texpr1.Pow,$1,$3,t,r) }
| texpr0_3
    { $1 }
texpr0_3:
  TK_SUB texpr0_3
    { let (t,r) = $1 in Texpr1.Unop(Texpr1.Neg,$2,t,r) }
| texpr0_4
    { $1 }
texpr0_4:
  TK_CAST texpr0_4
    { let (t,r) = $1 in Texpr1.Unop(Texpr1.Cast,$2,t,r) }
| TK_SQRT texpr0_4
    { let (t,r) = $1 in Texpr1.Unop(Texpr1.Sqrt,$2,t,r) }
| TK_LPAR texpr0 TK_RPAR
    { $2 }
| coeff0
    { Texpr1.Cst($1) }
| id
    { Texpr1.Var(Var.of_string $1) }

id:
  TK_VAR { $1 }

scalar0:
  TK_MPQF { Scalar.Mpqf($1) }
| TK_FLOAT { Scalar.Float($1) }
scalar:
  scalar0 { $1 }
| TK_SUB scalar0 { Scalar.neg $2 }
coeff0:
  scalar0
    { Coeff.Scalar $1 }
| TK_LBRACKET scalar TK_SEMICOLON scalar TK_RBRACKET
    { Coeff.Interval(Interval.of_infsup $2 $4) }
coeff:
  coeff0 { $1 }
| TK_SUB coeff0 { Coeff.neg $2 }
