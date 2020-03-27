(* Original file: herdtools7.7.51/herdtools7-7.51/lib/CParser.mly *)
%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let mk_sym s = Constant.Symbolic (s,0)

open CBase
open MemOrder
open CType
open MemOrderOrAnnot
%}

%token EOF
%token <string> IDENTIFIER
%token <string> BASE_TYPE
%token <string> ATOMIC_TYPE
%token <string> CONSTVAR
%token <string> CODEVAR
%token <int> PROC
%token LPAR RPAR COMMA LBRACE RBRACE STAR
%token ATOMIC CHAR INT LONG VOID
%token MUTEX
%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CONST VOLATILE

/* For shallow parsing */
%token <string> BODY
%type <string CAst.t list> shallow_main
%start shallow_main

/* For deep parsing */
%token <string> CONSTANT
%token NULL
%token SEMI COLON EQ EQ_OP NEQ_OP LT LE GT GE DOT
%token XOR PIPE
%token LAND
%token ADD SUB
%token MUL DIV
%token WHILE IF ELSE
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%token <MemOrder.t> MEMORDER
%token LD_EXPLICIT ST_EXPLICIT EXC_EXPLICIT FENCE LOCK UNLOCK SPINLOCK SPINUNLOCK SPINTRYLOCK SPINISLOCKED SCAS WCAS SCAS_EXPLICIT WCAS_EXPLICIT
%token LOAD STORE UNDERFENCE XCHG CMPXCHG
%token   UNDERATOMICOP  UNDERATOMICOPRETURN UNDERATOMICFETCHOP UNDERATOMICADDUNLESS ATOMICADDUNLESS
%token <Op.op> ATOMIC_FETCH_EXPLICIT

%left PIPE
%left XOR
%left LAND

%nonassoc EQ_OP NEQ_OP LT LE GT GE
%left ADD SUB
%left STAR DIV
%nonassoc CAST
%nonassoc PREC_BASE

%type <(CBase.pseudo list) CAst.test list> deep_main
%start deep_main

%type <CBase.pseudo list> pseudo_seq
%start pseudo_seq

%type <CBase.macro list> macros
%start macros

%%

parameter_list:
| { [] }
| parameter_declaration { [ $1 ] }
| parameter_declaration COMMA parameter_list { $1 :: $3 }

parameter_declaration:
| toptyp IDENTIFIER { {CAst.param_ty = $1; param_name = $2} }

toptyp:
| typ STAR { Pointer $1 }

typ:
| typ STAR { Pointer $1 }
| typ VOLATILE { Volatile $1 }
| ATOMIC base { Atomic $2 }
| VOLATILE base0 { Volatile $2 }
| base { $1 }

base0:
| ATOMIC_TYPE { Atomic (Base $1) }
| BASE_TYPE { (Base $1) }
| VOID { (Base "void") }
| ty_attr MUTEX { Base ($1 ^ "mutex") }
| ty_attr CHAR { Base ($1 ^ "char") }
| ty_attr INT { Base ($1 ^ "int") }
| ty_attr LONG { Base ($1 ^ "long") }


base:
| base0 { $1 }
| LPAR typ RPAR %prec PREC_BASE { $2 }

ty_attr:
| { "" }

shallow_main:
| EOF { [] }
| BODY shallow_main { CAst.Global $1 :: $2 }
| voidopt PROC LPAR parameter_list RPAR BODY shallow_main
    { CAst.Test {CAst.proc = $2; params = $4; body = $6} :: $7 }

voidopt:
| VOID { () }
| { () }

declaration:
| typ IDENTIFIER SEMI { DeclReg ($1,$2) }

initialisation:
| typ IDENTIFIER EQ expr { StoreReg (Some $1,$2,$4) ; }

atomic_op:
| ADD { Op.Add }
| SUB { Op.Sub }

annot:
| annot_base  { $1 }
| annot_base SUB annot { $1 ^ "-" ^ $3 }

annot_base :
| LOCK       { "lock" }
| UNLOCK     { "unlock" }
| IDENTIFIER { $1 }



annot_list:
| annot COMMA annot_list
  {$1::$3}
| annot
  {[$1]}


expr:
| LPAR expr RPAR { $2 }
| CONSTANT { Const(Constant.Concrete $1) }
| CONSTVAR { Const(mk_sym $1) }
| IDENTIFIER { LoadReg $1 }
| LPAR typ RPAR expr %prec CAST { $4 }
| STAR IDENTIFIER { LoadMem (LoadReg $2,AN []) }
| STAR LPAR typ RPAR IDENTIFIER { LoadMem (LoadReg $5,AN []) }
| STAR LPAR expr RPAR { LoadMem ($3,AN []) }
| LOAD LBRACE annot_list RBRACE LPAR expr RPAR { LoadMem($6,AN $3) }
| LD_EXPLICIT LPAR expr COMMA MEMORDER RPAR { LoadMem($3,MO $5) }
| expr STAR expr { Op(Op.Mul,$1,$3) }
| expr ADD expr { Op(Op.Add,$1,$3) }
| expr SUB expr { Op(Op.Sub,$1,$3) }
| expr DIV expr { Op(Op.Div,$1,$3) }
| expr LAND expr { Op(Op.And,$1,$3) }
| expr PIPE expr { Op(Op.Or,$1,$3) }
| expr XOR expr { Op(Op.Xor,$1,$3) }
| expr EQ_OP expr { Op(Op.Eq,$1,$3) }
| expr NEQ_OP expr { Op(Op.Ne,$1,$3) }
| expr LT expr { Op(Op.Lt,$1,$3) }
| expr GT expr { Op(Op.Gt,$1,$3) }
| expr LE expr { Op(Op.Le,$1,$3) }
| expr GE expr { Op(Op.Ge,$1,$3) }
| EXC_EXPLICIT LPAR expr COMMA expr COMMA MEMORDER RPAR
  { Exchange($3, $5, MO $7) }
| XCHG LBRACE annot_list RBRACE LPAR expr COMMA expr RPAR
  { Exchange($6,$8,AN $3) }
| CMPXCHG LBRACE annot_list RBRACE LPAR expr COMMA expr COMMA expr RPAR
  { CmpExchange($6,$8,$10,$3) }
| ATOMIC_FETCH_EXPLICIT LPAR expr COMMA expr COMMA MEMORDER RPAR
  { Fetch($3, $1, $5, $7) }
| IDENTIFIER LPAR args RPAR
  { ECall ($1,$3) }
| WCAS LPAR expr COMMA expr COMMA expr RPAR
  { ECas ($3,$5,$7,SC,SC,false) }
| WCAS_EXPLICIT LPAR expr COMMA expr COMMA expr COMMA MEMORDER COMMA MEMORDER  RPAR
  { ECas ($3,$5,$7,$9,$11,false) }
| SCAS LPAR expr COMMA expr COMMA expr RPAR
  { ECas ($3,$5,$7,SC,SC,true) }
| SCAS_EXPLICIT LPAR expr COMMA expr COMMA expr COMMA MEMORDER COMMA MEMORDER  RPAR
  { ECas ($3,$5,$7,$9,$11,true) }
| SPINTRYLOCK LPAR expr RPAR
  { TryLock ($3,MutexLinux) }
| SPINISLOCKED LPAR expr RPAR
  { IsLocked ($3,MutexLinux) }
| UNDERATOMICOPRETURN LBRACE annot_list RBRACE LPAR expr COMMA atomic_op COMMA expr RPAR
  { AtomicOpReturn($6,$8,$10,OpReturn,$3) }
| UNDERATOMICFETCHOP LBRACE annot_list RBRACE LPAR expr COMMA atomic_op COMMA expr RPAR
  { AtomicOpReturn($6,$8,$10,FetchOp,$3) }
| UNDERATOMICADDUNLESS LPAR expr COMMA expr COMMA expr RPAR
  { AtomicAddUnless($3,$5,$7,false) }
| ATOMICADDUNLESS LPAR expr COMMA expr COMMA expr RPAR
  { AtomicAddUnless($3,$5,$7,true) }

args:
| { [] }
| args_ne { $1 }

args_ne:
| expr { [$1] }
| expr COMMA args_ne { $1 :: $3 }

location:
| IDENTIFIER { LoadReg($1) }
| STAR location { LoadMem($2,AN []) }
| LPAR expr RPAR { $2 }

instruction:
| IF LPAR expr RPAR block_ins %prec LOWER_THAN_ELSE
  { If($3,$5,None) }
| IF LPAR expr RPAR block_ins ELSE block_ins
  { If($3,$5,Some $7) }
| initialisation SEMI
  { $1 }
| IDENTIFIER EQ expr SEMI
  { StoreReg(None,$1,$3) }
| STAR location EQ expr SEMI
  { StoreMem($2,$4,AN []) }
| STORE LBRACE annot_list RBRACE LPAR expr COMMA expr RPAR SEMI
  { StoreMem($6,$8,AN $3) }
| ST_EXPLICIT LPAR expr COMMA expr COMMA MEMORDER RPAR SEMI
  { StoreMem($3, $5, MO $7) }

| LOCK LPAR expr RPAR SEMI
  { Lock ($3,MutexC11) }
| UNLOCK LPAR expr RPAR SEMI
  { Unlock ($3,MutexC11) }
| SPINLOCK LPAR expr RPAR SEMI
  { Lock ($3,MutexLinux) }
| SPINUNLOCK LPAR expr RPAR SEMI
  { Unlock ($3,MutexLinux) }
| UNDERFENCE LBRACE annot_list RBRACE SEMI
  { Fence(AN $3) }
| UNDERATOMICOP LPAR expr COMMA atomic_op COMMA expr RPAR SEMI
  { AtomicOp($3,$5,$7) }
| FENCE LPAR MEMORDER RPAR SEMI
  { Fence(MO $3) }
| CODEVAR SEMI
  { Symb $1 }
| IDENTIFIER LPAR args RPAR SEMI
  { PCall ($1,$3) }

ins_seq:
| block_ins { [$1] }
| block_ins ins_seq { $1::$2 }
| declaration { [$1] }
| declaration ins_seq { $1::$2 }

block_ins:
| instruction { $1 }
| LBRACE ins_seq RBRACE { Seq($2,true) }

pseudo_seq:
| block_ins { [Instruction $1] }
| block_ins pseudo_seq { (Instruction $1)::$2 }
| declaration { [] }
| declaration pseudo_seq { $2 }

function_def:
| voidopt PROC LPAR parameter_list RPAR LBRACE pseudo_seq RBRACE
  { { CAst.proc = $2;
      CAst.params = $4;
      CAst.body = $7 } }

trans_unit:
| function_def
  { [$1] }
| trans_unit function_def
  { $1 @ [$2] }

deep_main:
| trans_unit EOF { $1 }

formals_ne:
| IDENTIFIER { [ $1 ] }
| IDENTIFIER COMMA formals_ne { $1 :: $3 }

formals:
| { [] }
| formals_ne { $1 }

body:
| LBRACE ins_seq RBRACE { Seq ($2,true) }

macro:
| IDENTIFIER LPAR formals RPAR expr { EDef ($1,$3,$5) }
| IDENTIFIER LPAR formals RPAR body { PDef ($1,$3,$5) }

macros:
| { [] }
| macro macros
    { $1 :: $2 }
