(* Original file: diy.5.01/diy-5.01/litmus/ARMParser.mly *)
/*********************************************************************/
/*                        Memevents                                  */
/*                                                                   */
/* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     */
/* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%{
module ARM = ARMBase
open ARM
%}

%token EOF
%token <ARMBase.reg> ARCH_REG
%token <string> SYMB_REG
%token <int> NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE COLON LBRK RBRK

/* Instruction tokens */

%token I_ADD I_ADDS I_AND I_ANDS I_B I_BEQ I_BNE I_CMP I_MOV I_MOVNE I_MOVEQ I_XOR I_XORS I_DMB I_DSB I_ISB
%token I_LDR I_LDREX I_LDRNE I_LDREQ I_STR I_STRNE I_STREQ I_STREX
%token I_SY I_ST I_ISH I_ISHST I_NSH I_NSHST I_OSH I_OSHST
%type <int list * (ARMBase.pseudo) list list> main 
%start  main

%nonassoc SEMI
%%

main:
| semi_opt proc_list iol_list EOF { $2,$3 }

semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI
    {[$1]}

| PROC PIPE proc_list  { $1::$3 }

iol_list :
|  instr_option_list SEMI
    {[$1]}
|  instr_option_list SEMI iol_list {$1::$3}

instr_option_list :
  | instr_option
      {[$1]}
  | instr_option PIPE instr_option_list 
      {$1::$3}

instr_option :
|            { Nop }
| NAME COLON instr_option { Label ($1,$3) }
| instr      { Instruction $1}

reg:
| SYMB_REG { Symbolic_reg $1 }
| ARCH_REG { $1 }

k:
| NUM { $1 }

instr:
  | I_ADD reg COMMA reg COMMA k
     { I_ADD (DontSetFlags,$2,$4,$6) }
  | I_ADDS reg COMMA reg COMMA k
     { I_ADD (SetFlags,$2,$4,$6) }
  | I_ADD reg COMMA reg COMMA reg
     { I_ADD3 (DontSetFlags,$2, $4, $6) }
  | I_ADDS reg COMMA reg COMMA reg
     { I_ADD3 (SetFlags,$2, $4, $6) }
  | I_AND reg COMMA reg COMMA k
     { I_AND (DontSetFlags,$2,$4,$6) }
  | I_ANDS reg COMMA reg COMMA k
     { I_AND (SetFlags,$2,$4,$6) }
  | I_B NAME
     { I_B $2 }
  | I_BNE NAME
     { I_BNE $2 }
  | I_BEQ NAME
     { I_BEQ $2 }
  | I_CMP reg COMMA k
     { I_CMPI ($2,$4) }
  | I_CMP reg COMMA reg
     { I_CMP ($2,$4) }
/* Load */
  | I_LDR reg COMMA reg
     { I_LDR ($2,$4, AL) }
  | I_LDR reg COMMA LBRK reg RBRK
     { I_LDR ($2,$5,AL) }
  | I_LDR reg COMMA LBRK reg COMMA reg RBRK
     { I_LDR3 ($2,$5,$7,AL) }
  | I_LDRNE reg COMMA reg
     { I_LDR ($2,$4,NE) }
  | I_LDRNE reg COMMA LBRK reg RBRK
     { I_LDR ($2,$5,NE) }
  | I_LDRNE reg COMMA LBRK reg COMMA reg RBRK
     { I_LDR3 ($2,$5,$7,NE) }
  | I_LDREQ reg COMMA reg
     { I_LDR ($2,$4,EQ) }
  | I_LDREQ reg COMMA LBRK reg RBRK
     { I_LDR ($2,$5,EQ) }
  | I_LDREQ reg COMMA LBRK reg COMMA reg RBRK
     { I_LDR3 ($2,$5,$7,EQ) }
  | I_LDREX reg COMMA reg
     { I_LDREX ($2,$4) }
  | I_LDREX reg COMMA LBRK reg RBRK
     { I_LDREX ($2,$5) }
/* Store */
  | I_STR reg COMMA reg
     { I_STR ($2,$4,AL) }
  | I_STR reg COMMA LBRK reg RBRK
     { I_STR ($2,$5,AL) }
  | I_STR reg COMMA LBRK reg COMMA reg RBRK
     { I_STR3 ($2,$5,$7,AL) }
  | I_STRNE reg COMMA reg
     { I_STR ($2,$4,NE) }
  | I_STRNE reg COMMA LBRK reg RBRK
     { I_STR ($2,$5,NE) }
  | I_STRNE reg COMMA LBRK reg COMMA reg RBRK
     { I_STR3 ($2,$5,$7,NE) }
  | I_STREQ reg COMMA reg
     { I_STR ($2,$4,EQ) }
  | I_STREQ reg COMMA LBRK reg RBRK
     { I_STR ($2,$5,EQ) }
  | I_STREQ reg COMMA LBRK reg COMMA reg RBRK
     { I_STR3 ($2,$5,$7,EQ) }
  | I_STREX reg COMMA reg COMMA LBRK reg RBRK
     { I_STREX ($2,$4,$7,AL) }
/* MOVE */
  | I_MOV reg COMMA k
     { I_MOVI ($2,$4,AL) }
  | I_MOVNE reg COMMA k
     { I_MOVI ($2,$4,NE) }
  | I_MOVEQ reg COMMA k
     { I_MOVI ($2,$4,EQ) }
  | I_MOV reg COMMA reg
     { I_MOV ($2,$4,AL) }
  | I_MOVNE reg COMMA reg
     { I_MOV ($2,$4,NE) }
  | I_MOVEQ reg COMMA reg
     { I_MOV ($2,$4,EQ) }
  | I_XOR reg COMMA reg COMMA reg
     { I_XOR (DontSetFlags,$2,$4,$6) }
  | I_XORS reg COMMA reg COMMA reg
     { I_XOR (SetFlags,$2,$4,$6) }
/* FENCES */
  | I_DMB { I_DMB SY }
  | I_DSB opt { I_DSB $2 }
  | I_DMB opt { I_DMB $2 }
  | I_DSB { I_DSB SY }
  | I_ISB { I_ISB }

opt:
  | I_SY { SY }
  | I_ST { ST }
  | I_ISH { ISH }
  | I_ISHST { ISHST }
  | I_NSH { NSH }
  | I_NSHST { NSHST }
  | I_OSH { OSH }
  | I_OSHST { OSHST }
