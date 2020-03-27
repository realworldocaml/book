(* Original file: herdtools7.7.51/herdtools7-7.51/lib/X86Parser.mly *)
%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module X86 = X86Base
open X86
%}

%token EOF
%token <X86Base.reg> ARCH_REG
%token <string> SYMB_REG
%token <string> NUM
%token <string> INTEL_NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE LBRK RBRK
%token LPAR RPAR COLON
/* Instruction tokens */

%token  I_XOR I_OR I_ADD  I_MOV  I_MOVB I_MOVW I_MOVL I_MOVQ I_MOVT I_MOVSD I_DEC  I_CMP  I_CMOVC  I_INC  I_JMP
%token  I_LOCK  I_XCHG   I_LFENCE  I_SFENCE  I_MFENCE
%token  I_READ I_SETNB I_JE I_JNE
%token  I_CMPXCHG

%type <int list * (X86Base.pseudo) list list> main
%start  main

%nonassoc SEMI
%%
main: semi_opt proc_list iol_list EOF { $2,$3 }


semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI  {[$1]}
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
| {Nop}
| NAME COLON instr_option { Label ($1,$3) }
| instr      { Instruction $1}


reg:
| SYMB_REG { Symbolic_reg $1 }
| ARCH_REG { $1 }

k:
| NUM { $1 }

instr:
  | I_XOR   effaddr  COMMA  operand
    {I_XOR ($2,$4)}
  | I_OR   effaddr  COMMA  operand
    {I_OR ($2,$4)}
  | I_ADD   effaddr  COMMA  operand
    {I_ADD ($2,$4)}
  | I_MOV   effaddr  COMMA  operand
    {I_MOV ($2,$4)}
  | I_MOVB   effaddr  COMMA  operand
    {I_MOVB ($2,$4)}
  | I_MOVW   effaddr  COMMA  operand
    {I_MOVW ($2,$4)}
  | I_MOVL   effaddr  COMMA  operand
    {I_MOVL ($2,$4)}
  | I_MOVQ   effaddr  COMMA  operand
    {I_MOVQ ($2,$4)}
  | I_MOVT   effaddr  COMMA  operand
    {I_MOVT ($2,$4)}
  | I_MOVSD
    {I_MOVSD}
  | I_DEC   effaddr
    {I_DEC $2}
  | I_CMP   effaddr COMMA   operand
    {I_CMP ($2,$4)}
  | I_CMOVC reg COMMA  effaddr
    {I_CMOVC ($2, $4)}
  | I_INC   effaddr
    {I_INC $2}
  | I_JMP  NAME
    {I_JMP $2}
  | I_JE NAME
    {I_JCC(C_EQ, $2)}
  | I_JNE NAME
    {I_JCC(C_NE, $2)}
  | I_LOCK semi_opt instr
    {I_LOCK $3 }
  | I_XCHG   effaddr COMMA effaddr
    { I_XCHG ($2,$4)}
  | I_CMPXCHG effaddr COMMA reg
    { I_CMPXCHG ($2,$4)}
  | I_LFENCE
      { I_LFENCE}
  | I_SFENCE
      { I_SFENCE}
  | I_MFENCE
      { I_MFENCE}
  | I_SETNB effaddr {I_SETNB $2 }

effaddr:
  | rm32  {Effaddr_rm32 $1}

rm32:
  |  reg {Rm32_reg $1}
  |  LPAR reg RPAR {Rm32_deref $2}
  |  LBRK reg RBRK {Rm32_deref $2}
  |  LBRK NAME RBRK {Rm32_abs (Constant.Symbolic ($2,0))}
  |  LBRK NUM RBRK {Rm32_abs (Constant.Concrete $2)}

operand:
  | effaddr {Operand_effaddr $1}
  | k {Operand_immediate (Misc.string_as_int $1) }
  | INTEL_NUM {Operand_immediate (Misc.string_as_int $1)} /* enough ? */
