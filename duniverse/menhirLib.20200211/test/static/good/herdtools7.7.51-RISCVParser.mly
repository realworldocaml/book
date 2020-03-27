(* Original file: herdtools7.7.51/herdtools7-7.51/lib/RISCVParser.mly *)
%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open RISCVBase

let tr_rw = function
  | "r" -> R
  | "w" -> W
  | "rw" -> RW
  | _ -> raise Parsing.Parse_error
%}

%token EOF
%token <RISCVBase.reg> ARCH_REG
%token <string> SYMB_REG
%token <int> NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE COLON LPAR RPAR

/* Instruction tokens */
%token <RISCVBase.opi> OPI
%token <RISCVBase.opiw> OPIW
%token LI
%token <RISCVBase.op> OP
%token <RISCVBase.opw> OPW
%token J
%token <RISCVBase.cond> BCC
%token <RISCVBase.width * RISCVBase.signed * RISCVBase.mo > LOAD
%token <RISCVBase.width * RISCVBase.mo> STORE
%token <RISCVBase.width * RISCVBase.mo> LR
%token <RISCVBase.width * RISCVBase.mo> SC
%token <RISCVBase.opamo * RISCVBase.width * RISCVBase.mo> AMO
%token FENCE FENCEI FENCETSO
%token <string> META
%type <int list * (RISCVBase.parsedPseudo) list list> main 
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
| NUM  { MetaConst.Int $1 }
| META { MetaConst.Meta $1 }

addr:
| NUM LPAR reg RPAR
    { $1,$3 }
| LPAR reg RPAR
    { 0,$2 }

addr0:
| NUM LPAR reg RPAR
    { if $1 <> 0 then raise Parsing.Parse_error;
      $3 }
| LPAR reg RPAR
    { $2 }

instr:
/* OPs */
| LI reg COMMA k
  { OpI (ORI,$2,Ireg X0,$4) }
| OPI reg COMMA reg COMMA k 
  { OpI ($1,$2,$4,$6) }
| OPIW reg COMMA reg COMMA k 
  { OpIW ($1,$2,$4,$6) }
| OP reg COMMA reg COMMA reg 
  { Op ($1,$2,$4,$6) }
| OPW reg COMMA reg COMMA reg 
  { OpW ($1,$2,$4,$6) }
| J NAME
    { J $2 }
| BCC reg COMMA reg COMMA NAME
    { Bcc ($1,$2,$4,$6) }
| LOAD reg COMMA addr
    { let w,s,mo = $1 in
    let off,r = $4 in
    Load (w,s,mo,$2,off,r) }
| STORE reg COMMA addr
    {let w,mo = $1 in
     let off,r = $4 in
     Store (w,mo,$2,off,r) }
| LR reg COMMA addr0
    { let w,mo = $1 in
    LoadReserve (w,mo,$2,$4) }
| SC reg COMMA reg COMMA addr0
    { let w,mo = $1 in
    StoreConditional (w,mo,$2,$4,$6) }
| AMO reg COMMA reg COMMA addr0
    { let op,w,mo = $1 in
    Amo (op,w,mo,$2,$4,$6) }
| FENCEI
    { FenceIns FenceI }
| FENCETSO
    { FenceIns FenceTSO }
| FENCE
    { FenceIns (Fence (RW,RW)) }
| FENCE NAME COMMA NAME
    { FenceIns (Fence (tr_rw $2,tr_rw $4)) } 
