(* Original file: herdtools7.7.51/herdtools7-7.51/lib/PPCParser.mly *)
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

module PPC = PPCBase
open PPC
open MachSize
%}

%token EOF
%token <PPCBase.reg> ARCH_REG
%token <string> SYMB_REG
%token <int> NUM
%token <string> NAME
%token <string> CSTVAR
%token <int> PROC

%token SEMI COMMA PIPE COLON LPAR RPAR

%token <int> CRK

/* Instruction tokens */
%token LI
%token ADD ADDDOT SUB SUBF SUBFDOT SUBDOT XOR XORDOT OR ORDOT  AND ANDDOT
%token MULL MULLDOT DIV DIVDOT
%token ADDI SUBI ORI XORI ANDIDOT MULLI
%token LBZ LBZX LHZ LHZX LWZ LWZU LWZX
%token MR STB STBX STH STHX STW STWU STWX LWARX STWCX CMPWI CMPW
%token LD STD LDX STDX
%token SYNC EIEIO ISYNC LWSYNC DCBF B BEQ BNE BLT BGT BLE BGE BNL BNG
%token NOR NORDOT NEG NEGDOT SLW SRAWI SRAW BL BLR MTLR MFLR
%token LMW STMW
%token COMMENT
%token <string> STRING

%type <int list * (PPCBase.parsedPseudo) list list> main
%start main

%type <PPCBase.parsedPseudo list> instr_option_seq
%start instr_option_seq

%nonassoc SEMI
%%

main:
| semi_opt proc_list iol_list EOF { $2,$3 }
| semi_opt proc_list EOF { $2,[] }

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

instr_option_seq :
  | instr_option
      {[$1]}
  | instr_option SEMI instr_option_seq 
      {$1::$3}

instr_option :
|            { Nop }
| NAME COLON instr_option { Label ($1,$3) }
| NAME LPAR reg_list RPAR
             { Macro ($1,$3) }
| instr      { Instruction $1}

reg_list :
| { [] }
| reg { [$1] }
| reg COMMA reg_list { $1 :: $3 }

instr:
  | ADD reg COMMA reg COMMA reg
    { Padd (DontSetCR0,$2,$4,$6) }
  | ADDDOT reg COMMA reg COMMA reg
    { Padd (SetCR0,$2,$4,$6) }
  | SUB reg COMMA reg COMMA reg
    { Psub (DontSetCR0,$2,$4,$6) }
  | SUBDOT reg COMMA reg COMMA reg
    { Psub (SetCR0,$2,$4,$6) }
  | SUBF reg COMMA reg COMMA reg
    { Psubf (DontSetCR0,$2,$4,$6) }
  | SUBFDOT reg COMMA reg COMMA reg
    { Psubf (SetCR0,$2,$4,$6) }
  | ADDI reg COMMA reg COMMA k
    { Paddi ($2,$4,$6) }
  | SUBI reg COMMA reg COMMA k
      { Paddi ($2,$4, match $6 with MetaConst.Meta _ as k -> k
                     | MetaConst.Int i -> MetaConst.Int(-i)) }
  | CMPWI reg COMMA k
    { Pcmpwi (0,$2,$4) }
  | CMPWI crindex COMMA reg COMMA k
    { Pcmpwi ($2,$4,$6) }
  | CMPW crindex COMMA reg COMMA reg
    { Pcmpw ($2,$4,$6)}
  | CMPW reg COMMA reg
    { Pcmpw (0,$2,$4)}
  | LI reg COMMA k
    { Pli ($2,$4) }
  | XOR reg COMMA reg COMMA reg
    { Pxor (DontSetCR0,$2,$4,$6) }
  | XORDOT reg COMMA reg COMMA reg
    { Pxor (SetCR0,$2,$4,$6) }
  | XORI reg COMMA reg COMMA k
    { Pxori ($2,$4,$6) }
  | AND reg COMMA reg COMMA reg
    { Pand (DontSetCR0,$2,$4,$6) }
  | ANDDOT reg COMMA reg COMMA reg
    { Pand (SetCR0,$2,$4,$6) }
  | ANDIDOT reg COMMA reg COMMA k
    { Pandi ($2,$4,$6) }
  | OR reg COMMA reg COMMA reg
    { Por (DontSetCR0,$2,$4,$6) }
  | ORDOT reg COMMA reg COMMA reg
    { Por (SetCR0,$2,$4,$6) }
  | ORI reg COMMA reg COMMA k
    { Pori ($2,$4,$6) }
  | MULL  reg COMMA reg COMMA reg
    { Pmull (DontSetCR0,$2,$4,$6) }
  | MULLDOT  reg COMMA reg COMMA reg
    { Pmull (SetCR0,$2,$4,$6) }
  | MULLI reg COMMA reg COMMA k
    { Pmulli ($2,$4,$6) }
  | DIV  reg COMMA reg COMMA reg
    { Pdiv (DontSetCR0,$2,$4,$6) }
  | DIVDOT  reg COMMA reg COMMA reg
    { Pdiv (SetCR0,$2,$4,$6) }
  | LBZ reg COMMA idx COMMA reg
    { Pload (Byte,$2,$4,$6)}
  | LBZ reg COMMA idx LPAR reg RPAR
    { Pload (Byte,$2,$4,$6)}
  | LHZ reg COMMA idx COMMA reg
    { Pload (Short,$2,$4,$6)}
  | LHZ reg COMMA idx LPAR reg RPAR
    { Pload (Short,$2,$4,$6)}
  | LWZ reg COMMA idx COMMA reg
    { Pload (Word,$2,$4,$6)}
  | LWZ reg COMMA idx LPAR reg RPAR
    { Pload (Word,$2,$4,$6)}
  | LWZU reg COMMA idx COMMA reg
    { Plwzu ($2,$4,$6)}
  | LWZU reg COMMA idx LPAR reg RPAR
    { Plwzu ($2,$4,$6)}
  | LD reg COMMA idx COMMA reg
    { Pload (Quad,$2,$4,$6)}
  | LD reg COMMA idx LPAR reg RPAR
    { Pload (Quad,$2,$4,$6)}
  | LBZX reg COMMA reg COMMA reg
    { Ploadx (Byte,$2,$4,$6)}
  | LHZX reg COMMA reg COMMA reg
    { Ploadx (Short,$2,$4,$6)}
  | LWZX reg COMMA reg COMMA reg
    { Ploadx (Word,$2,$4,$6)}
  | LDX reg COMMA reg COMMA reg
    { Ploadx (Quad,$2,$4,$6)}
  | MR reg COMMA reg
    { Pmr ($2,$4) }
  | STB reg COMMA idx COMMA reg
    { Pstore (Byte,$2,$4,$6) }
  | STB reg COMMA idx LPAR reg RPAR
    { Pstore (Byte,$2,$4,$6) }
  | STH reg COMMA idx COMMA reg
    { Pstore (Short,$2,$4,$6) }
  | STH reg COMMA idx LPAR reg RPAR
    { Pstore (Short,$2,$4,$6) }
  | STW reg COMMA idx COMMA reg
    { Pstore (Word,$2,$4,$6) }
  | STW reg COMMA idx LPAR reg RPAR
    { Pstore (Word,$2,$4,$6) }
  | STWU reg COMMA idx COMMA reg
    { Pstwu ($2,$4,$6) }
  | STWU reg COMMA idx LPAR reg RPAR
    { Pstwu ($2,$4,$6) }
  | STD reg COMMA idx COMMA reg
    { Pstore (Quad,$2,$4,$6) }
  | STD reg COMMA idx LPAR reg RPAR
    { Pstore (Quad,$2,$4,$6) }
  | STBX reg COMMA reg COMMA reg
    { Pstorex (Byte,$2,$4,$6) }
  | STHX reg COMMA reg COMMA reg
    { Pstorex (Short,$2,$4,$6) }
  | STWX reg COMMA reg COMMA reg
    { Pstorex (Word,$2,$4,$6) }
  | STDX reg COMMA reg COMMA reg
    { Pstorex (Quad,$2,$4,$6) }
  | LWARX  reg COMMA reg COMMA reg
    { Plwarx ($2,$4,$6)}
  | STWCX reg COMMA reg COMMA reg
    { Pstwcx ($2,$4,$6) }
  | SYNC
    { Psync }
  | EIEIO
    { Peieio }
  | LWSYNC
    { Plwsync }
  | ISYNC
    { Pisync }
  | DCBF reg COMMA reg
    { Pdcbf ($2,$4) }
  | B NAME { Pb $2 }
  | BEQ NAME { Pbcc (Eq,$2) }
  | BNE NAME { Pbcc (Ne,$2) }
  | BLT NAME { Pbcc (Lt,$2) }
  | BGE NAME { Pbcc (Ge,$2) }
  | BNL NAME { Pbcc (Ge,$2) }
  | BGT NAME { Pbcc (Gt,$2) }
  | BLE NAME { Pbcc (Le,$2) }
  | BNG NAME { Pbcc (Le,$2) }
  | NOR reg COMMA reg COMMA reg
    { Pnor (DontSetCR0,$2,$4,$6)}
  | NORDOT reg COMMA reg COMMA reg
    { Pnor (SetCR0,$2,$4,$6)}
  | NEG reg COMMA reg 
    { Pneg (DontSetCR0,$2,$4)}
  | NEGDOT reg COMMA reg 
    { Pneg (SetCR0,$2,$4)}
  | SLW reg COMMA reg COMMA reg
    { Pslw (DontSetCR0,$2,$4,$6)}
  | SRAWI reg COMMA reg COMMA k
    { Psrawi (DontSetCR0,$2,$4,$6)}
  | SRAW reg COMMA reg COMMA reg
    { Psraw (DontSetCR0,$2,$4,$6)}
  | BL NAME { Pbl $2 }
  | BLR { Pblr }
  | MTLR reg { Pmtlr $2}
  | MFLR reg { Pmflr $2}
  | LMW reg  COMMA idx LPAR reg RPAR { Plmw ($2,$4,$6) }
  | STMW reg  COMMA idx LPAR reg RPAR { Pstmw ($2,$4,$6) }
  | COMMENT STRING { Pcomment $2 }
 
k:
| NUM  { MetaConst.Int $1 }
| CSTVAR { MetaConst.Meta $1 }

idx:
| NUM  { MetaConst.Int $1 }
| CSTVAR { MetaConst.Meta $1 }


crindex:
| CRK  { $1 }

reg:
| SYMB_REG { Symbolic_reg $1 }
| ARCH_REG { $1 }
