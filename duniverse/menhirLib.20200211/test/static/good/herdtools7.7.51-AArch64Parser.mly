(* Original file: herdtools7.7.51/herdtools7-7.51/lib/AArch64Parser.mly *)
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


open AArch64Base
%}

%token EOF
%token <AArch64Base.reg> ARCH_XREG
%token <string> SYMB_XREG
%token <AArch64Base.reg> ARCH_WREG
%token <string> SYMB_WREG
%token <int> NUM
%token <string> NAME
%token <string> META
%token <string> CODEVAR
%token <int> PROC

%token SEMI COMMA PIPE COLON LBRK RBRK
%token SXTW

/* Instructions */
%token B BEQ BNE CBZ CBNZ EQ NE
%token LDR LDP LDNP STP STNP LDRB LDRH STR STRB STRH LDAR LDAPR LDXR LDAXR STLR STXR STLXR CMP MOV
%token <AArch64Base.op> OP
%token CSEL CSINC CSINV CSNEG
%token DMB DSB ISB
%token SY ST LD
%token OSH OSHST OSHLD
%token ISH ISHST ISHLD
%token NSH NSHST NSHLD

%type <int list * (AArch64Base.parsedPseudo) list list> main
%type <AArch64Base.parsedPseudo list> instr_option_seq
%start  main instr_option_seq

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

instr_option_seq :
  | instr_option
      {[$1]}
  | instr_option SEMI instr_option_seq
      {$1::$3}

instr_option :
|            { Nop }
| NAME COLON instr_option { Label ($1,$3) }
| CODEVAR    { Symbolic $1 }
| instr      { Instruction $1}

reg:
| SYMB_XREG { V64,Symbolic_reg $1 }
| ARCH_XREG { V64,$1 }
| SYMB_WREG { V32,Symbolic_reg $1 }
| ARCH_WREG { V32,$1 }

xreg:
| SYMB_XREG { Symbolic_reg $1 }
| ARCH_XREG { $1 }

wreg:
| SYMB_WREG { Symbolic_reg $1 }
| ARCH_WREG { $1 }

k:
| NUM  { MetaConst.Int $1 }
| META { MetaConst.Meta $1 }

kr:
| k { K $1 }
| xreg { RV (V64,$1) }
| wreg COMMA SXTW { RV (V32,$1) }

kr0:
| { K (MetaConst.zero) }
| COMMA kr { $2 }

kwr:
| k { K $1 }
| wreg { RV (V32,$1) }

ldp_instr:
| LDP
  { (fun v r1 r2 r3 kr -> I_LDP (TT,v,r1,r2,r3,kr)) }
| LDNP
  { (fun v r1 r2 r3 kr -> I_LDP (NT,v,r1,r2,r3,kr)) }

stp_instr:
| STP
  { (fun v r1 r2 r3 kr -> I_STP (TT,v,r1,r2,r3,kr)) }
| STNP
  { (fun v r1 r2 r3 kr -> I_STP (NT,v,r1,r2,r3,kr)) }

cond:
| EQ { EQ }
| NE { NE }

instr:
/* Branch */
| B NAME { I_B $2 }
| BEQ NAME { I_BC (EQ,$2) }
| BNE NAME { I_BC (NE,$2) }
| CBZ reg COMMA NAME   { let v,r = $2 in I_CBZ (v,r,$4) }
| CBNZ reg COMMA NAME  { let v,r = $2 in I_CBNZ (v,r,$4) }
/* Memory */
| LDR reg COMMA LBRK xreg kr0 RBRK
  { let v,r = $2 in I_LDR (v,r,$5,$6) }
| ldp_instr wreg COMMA wreg COMMA LBRK xreg kr0 RBRK
  { $1 V32 $2 $4 $7 $8 }
| ldp_instr xreg COMMA xreg COMMA LBRK xreg kr0 RBRK
  { $1 V64 $2 $4 $7 $8 }
| stp_instr wreg COMMA wreg COMMA LBRK xreg kr0 RBRK
  { $1 V32 $2 $4 $7 $8 }
| stp_instr xreg COMMA xreg COMMA LBRK xreg kr0 RBRK
  { $1 V64 $2 $4 $7 $8 }
| LDRB wreg COMMA LBRK xreg kr0 RBRK
  { I_LDRBH (B,$2,$5,$6) }
| LDRH wreg COMMA LBRK xreg kr0 RBRK
  { I_LDRBH (H,$2,$5,$6) }
| LDAR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_LDAR (v,AA,r,$5) }
| LDXR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_LDAR (v,XX,r,$5) }
| LDAXR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_LDAR (v,AX,r,$5) }
| LDAPR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_LDAR (v,AQ,r,$5) }
| STR reg COMMA LBRK xreg kr0 RBRK
  { let v,r = $2 in I_STR (v,r,$5,$6) }
| STRB wreg COMMA LBRK xreg kr0 RBRK
  { I_STRBH (B,$2,$5,$6) }
| STRH wreg COMMA LBRK xreg kr0 RBRK
  { I_STRBH (H,$2,$5,$6) }
| STLR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_STLR (v,r,$5) }
| STXR wreg COMMA reg COMMA LBRK xreg RBRK
  { let v,r = $4 in I_STXR (v,YY,$2,r,$7) }
| STLXR wreg COMMA reg COMMA LBRK xreg RBRK
  { let v,r = $4 in I_STXR (v,LY,$2,r,$7) }

/* Operations */
| MOV xreg COMMA kr
  { I_MOV (V64,$2,$4) }
| MOV wreg COMMA kwr
  { I_MOV (V32,$2,$4) }
| SXTW xreg COMMA wreg
  { I_SXTW ($2,$4) }
| OP xreg COMMA xreg COMMA kr
  { I_OP3 (V64,$1,$2,$4,$6) }
| OP wreg COMMA wreg COMMA kwr
    { I_OP3 (V32,$1,$2,$4,$6) }
| CMP wreg COMMA kwr
  { I_OP3 (V32,SUBS,ZR,$2,$4) }
| CMP xreg COMMA kr
  { I_OP3 (V64,SUBS,ZR,$2,$4) }
/* Misc */
| CSEL xreg COMMA  xreg COMMA  xreg COMMA cond
  { I_CSEL (V64,$2,$4,$6,$8,Cpy) }
| CSEL wreg COMMA  wreg COMMA  wreg COMMA cond
  { I_CSEL (V32,$2,$4,$6,$8,Cpy) }
| CSINC xreg COMMA  xreg COMMA  xreg COMMA cond
  { I_CSEL (V64,$2,$4,$6,$8,Inc) }
| CSINC wreg COMMA  wreg COMMA  wreg COMMA cond
  { I_CSEL (V32,$2,$4,$6,$8,Inc) }
| CSINV xreg COMMA  xreg COMMA  xreg COMMA cond
  { I_CSEL (V64,$2,$4,$6,$8,Inv) }
| CSINV wreg COMMA  wreg COMMA  wreg COMMA cond
  { I_CSEL (V32,$2,$4,$6,$8,Inv) }
| CSNEG xreg COMMA  xreg COMMA  xreg COMMA cond
  { I_CSEL (V64,$2,$4,$6,$8,Neg) }
| CSNEG wreg COMMA  wreg COMMA  wreg COMMA cond
  { I_CSEL (V32,$2,$4,$6,$8,Neg) }

/* Fences */
| DMB fenceopt
  { let d,t = $2 in I_FENCE (DMB (d,t)) }
| DSB fenceopt
  { let d,t = $2 in I_FENCE (DSB (d,t)) }
| ISB
  { I_FENCE ISB }

fenceopt:
| SY
  { SY,FULL }
| ST
  { SY,ST }
| LD
  { SY,LD }
| OSH
  { OSH,FULL }
| OSHST
  { OSH,ST }
| OSHLD
  { OSH,LD }
| ISH
  { ISH,FULL }
| ISHST
  { ISH,ST }
| ISHLD
  { ISH,LD }
| NSH
  { NSH,FULL }
| NSHST
  { NSH,ST }
| NSHLD
  { NSH,LD}
