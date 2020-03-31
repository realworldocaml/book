(* Original file: herdtools7.7.51/herdtools7-7.51/lib/stateParser.mly *)
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

open Constant
open MiscParser
open ConstrGen
let mk_sym s = Symbolic (s,0)
%}

%token EOF
%token <int> PROC
%token <string> SYMB_REG
%token <string> NAME
%token <string> DOLLARNAME
%token <string> NUM

%token TRUE FALSE
%token EQUAL NOTEQUAL EQUALEQUAL PLUS_DISJ
%token FINAL FORALL EXISTS OBSERVED TOKAND NOT AND OR IMPLIES CASES WITH FILTER
%token LOCATIONS STAR
%token LBRK RBRK LPAR RPAR SEMI COLON AMPER
%token ATOMIC
%token ATOMICINIT

%token PTX_REG_DEC
%token <string> PTX_REG_TYPE


%left PLUS_DISJ OR
%left AND
%right IMPLIES
%nonassoc NOT

%type <MiscParser.state> init
%start init
%type <MiscParser.location> location
%start location
%type <(MiscParser.location * MiscParser.run_type) list * MiscParser.prop option * MiscParser.constr * (string * MiscParser.quantifier) list> constraints
%start constraints
%type  <MiscParser.constr> constr
%start constr
%type  <MiscParser.constr> skip_loc_constr
%start skip_loc_constr
%type  <(MiscParser.location * MiscParser.run_type) list * MiscParser.constr> loc_constr
%start loc_constr
%type <MiscParser.location list> locs
%start locs
%type <MiscParser.prop option> filter
%start filter
%%

/* For initial state */
init:
| init_semi_list EOF { $1 }


reg:
| NAME       {  $1 }
| DOLLARNAME {  $1 }
maybev:
| NUM  { Concrete $1 }
| NAME { mk_sym $1 }

location_reg:
| PROC COLON reg  {Location_reg ($1,$3)}
| NUM COLON reg   {Location_reg (Misc.string_as_int $1,$3)}
| SYMB_REG        {Location_sreg $1 }
/* PTX registers */
| NUM COLON PTX_REG_DEC PTX_REG_TYPE reg
                  {Location_reg(Misc.string_as_int $1,$5)}
| PROC COLON PTX_REG_DEC PTX_REG_TYPE reg
                  {Location_reg($1,$5)}

location_deref:
| location_reg { $1 }
| STAR location_reg { $2 }
| STAR NAME { Location_global (mk_sym $2) }

location:
| location_reg { $1 }
| LBRK maybev RBRK {Location_global $2}
/* Hum, for backward compatibility, and compatibility with printer */
| maybev { Location_global $1 }

atom:
| location {($1,ParsedConstant.zero)}
| location EQUAL maybev {($1,$3)}

atom_init:
| atom { let x,v = $1 in x,(TyDef,v) }
| NAME location { $2,(Ty $1,ParsedConstant.zero)}
| ATOMIC NAME location { $3,(Atomic $2,ParsedConstant.zero)}
| NAME location EQUAL maybev { ($2,(Ty $1,$4))}
| NAME location EQUAL ATOMICINIT LPAR maybev RPAR { ($2,(Ty $1,$6))}
| NAME STAR location { ($3,(Pointer $1,ParsedConstant.zero))}
| NAME STAR location EQUAL amperopt maybev { ($3,(Pointer $1,$6))}
| STAR location { ($2,(TyDefPointer,ParsedConstant.zero))}
| STAR location EQUAL amperopt maybev { ($2,(TyDefPointer,$5))}
| NAME NAME LBRK NUM RBRK { (Location_global (mk_sym $2),(TyArray ($1,Misc.string_as_int $4),ParsedConstant.zero)) }

amperopt:
| AMPER { () }
| { () }


init_semi_list:
| {[]}
| SEMI {[]}
| atom_init {$1::[]}
| atom_init SEMI init_semi_list  {$1::$3}


/* For final state constraints */

loc_deref:
 NAME LBRK NUM RBRK { Location_deref (mk_sym $1, Misc.string_as_int $3) }

loc_typ:
| loc_deref { ($1,TyDef) }
| location { ($1, TyDef) }
| location STAR { ($1, TyDefPointer) }
| location NAME { ($1, Ty $2) }
| location NAME STAR { ($1, Pointer $2) }

loc_semi_list:
| {[]}
| SEMI {[]}
| loc_typ {$1::[]}
| loc_typ SEMI loc_semi_list  {$1::$3}

locations:
|  LOCATIONS LBRK loc_semi_list RBRK { $3 }
| { [] }

locs:
| { [] }
| location locs { $1 :: $2 }

filter:
| { None }
| FILTER prop { Some $2 }

constraints:
| locations filter old_constraints
  { let x = $1 in
    let f = $2 in
    let y,z = $3 in
    x,f,y,z }

old_constraints :
| final EOF { $1,[] }
| final WITH kinds EOF { $1,$3 }


kinds :
| kind         { [$1] }
| kind SEMI    { [$1] }
| kind SEMI kinds   { $1 :: $3 }

kind:
| NAME COLON FORALL { ($1,Require) }
| NAME COLON EXISTS { ($1,Allow) }
| NAME COLON NOT EXISTS { ($1,Forbid) }


final:
| constr { $1 }
| constr SEMI { $1 }

constr:
|  { ConstrGen.constr_true }
| FORALL prop
    {ForallStates $2}
| EXISTS prop
    {ExistsState $2}
| NOT EXISTS prop
        { NotExistsState $3 }
| FINAL prop
        { ExistsState $2 }
| LPAR prop RPAR
    {ExistsState $2}
| OBSERVED obs
    { ExistsState (Or $2) }

obs:
|  obsone {  [ And $1 ] }
|  obsone TOKAND obs { And $1 :: $3 }

obsone:
|                       { [] }
| atom_prop SEMI obsone { $1 :: $3 }

loc_constr:
| locations constr { $1,$2 }

skip_loc_constr:
| locations constr { $2 }


atom_prop:
| loc_deref  EQUAL maybev {Atom (LV ($1,$3))}
| location EQUAL maybev {Atom (LV ($1,$3))}
| loc_deref  EQUALEQUAL maybev {Atom (LV ($1,$3))}
| location EQUALEQUAL maybev {Atom (LV ($1,$3))}
| loc_deref  NOTEQUAL maybev {Not (Atom (LV ($1,$3)))}
| location NOTEQUAL maybev {Not (Atom (LV ($1,$3)))}
| location EQUAL location_deref {Atom (LL ($1,$3))}
| location EQUALEQUAL location_deref {Atom (LL ($1,$3))}


prop:
| TRUE
    {And []}
| FALSE
    {Or []}
| atom_prop
    { $1 }
| NOT prop
    {Not $2}
| prop AND prop
    {And [$1;$3]}
| prop OR  prop
    {Or [$1;$3]}
| prop IMPLIES prop
    { Implies ($1,$3) }
| LPAR prop RPAR
    { $2 }
