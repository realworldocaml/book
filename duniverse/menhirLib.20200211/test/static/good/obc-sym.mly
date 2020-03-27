/*
 *  Oxford Oberon-2 compiler
 *  symparse.mly
 *  Copyright (C) J. M. Spivey 1995, 1998
 */

%{
open Dict
open Symtab
open Error
open Print
open Mach
open Eval

let rcsid = "$Id: symparse.mly 76 2005-03-01 14:20:43Z mike $"
%}

%token 			ARRAY CONST PARAM DEF END ENUM FIELD TARGET
%token 			FLEX METH METHOD POINTER PROC PROCEDURE RECORD USE
%token 			STRING TYPE VAR VOID VPARAM I F REF SYMFILE ANON
%token			LOCAL ABSREC ABSMETH VARINFO PROCINFO
%token<string> 		TAG NUM HEX FLO
%token<Dict.otype>	BASICTYPE
%token<Dict.export> 	MARK

%type<Dict.def list * int>  file
%start			file

%{
let modname = ref anon

let in_table = Growvect.create 100 voidtype

(* All definitions created here have d_level = 0; this is wrong for
   e.g. formal parameters and record fields, but it doesn't matter in
   the rest of the compiler.  *)

let make_def x v k t =
  { d_tag = x; d_module = !modname; d_export = v; d_kind = k;
    d_used = true; d_loc = no_loc; d_type = t; d_lab = nosym;
    d_level = 0; d_offset = 0; d_param = 0; d_map = [] }

let field_def x v t o =
  { d_tag = x; d_module = !modname; d_export = v; d_kind = FieldDef;
    d_used = true; d_loc = no_loc; d_type = t; d_lab = nosym;
    d_level = 1; d_offset = o; d_param = 0; d_map = [] }

let def_type k m n (g, r, map) =
  let t = { t_id = n; t_name = anon; t_module = m; t_level = 0; t_guts = g;
    t_rep = r; t_desc = nosym; t_map = map } in
  Growvect.set in_table k t; t

let use_type k =
  Growvect.get in_table k

let desc = ref nosym
%}

%%

file :
    header defs END HEX
      { (List.rev $2, int_of_string $4) } ;

header :
    SYMFILE ident HEX
      { modname := $2;
	if int_of_string $3 <> Config.signature then begin
	  sem_error "symbol table for '$' is from wrong version of compiler"
	    [fId $2] no_loc;
	  exit 2
	end } ;

defs :
    /* EMPTY */			{ [] }
  | defs def			{ $2::$1 }
  | defs fixup			{ $1 } ;

def :
    TYPE ident MARK otype
      { if $4.t_name = anon then $4.t_name <- $2;
	make_def $2 $3 TypeDef $4 }
  | VAR ident MARK otype
      { let d = make_def $2 $3 VarDef $4 in
	d.d_lab <- sprintf "$.$" [fId !modname; fId $2]; d }
  | CONST ident MARK otype const
      { make_def $2 $3 (ConstDef $5) $4 }
  | STRING ident MARK int symbol
      { let d = make_def $2 Visible (StringDef $4)
		  (new_type 0 (row ($4+1) character)) in
	d.d_lab <- $5; d }
  | PROCEDURE ident MARK otype
      { let d = make_def $2 $3 ProcDef $4 in
	d.d_lab <- proc_name !modname 0 $2; d } ;

fixup :
    TARGET int otype
      { let t0 = use_type $2 in
	match t0.t_guts with
	    PointerType d -> d.d_type <- $3
	  | _ -> failwith "TARGET" }
  | METHOD int ident MARK int symbol otype
      { let t0 = use_type $2 in
	let r = get_record t0 in
	let d = make_def $3 $4 ProcDef $7 in
	d.d_offset <- $5; d.d_lab <- $6;
	r.r_methods <- r.r_methods @ [d] }
  | PROCINFO symbol otype locals END
      { let g = { g_name = $2; g_result = $3; g_locals = List.rev $4 } in
	Dict.put_info $2 (ProcInfo g) }
  | VARINFO symbol otype
      { let v = { v_name = $2; v_type = $3 } in
	Dict.put_info $2 (VarInfo v) }
  | typedef			{ }

otype :
    USE int			{ use_type $2 }
  | BASICTYPE			{ $1 }
  | typedef			{ $1 } ;

typedef :
    DEF int tguts
      { let t = def_type $2 !modname $2 $3 in
	if is_record t then t.t_desc <- !desc;
	t }
  | REF int ident int tname tguts
      { let t = def_type $2 $3 $4 $6 in
	if is_record t then t.t_desc <- !desc;
	t.t_name <- $5; t } ;

tguts :
    POINTER
      { (PointerType (make_def anon Private TypeDef voidtype),
	  addr_rep, [GC_Offset 0]) }
  | ENUM int
      { (EnumType $2, int_rep, []) }
  | RECORD symbol int otype locals END
      { desc := $2; record false $4 $3 (List.rev $5) }
  | ABSREC symbol int otype locals END
      { desc := $2; record true $4 $3 (List.rev $5) }
  | ARRAY int otype
      { row $2 $3 }
  | FLEX otype
      { flex $2 }
  | prockind int otype locals END
      { let p = { p_kind = $1; p_pcount = $2;
		  p_result = $3; p_fparams = List.rev $4 } in
	(ProcType p, addr_rep, []) } ;

locals :
    /* EMPTY */			{ [] }
  | locals local		{ $2::$1 }
  | locals fixup		{ $1 } ;

local :
    kind ident MARK int otype
      { let d = make_def $2 $3 $1 $5 in
	d.d_offset <- $4; d } ;

kind :
    LOCAL			{ VarDef }
  | PARAM			{ ParamDef }
  | VPARAM			{ VParamDef }
  | FIELD			{ FieldDef } ;

tname :
    ident			{ $1 }
  | ANON			{ anon } ;

ident :
    TAG				{ intern $1 } ;

symbol :
    TAG				{ $1 } ;

const :
    I NUM			{ IntVal (Int32.of_string $2) }
  | F FLO			{ FloVal (float_of_string $2) } ;

int :
    NUM				{ int_of_string $1 } ;

prockind :
    PROC			{ Procedure }
  | METH			{ Method }
  | ABSMETH			{ AbsMeth } ;
