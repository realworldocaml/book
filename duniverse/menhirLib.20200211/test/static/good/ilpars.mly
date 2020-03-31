%{
(* (c) Microsoft Corporation 2005-2007.  *)

(*F# open Microsoft.Research.AbstractIL
open Microsoft.Research.AbstractIL.Internal
module Ilascii = Microsoft.Research.AbstractIL.Internal.AsciiConstants
module Ildiag = Microsoft.Research.AbstractIL.Diagnostics
module Il = Microsoft.Research.AbstractIL.IL
module Ilx = Microsoft.Research.AbstractIL.Extensions.ILX.Types
module Il = Microsoft.Research.AbstractIL.IL
module Illib = Microsoft.Research.AbstractIL.Internal.Library
F#*)

open Illib
open Ildiag
open Il
open Nums
open Ilascii
open Ilx

let pfailwith s = prerr_endline ("*** error: "^s); raise Parsing.Parse_error

(*-----------------------------------------------------------------------
 * vararg sentinels
 *----------------------------------------------------------------------*)

type sig_arg = SigArg of (string option * typ)  | Sentinel

let decode_varargs args =
  let rec normals = function
      [] -> ([],None)
    | Sentinel :: t -> ([],Some (varargs t))
    | SigArg (_,p)::t -> let (n,r) = normals t in (p::n, r)
  and varargs = function
      [] -> []
    | SigArg (_,ty):: t ->  let l = varargs t in ty::l
    | Sentinel :: t -> pfailwith "two sentinels in vararg call" in
  normals args;;

(*-----------------------------------------------------------------------
 *
 *----------------------------------------------------------------------*)

type 'a resolved_at_mspec_scope =
    ResolvedAtMethodSpecScope of (genparams -> 'a)

let no_mspec_scope x = ResolvedAtMethodSpecScope (fun cgparams -> x)
let resolve_mspec_scope (ResolvedAtMethodSpecScope f) x = f x
let resolve_mspec_scope_then (ResolvedAtMethodSpecScope f) g =
  ResolvedAtMethodSpecScope (fun x -> resolve_mspec_scope (g(f x)) x)

let resolve_mspec_scope_to_formal_scope tspeco obj =
  match tspeco with
    None ->  resolve_mspec_scope obj mk_empty_gparams
  | Some tspec -> resolve_mspec_scope obj (gparams_of_inst (inst_of_tspec tspec))

let resolve_mspec_scope_to_current_scope obj =
    resolve_mspec_scope obj mk_empty_gparams

(*-----------------------------------------------------------------------
 *
 *----------------------------------------------------------------------*)

let find_mscorlib_aref() =
  match (!parse_ilGlobals).mscorlib_scoref with
  | ScopeRef_assembly aref -> aref
  | _ -> ecma_mscorlib_assref (* drop back to mscorlib, why not error?? *)

let find_aref nm =
  if nm = "mscorlib" then find_mscorlib_aref() else
  pfailwith ("Undefined assembly ref '" ^ nm ^ "'")

%}

/*-----------------------------------------------------------------------
 * The YACC Grammar
 *----------------------------------------------------------------------*/

%token <Nums.i64> VAL_INT64     /* 342534523534534      0x34FA434644554 */
%token <Nums.i32> VAL_INT32_ELIPSES     /* 342534523534534... */
%token <Nums.ieee64> VAL_FLOAT64        /* -334234 24E-34 */
%token <Ilascii.arg_instr> INSTR_ARG
%token <Ilascii.field_instr> INSTR_FIELD
%token <Ilascii.i32_instr> INSTR_I
%token <Ilascii.i32_i32_instr> INSTR_I32_I32
%token <Ilascii.i64_instr> INSTR_I8
%token <Ilascii.real_instr> INSTR_R
%token <Ilascii.loc_instr> INSTR_LOC
%token <Ilascii.method_instr> INSTR_METHOD
%token <Ilascii.none_instr> INSTR_NONE
%token <Ilascii.sig_instr> INSTR_SIG
%token <Ilascii.string_instr> INSTR_STRING
%token <Ilascii.switch_instr> INSTR_SWITCH
%token <Ilascii.tok_instr> INSTR_TOK
%token <Ilascii.type_instr> INSTR_TYPE
%token <Ilascii.int_type_instr> INSTR_INT_TYPE
%token <Ilascii.valuetype_instr> INSTR_VALUETYPE
%token <int>     VAL_HEXBYTE    /* 05 1A FA */
%token <string>  VAL_ID                 /* testing343 */
%token <string>  VAL_DOTTEDNAME                 /* testing343.abd */
%token <string>  VAL_QSTRING    /* "Hello World\n" */
%token <string>  VAL_SQSTRING   /* 'Hello World\n' */
%token AMP
%token BANG
%token BOOL
%token BYTEARRAY
%token CDECL
%token CHAR
%token CLASS
%token COMMA
%token DCOLON
%token DEFAULT
%token DOT
%token DOT_CCTOR
%token DOT_CTOR
%token ELIPSES
%token EOF
%token EXPLICIT
%token FASTCALL
%token FIELD
%token FLOAT32
%token FLOAT64
%token GREATER
%token INSTANCE
%token INT
%token INT16
%token INT32
%token INT64
%token INT8
%token LBRACK
%token LESS
%token LPAREN
%token METHOD
%token NATIVE
%token OBJECT
%token PLUS
%token RBRACK
%token RPAREN
%token SLASH
%token STAR
%token STDCALL
%token STRING
%token THISCALL
%token TYPEDREF
%token UINT
%token UINT16
%token UINT32
%token UINT64
%token UINT8
%token UNMANAGED
%token UNSIGNED
%token VALUE
%token VALUETYPE
%token VARARG
%token VOID

/* %type <Il.modul> modul */
%type <string> name1
%type <Il.typ resolved_at_mspec_scope> typ
%type <Il.instr array> top_instrs
%type <Il.typ> top_typ
%start top_instrs top_typ

/**************************************************************************/
%%

/* ENTRYPOINT */
top_typ: typ EOF
       { resolve_mspec_scope $1 [] }

/* ENTRYPOINT */
top_instrs: instrs2 EOF
       { Array.of_list $1 }


compQstring:
     VAL_QSTRING { $1 }
   | compQstring PLUS VAL_QSTRING { $1 ^ $3 }

methodName:
     DOT_CTOR
       { ".ctor" }
   | DOT_CCTOR
       { ".cctor" }
   | name1
       { $1 }

instrs2:
   | instr instrs2
        { $1 []  :: $2  }
   | { [] }




bytearrayhead:
     BYTEARRAY LPAREN
       { lexing_bytearray := true }

bytes:
     bytes_aux
       {  lexing_bytearray := false;
          Bytes.of_intarray (Array.of_list (List.rev $1)) }
   |
       {  lexing_bytearray := false;
          Bytes.of_intarray [| |] }
bytes_aux:
     VAL_HEXBYTE
       { [ $1 ] }
   | bytes_aux VAL_HEXBYTE
       { $2 :: $1 }

methodSpec: methodSpecMaybeArrayMethod
    { let  data,varargs = $1 in
      mk_mspec_in_typ data,varargs }


methodSpecMaybeArrayMethod:
     callConv typ typSpec DCOLON methodName opt_actual_tyargs LPAREN sigArgs0 RPAREN
       { let callee_class_typ = resolve_mspec_scope_to_current_scope $3 in
         let gscope = (if Ilx.gen_is_array_ty callee_class_typ then None else Some (tspec_of_typ callee_class_typ)) in
         let argtys_n_varargs = resolve_mspec_scope_to_formal_scope gscope $8 in
         let (argtys,varargs) = decode_varargs argtys_n_varargs in
         let minst = resolve_mspec_scope_to_current_scope $6 in
         let callee_retty = resolve_mspec_scope_to_formal_scope gscope $2 in
         (callee_class_typ, $1, $5, argtys, callee_retty, minst), varargs }

   | callConv typ methodName opt_actual_tyargs LPAREN sigArgs0 RPAREN
       { let argtys_n_varargs = resolve_mspec_scope_to_formal_scope None $6 in
         let (argtys,varargs) = decode_varargs argtys_n_varargs in
         let retty = resolve_mspec_scope_to_formal_scope None $2 in
         let minst = resolve_mspec_scope_to_current_scope $4 in
         (typ_for_toplevel ScopeRef_local, $1, $3, argtys, retty, minst), varargs }

fieldSpec:
     typ typSpec DCOLON id
       { let callee_class_typ = resolve_mspec_scope_to_current_scope $2 in
         let tspec = tspec_of_typ callee_class_typ in
         let tref = tref_of_tspec tspec in
         let callee_field_typ = resolve_mspec_scope_to_formal_scope (Some tspec) $1 in
         mk_fspec_in_typ (callee_class_typ, $4, callee_field_typ)
       }
   | typ id
       { let callee_field_typ = resolve_mspec_scope_to_formal_scope None $1 in
         mk_fspec_in_typ (typ_for_toplevel ScopeRef_local, $2, callee_field_typ)
       }


instr_r_head:
     INSTR_R LPAREN
        { lexing_bytearray := true; $1 }

instr:
     INSTR_NONE
        {  ($1 ()) }
   | INSTR_ARG int32
        {  ($1 (int_to_u16 ( (i32_to_int $2)))) }
   | INSTR_LOC int32
        {  ($1 (int_to_u16 ( (i32_to_int $2)))) }
   | INSTR_I int32
        {  ($1 $2) }
   | INSTR_I32_I32 int32 int32
        {  ($1 ($2,$3)) }
   | INSTR_I8 int64
        {  ($1 $2) }
   | INSTR_R float64
        {  ($1 (NUM_R8 $2)) }
   | INSTR_R int64
        {  ($1 (NUM_R8 (i64_to_ieee64 $2))) }
   | instr_r_head bytes RPAREN
        {  let r =
             let b = $2 in
             match Bytes.length b with
               4 -> NUM_R4 (ieee32_of_four_bytes  (Bytes.get b 0) (Bytes.get b 1) (Bytes.get b 2) (Bytes.get b 3))
             | 8 -> NUM_R8 (ieee64_of_eight_bytes  (Bytes.get b 0) (Bytes.get b 1) (Bytes.get b 2) (Bytes.get b 3) (Bytes.get b 4) (Bytes.get b 5) (Bytes.get b 6) (Bytes.get b 7))
             | _ -> pfailwith "you must use exactly four or eight bytes when specifying a float by bytes." in
            ($1 r) }
   | INSTR_METHOD methodSpecMaybeArrayMethod
        {
             begin
               let  ((encl_typ, cc, nm, argtys, retty, minst) as data),varargs = $2 in
               if Ilx.gen_is_array_ty encl_typ then
                 (fun prefixes ->
                   let (shape,ty,p) = gen_dest_array_ty encl_typ in
                   begin match nm with
                   | "Get" -> if not p then I_ldelem_any(shape,ty) else I_other (mk_ilx_ext_instr (EI_ldelem_any_erasable (shape,ty)))
                   | "Set" ->  if not  p then I_stelem_any(shape,ty) else I_other (mk_ilx_ext_instr (EI_stelem_any_erasable (shape,ty)))
                   | "Address" ->  if not p then I_ldelema((if prefixes=[Prefix_Readonly] then ReadonlyAddress else NormalAddress), shape,ty) else failwith "cannot use ldelema on this kind of array"
                   | ".ctor" ->   if not p then I_newarr(shape,ty) else  I_other (mk_ilx_ext_instr (EI_newarr_erasable (shape,ty)))
                   | _ -> failwith "bad method on array type"
                   end)
               else

                 $1 (mk_mspec_in_typ data, varargs)
             end }
   | INSTR_FIELD fieldSpec
        {  ($1 $2) }
   | INSTR_TYPE typSpec
        {  ($1 (resolve_mspec_scope_to_current_scope $2)) }
   | INSTR_INT_TYPE int32 typSpec
        {  ($1 (Int32.to_int $2,resolve_mspec_scope_to_current_scope $3)) }
   | INSTR_VALUETYPE typSpec
        { let vtr =
             match resolve_mspec_scope_to_current_scope $2 with
               (* Type_boxed tr -> Type_value tr
             | Type_value vtr as typ -> typ
             | *) typ ->  typ in
           ($1 vtr) }
   | INSTR_STRING compQstring
        {  ($1 (Bytes.string_as_unicode_bytes $2))  }
   | INSTR_STRING  bytearrayhead bytes RPAREN
        {  ($1 $3)  }
   | INSTR_SIG callConv typ LPAREN sigArgs0 RPAREN
        { let argtys_n_varargs = resolve_mspec_scope_to_current_scope $5 in
          let (argtys,varargs) = decode_varargs argtys_n_varargs in
          let callee_retty = resolve_mspec_scope_to_current_scope $3 in
          let mref = mk_callsig ($2,argtys,callee_retty) in
           ($1 (mref,varargs))
        }
   | INSTR_TOK typSpec
        {  ($1 (Token_type (resolve_mspec_scope_to_current_scope $2)))  }
   | INSTR_TOK METHOD methodSpec
        { let (argtys,varargs) = $3 in
           ($1 (Token_method argtys))
        }
   | INSTR_TOK FIELD fieldSpec
        {  ($1 (Token_field $3)) }

/*-----------------------------------------------
 * Formal signatures of methods etc.
 *---------------------------------------------*/

sigArgs0:
     /* EMPTY */
        {  no_mspec_scope [] }
   | sigArgs1   { $1 }

sigArgs1:
   sigArgs1a
        { ResolvedAtMethodSpecScope (fun c -> List.map (fun obj -> resolve_mspec_scope obj c) (List.rev $1)) }

sigArgs1a:
     sigArg
        { [$1] }
   | sigArgs1a COMMA sigArg
        { $3:: $1 }

sigArg:
     ELIPSES
       { no_mspec_scope Sentinel }
   | typ opt_id
       { resolve_mspec_scope_then $1 (fun ty ->
         no_mspec_scope (SigArg($2, ty))) }


opt_id: /* EMPTY */ { None } | id { Some $1 }


/*-----------------------------------------------
 * Type names
 *---------------------------------------------*/
name1:
   | id
        { $1 }
   | VAL_DOTTEDNAME
        { $1 }
   | name1 DOT id
        { $1 ^"."^ $3 }

className:
     LBRACK name1 RBRACK slashedName
        { let (enc,nm) = $4 in
          let aref = find_aref $2 in
          ScopeRef_assembly aref, enc, nm }
   | slashedName
        { let enc, nm = $1 in (ScopeRef_local, enc, nm) }

slashedName:
     name1
        { ([],$1) }
   | name1 SLASH slashedName
        { let (enc,nm) = $3 in ($1::enc, nm)  }

typeNameInst:
     className opt_actual_tyargs
        { let (a,b,c) = $1 in
          resolve_mspec_scope_then $2 (fun inst ->
          no_mspec_scope ( (mk_tspec ( (mk_nested_tref (a,b,c)), inst)))) }


typeName:
     className
        { let (a,b,c) = $1 in
          no_mspec_scope ( (mk_tspec ( (mk_nested_tref (a,b,c)), mk_empty_gactuals))) }
   | LBRACK name1 RBRACK
        { (* reference to the toplevel constructs of an assembly *)
          let aref = find_aref  $2 in
          no_mspec_scope (tspec_for_toplevel ScopeRef_local) }


typSpec:
     typeName
        { resolve_mspec_scope_then $1 (fun tref ->
          no_mspec_scope (Type_boxed tref))  }
   | typ
        { $1 }
   | LPAREN typ RPAREN
        { $2 }


callConv:
     INSTANCE callKind
        { Callconv (CC_instance,$2) }
   | EXPLICIT callKind
        { Callconv (CC_instance_explicit,$2) }
   | callKind
        { Callconv (CC_static,$1) }

callKind:
     /* EMPTY */
      { CC_default }
   | DEFAULT
      { CC_default }
   | VARARG
      { CC_vararg }
   | UNMANAGED CDECL
      { CC_cdecl }
   | UNMANAGED STDCALL
      { CC_stdcall }
   | UNMANAGED THISCALL
      { CC_thiscall }
   | UNMANAGED FASTCALL
      { CC_fastcall }

/*-----------------------------------------------
 * The full algebra of types, typically producing results
 * awaiting further info about how to fix up type
 * variable numbers etc.
 *---------------------------------------------*/

typ: STRING
       { no_mspec_scope (!parse_ilGlobals).typ_String }
   | OBJECT
       { no_mspec_scope (!parse_ilGlobals).typ_Object }
   | CLASS typeNameInst
       { resolve_mspec_scope_then $2 (fun tspec ->
          no_mspec_scope (Type_boxed tspec)) }
   | VALUE CLASS typeNameInst
       { resolve_mspec_scope_then $3 (fun tspec ->
         no_mspec_scope (Type_value tspec)) }
   | VALUETYPE typeNameInst
       { resolve_mspec_scope_then $2 (fun tspec ->
         no_mspec_scope (Type_value tspec)) }
   | typ LBRACK RBRACK
       { resolve_mspec_scope_then $1 (fun ty -> no_mspec_scope (mk_sdarray_ty ty)) }
   | typ LBRACK bounds1 RBRACK
       { resolve_mspec_scope_then $1 (fun ty -> no_mspec_scope (mk_array_ty (ty,ArrayShape $3))) }
   | typ LBRACK RBRACK  BANG
       { resolve_mspec_scope_then $1 (fun ty -> no_mspec_scope (Ilx.mk_array_ty_old (sdshape,ty))) }
   | typ LBRACK bounds1 RBRACK BANG
       { resolve_mspec_scope_then $1 (fun ty -> no_mspec_scope (Ilx.mk_array_ty_old (ArrayShape $3,ty))) }
   | typ AMP
       { resolve_mspec_scope_then $1 (fun ty -> no_mspec_scope (Type_byref ty)) }
   | typ STAR
       { resolve_mspec_scope_then $1 (fun ty -> no_mspec_scope (Type_ptr ty)) }
   | METHOD callConv typ STAR LPAREN sigArgs0 RPAREN
        { resolve_mspec_scope_then $3 (fun callee_retty ->
          resolve_mspec_scope_then $6 (fun argtys_n_varargs ->
          let (argtys,varargs) = decode_varargs argtys_n_varargs in
          let csig = mk_callsig($2,
                                    argtys,
                                    callee_retty) in
         no_mspec_scope (Type_fptr csig))) }

   | TYPEDREF
       { no_mspec_scope (!parse_ilGlobals).typ_TypedReference }
   | CHAR
       { no_mspec_scope (!parse_ilGlobals).typ_char }
   | VOID
       { no_mspec_scope Type_void }
   | BOOL
       { no_mspec_scope (!parse_ilGlobals).typ_bool }
   | INT8
       { no_mspec_scope (!parse_ilGlobals).typ_int8 }
   | INT16
       { no_mspec_scope (!parse_ilGlobals).typ_int16 }
   | INT32
       { no_mspec_scope (!parse_ilGlobals).typ_int32 }
   | INT64
       { no_mspec_scope (!parse_ilGlobals).typ_int64 }
   | FLOAT32
       { no_mspec_scope (!parse_ilGlobals).typ_float32 }
   | FLOAT64
       { no_mspec_scope (!parse_ilGlobals).typ_float64 }
   | UNSIGNED INT8
       { no_mspec_scope (!parse_ilGlobals).typ_uint8 }
   | UNSIGNED INT16
       { no_mspec_scope (!parse_ilGlobals).typ_uint16 }
   | UNSIGNED INT32
       { no_mspec_scope (!parse_ilGlobals).typ_uint32 }
   | UNSIGNED INT64
       { no_mspec_scope (!parse_ilGlobals).typ_uint64 }
   | UINT8
       { no_mspec_scope (!parse_ilGlobals).typ_uint8 }
   | UINT16
       { no_mspec_scope (!parse_ilGlobals).typ_uint16 }
   | UINT32
       { no_mspec_scope (!parse_ilGlobals).typ_uint32 }
   | UINT64
       { no_mspec_scope (!parse_ilGlobals).typ_uint64 }
   | NATIVE INT
       { no_mspec_scope (!parse_ilGlobals).typ_int }
   | NATIVE UNSIGNED INT
       { no_mspec_scope (!parse_ilGlobals).typ_uint }
   | NATIVE UINT
       { no_mspec_scope (!parse_ilGlobals).typ_uint }

   | BANG int32
       { no_mspec_scope (Type_tyvar (int_to_u16 (i32_to_int $2)))  }

   /* !!n is an alternative syntax for method type variables and is the syntax */
   /* used by the generic COM+ EE.  We adjust these to be count-from-outside indexes */
   /* into the type variables in scope. */
   | BANG BANG int32
       { ResolvedAtMethodSpecScope
           (fun cgparams -> Type_tyvar (int_to_u16 (i32_to_int $3 + List.length cgparams)))  }

bounds1:
     bound
       { [$1] }
   | bounds1 COMMA bound
       { $1 @ [$3] }

bound:
     /*EMPTY*/
       { (None, None) }
   | int32
       { (None, Some $1) }
   | int32 ELIPSES int32
       { (Some $1, Some (Int32.add (Int32.sub $3 $1) (Int32.of_int 1))) }
   | int32 ELIPSES
       { (Some $1, None) }
/* This is a complete hack to make up for the awful lexical structure of IL. */
/* We need to be able to parse all of */
/* ldc.r8     0. */
/* float64(-657435.)     */
/* and int32[0...,0...] */
/* The problem is telling an integer-followed-by-ellipses from a floating-point-nubmer-followed-by-dots */
   | VAL_INT32_ELIPSES int32
       { (Some $1, Some (Int32.add (Int32.sub $2 $1) (Int32.of_int 1))) }
   | VAL_INT32_ELIPSES
       { (Some $1, None) }

id:
     VAL_ID
       { $1 }
   | VAL_SQSTRING
       { $1 }

int32:
     VAL_INT64
       { i64_to_i32 $1 }

int64:
     VAL_INT64
       { $1 }

float64:
     VAL_FLOAT64
       { $1 }
   | FLOAT64 LPAREN int64 RPAREN
       { Nums.bits_to_ieee64 $3 }

opt_actual_tyargs:
      /* EMPTY */
        { no_mspec_scope mk_empty_gactuals }
   | actual_tyargs
        { resolve_mspec_scope_then $1 (fun res ->
          no_mspec_scope  res) }

actual_tyargs:
     LESS actualTypSpecs GREATER
        { $2 }

actualTypSpecs:
     typSpec
        { resolve_mspec_scope_then $1 (fun res ->
          no_mspec_scope [ res]) }
   | actualTypSpecs COMMA typSpec
        { resolve_mspec_scope_then $1 (fun x ->
          resolve_mspec_scope_then $3 (fun y ->
          no_mspec_scope (x @ [ y]))) }

