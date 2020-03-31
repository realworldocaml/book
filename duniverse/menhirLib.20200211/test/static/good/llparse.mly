%{

(* Parser for LLVM IR.  Needs to be kept up to date with the LLVM equivalent, see: *)
(*  https://github.com/llvm-mirror/llvm/commits/master/lib/AsmParser/LLParser.cpp  *)

type toplevel =
  | Fun of Llabs.finfo
  | Asm of string
  | Target of string
  | Datalayout of string
  | Deplibs of string list
  | Typ of Llabs.var * Llabs.typ option
  | Global of Llabs.ginfo
  | Alias of Llabs.ainfo
  | MDNodeDefn of Llabs.mdinfo
  | MDVarDefn of string * int list
  | ComdatVarDefn of string * Llabs.selectionkind
  | Attrgrp of int * Llabs.attribute list

let list_of_string s =
  if String.length s < 2 || String.get s 0 <> '"' || String.get s (String.length s - 1) <> '"' then
    failwith "list_of_string: expected quoted string constant";
  let l = ref [] in
  for i = (String.length s - 2) downto 1 do
    l := (String.get s i)::(!l)
  done;
  let is_hexdigit c =
    ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('A' <= c && c <= 'F') in
  let rec build = function
    | '\\'::x::y::tl when is_hexdigit x && is_hexdigit y ->
        (Scanf.sscanf (Printf.sprintf "%c%c" x y) "%x" (fun i -> i))::(build tl)
    | [] -> []
    | hd::tl -> (Char.code hd)::(build tl) in
  List.map
    (fun i -> (Llabs.Integer 8, Llabs.Int(Big_int.big_int_of_int i)))
    (build !l)

let process_toplevels t =
  let cu = {
    Llabs.ctarget=None;
    Llabs.cdatalayout=None;
    Llabs.casms=[];
    Llabs.cfuns=[];
    Llabs.ctyps=[];
    Llabs.cglobals=[];
    Llabs.caliases=[];
    Llabs.cmdnodes=[];
    Llabs.cmdvars=[];
    Llabs.cattrgrps=[];
  } in
  let proc = function
    | Fun fi -> cu.Llabs.cfuns <- fi::cu.Llabs.cfuns
    | Asm x -> cu.Llabs.casms <- x::cu.Llabs.casms
    | Target x ->
        if cu.Llabs.ctarget<>None then failwith "compilation unit with multiple targets"
        else cu.Llabs.ctarget <- Some x
    | Datalayout x ->
        if cu.Llabs.cdatalayout<>None then failwith "compilation unit with multiple datalayouts"
        else cu.Llabs.cdatalayout <- Some x
    | Deplibs _ -> () (* parses but ignored in LLVM 3.4, to be removed in 4.0 *)
    | Typ(x,y) -> cu.Llabs.ctyps <- (x,y)::cu.Llabs.ctyps
    | Global x -> cu.Llabs.cglobals <- x::cu.Llabs.cglobals
    | Alias x -> cu.Llabs.caliases <- x::cu.Llabs.caliases
    | MDNodeDefn x -> cu.Llabs.cmdnodes <- x::cu.Llabs.cmdnodes
    | MDVarDefn(x,y) -> cu.Llabs.cmdvars <- (x,y)::cu.Llabs.cmdvars
    | ComdatVarDefn(x,y) -> () (* ignored for now *)
    | Attrgrp(x,y) -> cu.Llabs.cattrgrps <- (x,y)::cu.Llabs.cattrgrps in
  List.iter proc (List.rev t);
  cu

%}
%token <string> APFloat
%token <string> APInt
%token <string> APSint
%token <int> AttrGrpID
%token Backslash
%token Comma
%token DotDotDot
%token Eof
%token Equal
%token Exclaim
%token <Llabs.var> GlobalVar
%token <Llabs.var> GlobalID
%token <Llabs.var> LocalVar
%token <Llabs.var> LocalVarID
%token Greater
%token <string> LabelStr
%token <string> ComdatVar
%token Lbrace
%token Less
%token Lparen
%token Lsquare
%token <string> MetadataVar
%token Rbrace
%token Rparen
%token Rsquare
%token Star
%token <string> StringConstant
%token Error
%token Kw_void
%token <int> I
%token Kw_half
%token Kw_float
%token Kw_double
%token Kw_x86_fp80
%token Kw_fp128
%token Kw_ppc_fp128
%token Kw_label
%token Kw_metadata
%token Kw_x86_mmx
%token Kw_true
%token Kw_false
%token Kw_declare
%token Kw_define
%token Kw_global
%token Kw_constant
%token Kw_private
%token Kw_internal
%token Kw_linker_private
%token Kw_linker_private_weak
%token Kw_available_externally
%token Kw_linkonce
%token Kw_linkonce_odr
%token Kw_weak
%token Kw_weak_odr
%token Kw_appending
%token Kw_dllimport
%token Kw_dllexport
%token Kw_common
%token Kw_default
%token Kw_hidden
%token Kw_protected
%token Kw_unnamed_addr
%token Kw_externally_initialized
%token Kw_extern_weak
%token Kw_external
%token Kw_thread_local
%token Kw_localdynamic
%token Kw_initialexec
%token Kw_localexec
%token Kw_zeroinitializer
%token Kw_undef
%token Kw_null
%token Kw_to
%token Kw_tail
%token Kw_musttail
%token Kw_target
%token Kw_triple
%token Kw_unwind
%token Kw_deplibs
%token Kw_datalayout
%token Kw_volatile
%token Kw_atomic
%token Kw_unordered
%token Kw_monotonic
%token Kw_acquire
%token Kw_release
%token Kw_acq_rel
%token Kw_seq_cst
%token Kw_singlethread
%token Kw_nnan
%token Kw_ninf
%token Kw_nsz
%token Kw_arcp
%token Kw_fast
%token Kw_nuw
%token Kw_nsw
%token Kw_exact
%token Kw_inbounds
%token Kw_align
%token Kw_addrspace
%token Kw_section
%token Kw_alias
%token Kw_module
%token Kw_asm
%token Kw_sideeffect
%token Kw_alignstack
%token Kw_inteldialect
%token Kw_gc
%token Kw_prefix
%token Kw_ccc
%token Kw_fastcc
%token Kw_coldcc
%token Kw_x86_stdcallcc
%token Kw_x86_fastcallcc
%token Kw_x86_thiscallcc
%token Kw_x86_cdeclmethodcc
%token Kw_arm_apcscc
%token Kw_arm_aapcscc
%token Kw_arm_aapcs_vfpcc
%token Kw_msp430_intrcc
%token Kw_ptx_kernel
%token Kw_ptx_device
%token Kw_spir_kernel
%token Kw_spir_func
%token Kw_intel_ocl_bicc
%token Kw_x86_64_sysvcc
%token Kw_x86_64_win64cc
%token Kw_webkit_jscc
%token Kw_anyregcc
%token Kw_preserve_mostcc
%token Kw_preserve_allcc
%token Kw_cc
%token Kw_c
%token Kw_attributes
%token Kw_alwaysinline
%token Kw_builtin
%token Kw_byval
%token Kw_dereferenceable
%token Kw_inalloca
%token Kw_cold
%token Kw_inlinehint
%token Kw_inreg
%token Kw_jumptable
%token Kw_minsize
%token Kw_naked
%token Kw_nest
%token Kw_noalias
%token Kw_nobuiltin
%token Kw_nocapture
%token Kw_noduplicate
%token Kw_noimplicitfloat
%token Kw_noinline
%token Kw_nonlazybind
%token Kw_nonnull
%token Kw_noredzone
%token Kw_noreturn
%token Kw_nounwind
%token Kw_optnone
%token Kw_optsize
%token Kw_readnone
%token Kw_readonly
%token Kw_returned
%token Kw_returns_twice
%token Kw_signext
%token Kw_sret
%token Kw_ssp
%token Kw_sspreq
%token Kw_sspstrong
%token Kw_sanitize_address
%token Kw_sanitize_thread
%token Kw_sanitize_memory
%token Kw_uwtable
%token Kw_zeroext
%token Kw_type
%token Kw_opaque
%token Kw_comdat;
%token Kw_any;
%token Kw_exactmatch;
%token Kw_largest;
%token Kw_noduplicates;
%token Kw_samesize;
%token Kw_eq
%token Kw_ne
%token Kw_slt
%token Kw_sgt
%token Kw_sle
%token Kw_sge
%token Kw_ult
%token Kw_ugt
%token Kw_ule
%token Kw_uge
%token Kw_oeq
%token Kw_one
%token Kw_olt
%token Kw_ogt
%token Kw_ole
%token Kw_oge
%token Kw_ord
%token Kw_uno
%token Kw_ueq
%token Kw_une
%token Kw_xchg
%token Kw_nand
%token Kw_max
%token Kw_min
%token Kw_umax
%token Kw_umin
%token Kw_x
%token Kw_blockaddress
%token Kw_personality
%token Kw_cleanup
%token Kw_catch
%token Kw_filter
%token Kw_add
%token Kw_fadd
%token Kw_sub
%token Kw_fsub
%token Kw_mul
%token Kw_fmul
%token Kw_udiv
%token Kw_sdiv
%token Kw_fdiv
%token Kw_urem
%token Kw_srem
%token Kw_frem
%token Kw_shl
%token Kw_lshr
%token Kw_ashr
%token Kw_and
%token Kw_or
%token Kw_xor
%token Kw_icmp
%token Kw_fcmp
%token Kw_phi
%token Kw_call
%token Kw_trunc
%token Kw_zext
%token Kw_sext
%token Kw_fptrunc
%token Kw_fpext
%token Kw_uitofp
%token Kw_sitofp
%token Kw_fptoui
%token Kw_fptosi
%token Kw_inttoptr
%token Kw_ptrtoint
%token Kw_bitcast
%token Kw_addrspacecast
%token Kw_select
%token Kw_va_arg
%token Kw_ret
%token Kw_br
%token Kw_switch
%token Kw_indirectbr
%token Kw_invoke
%token Kw_resume
%token Kw_unreachable
%token Kw_alloca
%token Kw_load
%token Kw_store
%token Kw_cmpxchg
%token Kw_atomicrmw
%token Kw_fence
%token Kw_getelementptr
%token Kw_extractelement
%token Kw_insertelement
%token Kw_shufflevector
%token Kw_extractvalue
%token Kw_insertvalue
%token Kw_landingpad
%start main
%type <Llabs.cunit> main
%%
main:
| toplevel_list { process_toplevels $1 }
;
toplevel_list:
| Eof           { [] }
| toplevel toplevel_list { $1::$2 }
;
toplevel:
| Kw_declare function_header                   { Fun $2 }
| Kw_define function_header function_body      { $2.Llabs.fblocks <- $3; Fun $2 }
| Kw_module Kw_asm StringConstant              { Asm $3 }
| Kw_target Kw_triple Equal StringConstant     { Target $4 }
| Kw_target Kw_datalayout Equal StringConstant { Datalayout $4}
| Kw_deplibs Equal Lsquare string_list Rsquare { Deplibs $4 }
| LocalVarID Equal Kw_type Kw_opaque           { Typ($1, None) }
| LocalVarID Equal Kw_type typ                 { Typ($1, Some $4) }
| LocalVar Equal Kw_type Kw_opaque             { Typ($1, None) }
| LocalVar Equal Kw_type typ                   { Typ($1, Some $4) }
| global_eq external_linkage opt_visibility opt_dll_storageclass opt_thread_local
    opt_addrspace opt_unnamed_addr opt_externally_initialized
    constant_or_global typ opt_section_align_comdat
                                               { Global {Llabs.gname = $1;
                                                         Llabs.glinkage = Some $2;
                                                         Llabs.gvisibility = $3;
                                                         Llabs.gstorageclass = $4;
                                                         Llabs.gthread_local = $5;
                                                         Llabs.gaddrspace = $6;
                                                         Llabs.gunnamed_addr = $7;
                                                         Llabs.gexternally_initialized = $8;
                                                         Llabs.gconstant = $9;
                                                         Llabs.gtyp = $10;
                                                         Llabs.gvalue = None;
                                                         Llabs.gsection = (match $11 with (x, _, _) -> x);
                                                         Llabs.galign = (match $11 with (_, x, _) -> x);
                                                         Llabs.gcomdat = (match $11 with (_, _, x) -> x);}
                                               }
| global_eq non_external_linkage opt_visibility opt_dll_storageclass opt_thread_local
    opt_addrspace opt_unnamed_addr opt_externally_initialized
    constant_or_global typ value opt_section_align_comdat
                                               { Global {Llabs.gname = $1;
                                                         Llabs.glinkage = $2;
                                                         Llabs.gvisibility = $3;
                                                         Llabs.gstorageclass = $4;
                                                         Llabs.gthread_local = $5;
                                                         Llabs.gaddrspace = $6;
                                                         Llabs.gunnamed_addr = $7;
                                                         Llabs.gexternally_initialized = $8;
                                                         Llabs.gconstant = $9;
                                                         Llabs.gtyp = $10;
                                                         Llabs.gvalue = Some $11;
                                                         Llabs.gsection = (match $12 with (x, _, _) -> x);
                                                         Llabs.galign = (match $12 with (_, x, _) -> x);
                                                         Llabs.gcomdat = (match $12 with (_, _, x) -> x);}
                                               }
| global_eq external_linkage opt_visibility opt_thread_local Kw_alias opt_linkage aliasee
    { Alias({Llabs.aname=$1; Llabs.avisibility=$3; Llabs.athread_local=$4; Llabs.alinkage=$6; Llabs.aaliasee=$7}) }
| global_eq non_external_linkage opt_visibility opt_thread_local Kw_alias opt_linkage aliasee
    { Alias({Llabs.aname=$1; Llabs.avisibility=$3; Llabs.athread_local=$4; Llabs.alinkage=$6; Llabs.aaliasee=$7}) }
| ComdatVar Equal Kw_comdat selection_kind
    { ComdatVarDefn($1, $4) }
| Exclaim APInt Equal Exclaim Lbrace mdnodevector Rbrace
    { MDNodeDefn({Llabs.mdid=int_of_string $2; Llabs.mdcontents=$6}) }
| MetadataVar Equal Exclaim Lbrace mdlist Rbrace                             { MDVarDefn($1, $5) }
| Kw_attributes AttrGrpID Equal Lbrace group_attributes Rbrace               { Attrgrp($2, $5) }
;
aliasee:
| type_value { (None, $1) }
| Kw_addrspace Lparen APInt Rparen typ Comma type_value { (Some(int_of_string $3, $5) , $7) }
;
selection_kind:
| Kw_any          { Llabs.SK_any }
| Kw_exactmatch   { Llabs.SK_exactmatch }
| Kw_largest      { Llabs.SK_largest }
| Kw_noduplicates { Llabs.SK_noduplicates }
| Kw_samesize     { Llabs.SK_samesize }
;
global_eq: /* may want to allow empty here (per llvm parser) but haven't seen it yet and it causes grammar conflicts */
| GlobalID Equal  { $1 }
| GlobalVar Equal { $1 }
;
string_list:
| /* empty */                { [] }
| StringConstant string_list { $1::$2 }
;
mdlist:
| /* empty */          { [] }
| Exclaim APInt mdlist { (int_of_string $2)::$3 }
;
mdnodevector:
| value                         { [$1] }
| typ value                     { [$2] }     /* This is obsolete but still supported in 3.6 :-( */
| value Comma mdnodevector      { ($1)::$3 }
| typ value Comma mdnodevector  { ($2)::$4 } /* This is obsolete but still supported in 3.6 :-( */
;
constant_or_global:
| Kw_constant { true }
| Kw_global   { false }
;
function_header:
| opt_linkage opt_visibility opt_dll_storageclass opt_callingconv return_attributes
 typ global_name argument_list opt_unnamed_addr function_attributes opt_section
 opt_comdat opt_align opt_gc opt_prefix
                             {
                               {Llabs.flinkage = $1;
                                Llabs.fvisibility = $2;
                                Llabs.fstorageclass = $3;
                                Llabs.fcallingconv = $4;
                                Llabs.freturnattrs = $5;
                                Llabs.freturntyp = $6;
                                Llabs.fname = $7;
                                Llabs.fparams = $8;
                                Llabs.funnamed_addr = $9;
                                Llabs.fattrs = $10;
                                Llabs.fsection = $11;
                                Llabs.fcomdat = $12;
                                Llabs.falign = $13;
                                Llabs.fgc = $14;
                                Llabs.fprefix = $15;
                                Llabs.fblocks = [];}
                             }
;
typ:
| Kw_void       { Llabs.Void }
| non_void_type { $1 }
;
non_void_type:
| I                                    { Llabs.Integer $1 }
| Kw_half                              { Llabs.Half }
| Kw_float                             { Llabs.Float }
| Kw_double                            { Llabs.Double }
| Kw_x86_fp80                          { Llabs.X86_fp80 }
| Kw_fp128                             { Llabs.Fp128 }
| Kw_ppc_fp128                         { Llabs.Ppc_fp128 }
| Kw_label                             { Llabs.Label }
| Kw_metadata                          { Llabs.Metadata }
| Kw_x86_mmx                           { Llabs.X86_mmx }
| LocalVar                             { Llabs.Vartyp($1) }
| LocalVarID                           { Llabs.Vartyp($1) }
| Lbrace Rbrace                        { Llabs.Structtyp(false, []) }
| Less Lbrace Rbrace Greater           { Llabs.Structtyp(true, []) }
| Lbrace type_list Rbrace              { Llabs.Structtyp(false, $2) }
| Less Lbrace type_list Rbrace Greater { Llabs.Structtyp(true, $3) }
| Lsquare APInt Kw_x typ Rsquare       { Llabs.Arraytyp(int_of_string $2, $4) }
| Less APInt Kw_x typ Greater          { Llabs.Vector(int_of_string $2, $4) }
| typ opt_addrspace Star               { Llabs.Pointer($1, $2) }
| typ argument_list                    { Llabs.Funtyp($1, fst $2, snd $2) }
;
type_list:
| typ                 { [$1] }
| typ Comma type_list { $1::$3 }
;
global_name:
| GlobalID  { $1 }
| GlobalVar { $1 }
;
argument_list:
Lparen arg_type_list Rparen { $2 }
;
arg_type_list:
| /* empty */                  { ([], false) }
| DotDotDot                    { ([], true)}
| arg_type                     { ([$1], false) }
| arg_type Comma arg_type_list { ($1::(fst $3), snd $3) }
;
arg_type:
| typ param_attribute_list          { ($1, $2, None) }
| typ param_attribute_list LocalVar { ($1, $2, Some $3) }
;
opt_section:
| /* empty */               { None }
| Kw_section StringConstant { Some $2 }
;
opt_align:
| /* empty */    { None }
| Kw_align APInt { Some(int_of_string $2) }
;
opt_inbounds:
| /* empty */ { false }
| Kw_inbounds { true }
;
opt_tail:
| /* empty */ { Llabs.TCK_None }
| Kw_tail     { Llabs.TCK_Tail }
| Kw_musttail { Llabs.TCK_MustTail }
;
opt_cleanup:
| /* empty */ { false }
| Kw_cleanup  { true }
;
opt_comdat:
| /* empty */         { None }
| Kw_comdat ComdatVar { Some $2 }
;
opt_section_align_comdat:
| /* empty */                                          { (None    , None                  , None) }
| Comma Kw_section StringConstant                      { (Some $3 , None                  , None) }
| Comma Kw_align APInt                                 { (None    , Some(int_of_string $3), None) }
| Comma Kw_section StringConstant Comma Kw_align APInt { (Some $3 , Some(int_of_string $6), None) }
| Comma Kw_comdat ComdatVar                            { (None    , None                  , Some $3) }
;
align_metadata:
| instruction_metadata { (None, $1) }
| Comma Kw_align APInt instruction_metadata { (Some(int_of_string $3), $4) }
;
opt_gc:
| /* empty */          { None }
| Kw_gc StringConstant { Some $2 }
;
opt_prefix:
| /* empty */         { None }
| Kw_prefix typ value { Some($2, $3) }
;
opt_atomic:
| /* empty */ { false }
| Kw_atomic   { true }
;
opt_weak:
| /* empty */ { false }
| Kw_weak     { true }
;
opt_volatile:
| /* empty */ { false }
| Kw_volatile { true }
;
value:
| GlobalID                                                                                  { Llabs.Var $1 }
| GlobalVar                                                                                 { Llabs.Var $1 }
| LocalVarID                                                                                { Llabs.Var $1 }
| LocalVar                                                                                  { Llabs.Var $1 }
| Exclaim mdvalue                                                                           { $2 }
| APInt                                                                                     { Llabs.Int(Big_int.big_int_of_string $1) }
| APFloat                                                                                   { Llabs.Float $1 }
| Kw_true                                                                                   { Llabs.True }
| Kw_false                                                                                  { Llabs.False }
| Kw_null                                                                                   { Llabs.Null }
| Kw_undef                                                                                  { Llabs.Undef }
| Kw_zeroinitializer                                                                        { Llabs.Zero }
| Lbrace type_value_list Rbrace                                                             { Llabs.Struct(false, $2) }
| Less Lbrace Rbrace Greater                                                                { Llabs.Struct(true, []) }
| Less Lbrace type_value_LIST Rbrace Greater                                                { Llabs.Struct(true, $3) }
| Less type_value_list Greater                                                              { Llabs.Vector($2) }
| Lsquare type_value_list Rsquare                                                           { Llabs.Array($2) }
| Kw_c StringConstant                                                                       { Llabs.Array(list_of_string $2) }
| Kw_asm opt_sideeffect opt_alignstack opt_inteldialect StringConstant Comma StringConstant { Llabs.Asm($2, $3, $4, $5, $7) }
| Kw_blockaddress               Lparen value Comma value Rparen                             { Llabs.Blockaddress($3, $5) }
| Kw_trunc                      Lparen type_value Kw_to typ Rparen                          { Llabs.Trunc         ($3, $5) }
| Kw_zext                       Lparen type_value Kw_to typ Rparen                          { Llabs.Zext          ($3, $5) }
| Kw_sext                       Lparen type_value Kw_to typ Rparen                          { Llabs.Sext          ($3, $5) }
| Kw_fptrunc                    Lparen type_value Kw_to typ Rparen                          { Llabs.Fptrunc       ($3, $5) }
| Kw_fpext                      Lparen type_value Kw_to typ Rparen                          { Llabs.Fpext         ($3, $5) }
| Kw_bitcast                    Lparen type_value Kw_to typ Rparen                          { Llabs.Bitcast       ($3, $5) }
| Kw_addrspacecast              Lparen type_value Kw_to typ Rparen                          { Llabs.Addrspacecast ($3, $5) }
| Kw_uitofp                     Lparen type_value Kw_to typ Rparen                          { Llabs.Uitofp        ($3, $5) }
| Kw_sitofp                     Lparen type_value Kw_to typ Rparen                          { Llabs.Sitofp        ($3, $5) }
| Kw_fptoui                     Lparen type_value Kw_to typ Rparen                          { Llabs.Fptoui        ($3, $5) }
| Kw_fptosi                     Lparen type_value Kw_to typ Rparen                          { Llabs.Fptosi        ($3, $5) }
| Kw_inttoptr                   Lparen type_value Kw_to typ Rparen                          { Llabs.Inttoptr      ($3, $5) }
| Kw_ptrtoint                   Lparen type_value Kw_to typ Rparen                          { Llabs.Ptrtoint      ($3, $5) }
| Kw_extractvalue               Lparen type_value index_list Rparen                         { Llabs.Extractvalue($3, $4) }
| Kw_insertvalue                Lparen type_value Comma type_value index_list Rparen        { Llabs.Insertvalue($3, $5, $6) }
| Kw_icmp icmp_predicate        Lparen type_value Comma type_value Rparen                   { Llabs.Icmp($2, $4, $6) }
| Kw_fcmp fcmp_predicate        Lparen type_value Comma type_value Rparen                   { Llabs.Fcmp($2, $4, $6) }
| Kw_add opt_nuw_nsw            Lparen type_value Comma type_value Rparen                   { Llabs.Add(fst $2, snd $2, $4, $6) }
| Kw_sub opt_nuw_nsw            Lparen type_value Comma type_value Rparen                   { Llabs.Sub(fst $2, snd $2, $4, $6) }
| Kw_mul opt_nuw_nsw            Lparen type_value Comma type_value Rparen                   { Llabs.Mul(fst $2, snd $2, $4, $6) }
| Kw_shl opt_nuw_nsw            Lparen type_value Comma type_value Rparen                   { Llabs.Shl(fst $2, snd $2, $4, $6) }
| Kw_sdiv opt_exact             Lparen type_value Comma type_value Rparen                   { Llabs.Sdiv($2, $4, $6) }
| Kw_udiv opt_exact             Lparen type_value Comma type_value Rparen                   { Llabs.Udiv($2, $4, $6) }
| Kw_lshr opt_exact             Lparen type_value Comma type_value Rparen                   { Llabs.Lshr($2, $4, $6) }
| Kw_ashr opt_exact             Lparen type_value Comma type_value Rparen                   { Llabs.Ashr($2, $4, $6) }
| Kw_fadd                       Lparen type_value Comma type_value Rparen                   { Llabs.Fadd($3, $5) }
| Kw_fsub                       Lparen type_value Comma type_value Rparen                   { Llabs.Fsub($3, $5) }
| Kw_fmul                       Lparen type_value Comma type_value Rparen                   { Llabs.Fmul($3, $5) }
| Kw_fdiv                       Lparen type_value Comma type_value Rparen                   { Llabs.Fdiv($3, $5) }
| Kw_urem                       Lparen type_value Comma type_value Rparen                   { Llabs.Urem($3, $5) }
| Kw_srem                       Lparen type_value Comma type_value Rparen                   { Llabs.Srem($3, $5) }
| Kw_frem                       Lparen type_value Comma type_value Rparen                   { Llabs.Frem($3, $5) }
| Kw_and                        Lparen type_value Comma type_value Rparen                   { Llabs.And($3, $5) }
| Kw_or                         Lparen type_value Comma type_value Rparen                   { Llabs.Or($3, $5) }
| Kw_xor                        Lparen type_value Comma type_value Rparen                   { Llabs.Xor($3, $5) }
| Kw_getelementptr opt_inbounds Lparen type_value_list Rparen                               { Llabs.Getelementptr($2, $4) }
| Kw_shufflevector              Lparen type_value_list Rparen                               { Llabs.Shufflevector  $3 }
| Kw_insertelement              Lparen type_value_list Rparen                               { Llabs.Insertelement  $3 }
| Kw_extractelement             Lparen type_value_list Rparen                               { Llabs.Extractelement $3 }
| Kw_select                     Lparen type_value_list Rparen                               { Llabs.Select         $3 }
;
mdvalue:
| APInt                      { Llabs.Mdnode(int_of_string $1) }
| StringConstant             { Llabs.Mdstring $1 }
| Lbrace mdnodevector Rbrace { Llabs.Mdnodevector $2 }
;
type_value_LIST_metadata:
| type_value instruction_metadata           { ([$1], $2) }
| type_value Comma type_value_LIST_metadata { ($1::(fst $3), snd $3) }
;
type_value_LIST:
| type_value                       { [$1] }
| type_value Comma type_value_LIST { $1::$3 }
;
type_value_list:
| /* empty */                      { [] }
| type_value                       { [$1] }
| type_value Comma type_value_list { $1::$3 }
;
index_list:
| Comma APInt            { [(int_of_string $2)] }
| Comma APInt index_list { (int_of_string $2)::$3 }
;
index_list_metadata:
| Comma APInt            instruction_metadata { [(int_of_string $2)], $3 }
| Comma APInt index_list_metadata             { (int_of_string $2)::(fst $3), snd $3 }
;
fcmp_predicate:
| Kw_oeq   { Llabs.F.Oeq   }
| Kw_one   { Llabs.F.One   }
| Kw_olt   { Llabs.F.Olt   }
| Kw_ogt   { Llabs.F.Ogt   }
| Kw_ole   { Llabs.F.Ole   }
| Kw_oge   { Llabs.F.Oge   }
| Kw_ord   { Llabs.F.Ord   }
| Kw_uno   { Llabs.F.Uno   }
| Kw_ueq   { Llabs.F.Ueq   }
| Kw_une   { Llabs.F.Une   }
| Kw_ult   { Llabs.F.Ult   }
| Kw_ugt   { Llabs.F.Ugt   }
| Kw_ule   { Llabs.F.Ule   }
| Kw_uge   { Llabs.F.Uge   }
| Kw_true  { Llabs.F.True  }
| Kw_false { Llabs.F.False }
;
icmp_predicate:
| Kw_eq  { Llabs.I.Eq  }
| Kw_ne  { Llabs.I.Ne  }
| Kw_slt { Llabs.I.Slt }
| Kw_sgt { Llabs.I.Sgt }
| Kw_sle { Llabs.I.Sle }
| Kw_sge { Llabs.I.Sge }
| Kw_ult { Llabs.I.Ult }
| Kw_ugt { Llabs.I.Ugt }
| Kw_ule { Llabs.I.Ule }
| Kw_uge { Llabs.I.Uge }
;
function_body:
| Lbrace basicblock_list Rbrace { $2 }
;
basicblock_list:
| basicblock                 { [$1] }
| basicblock basicblock_list { $1::$2 }
;
basicblock:
| LabelStr instruction_list { {Llabs.bname=Llabs.Name(false, $1); Llabs.binstrs=$2} }
| instruction_list          { {Llabs.bname=Llabs.Id(false, -1); Llabs.binstrs=$1} }
;
instruction_list:
| terminator_instruction { [$1] }
| instruction instruction_list { $1::$2 }
;
instruction_metadata:
| /* empty */ { [] }
| Comma MetadataVar Exclaim APInt instruction_metadata { ($2,Llabs.Mdnode(int_of_string $4))::$5 }
| Comma MetadataVar Exclaim Lbrace mdnodevector Rbrace instruction_metadata { ($2,Llabs.Mdnodevector $5)::$7 }
;
local_eq:
| LocalVarID Equal { $1 }
| LocalVar Equal   { $1 }
;
opt_local:
| /* empty */ { None }
| local_eq    { Some $1 }
;
instruction:
| local_eq Kw_add opt_nuw_nsw type_value Comma value      instruction_metadata { Some $1, Llabs.Add(fst $3, snd $3, $4, $6, $7) }
| local_eq Kw_sub opt_nuw_nsw type_value Comma value      instruction_metadata { Some $1, Llabs.Sub(fst $3, snd $3, $4, $6, $7) }
| local_eq Kw_mul opt_nuw_nsw type_value Comma value      instruction_metadata { Some $1, Llabs.Mul(fst $3, snd $3, $4, $6, $7) }
| local_eq Kw_shl opt_nuw_nsw type_value Comma value      instruction_metadata { Some $1, Llabs.Shl(fst $3, snd $3, $4, $6, $7) }
| local_eq Kw_fadd fast_math_flags type_value Comma value instruction_metadata { Some $1, Llabs.Fadd($3, $4, $6, $7) }
| local_eq Kw_fsub fast_math_flags type_value Comma value instruction_metadata { Some $1, Llabs.Fsub($3, $4, $6, $7) }
| local_eq Kw_fmul fast_math_flags type_value Comma value instruction_metadata { Some $1, Llabs.Fmul($3, $4, $6, $7) }
| local_eq Kw_fdiv fast_math_flags type_value Comma value instruction_metadata { Some $1, Llabs.Fdiv($3, $4, $6, $7) }
| local_eq Kw_frem fast_math_flags type_value Comma value instruction_metadata { Some $1, Llabs.Frem($3, $4, $6, $7) }
| local_eq Kw_sdiv opt_exact type_value Comma value       instruction_metadata { Some $1, Llabs.Sdiv($3, $4, $6, $7) }
| local_eq Kw_udiv opt_exact type_value Comma value       instruction_metadata { Some $1, Llabs.Udiv($3, $4, $6, $7) }
| local_eq Kw_lshr opt_exact type_value Comma value       instruction_metadata { Some $1, Llabs.Lshr($3, $4, $6, $7) }
| local_eq Kw_ashr opt_exact type_value Comma value       instruction_metadata { Some $1, Llabs.Ashr($3, $4, $6, $7) }
| local_eq Kw_urem type_value Comma value                 instruction_metadata { Some $1, Llabs.Urem($3, $5, $6) }
| local_eq Kw_srem type_value Comma value                 instruction_metadata { Some $1, Llabs.Srem($3, $5, $6) }
| local_eq Kw_and type_value Comma value                  instruction_metadata { Some $1, Llabs.And($3, $5, $6) }
| local_eq Kw_or type_value Comma value                   instruction_metadata { Some $1, Llabs.Or($3, $5, $6) }
| local_eq Kw_xor type_value Comma value                  instruction_metadata { Some $1, Llabs.Xor($3, $5, $6) }
| local_eq Kw_icmp icmp_predicate type_value Comma value  instruction_metadata { Some $1, Llabs.Icmp($3, $4, $6, $7) }
| local_eq Kw_fcmp fcmp_predicate type_value Comma value  instruction_metadata { Some $1, Llabs.Fcmp($3, $4, $6, $7) }
| local_eq Kw_trunc type_value Kw_to typ                  instruction_metadata { Some $1, Llabs.Trunc($3, $5, $6) }
| local_eq Kw_zext type_value Kw_to typ                   instruction_metadata { Some $1, Llabs.Zext($3, $5, $6) }
| local_eq Kw_sext type_value Kw_to typ                   instruction_metadata { Some $1, Llabs.Sext($3, $5, $6) }
| local_eq Kw_fptrunc type_value Kw_to typ                instruction_metadata { Some $1, Llabs.Fptrunc($3, $5, $6) }
| local_eq Kw_fpext type_value Kw_to typ                  instruction_metadata { Some $1, Llabs.Fpext($3, $5, $6) }
| local_eq Kw_bitcast type_value Kw_to typ                instruction_metadata { Some $1, Llabs.Bitcast($3, $5, $6) }
| local_eq Kw_addrspacecast type_value Kw_to typ          instruction_metadata { Some $1, Llabs.Addrspacecast($3, $5, $6) }
| local_eq Kw_uitofp type_value Kw_to typ                 instruction_metadata { Some $1, Llabs.Uitofp($3, $5, $6) }
| local_eq Kw_sitofp type_value Kw_to typ                 instruction_metadata { Some $1, Llabs.Sitofp($3, $5, $6) }
| local_eq Kw_fptoui type_value Kw_to typ                 instruction_metadata { Some $1, Llabs.Fptoui($3, $5, $6) }
| local_eq Kw_fptosi type_value Kw_to typ                 instruction_metadata { Some $1, Llabs.Fptosi($3, $5, $6) }
| local_eq Kw_inttoptr type_value Kw_to typ               instruction_metadata { Some $1, Llabs.Inttoptr($3, $5, $6) }
| local_eq Kw_ptrtoint type_value Kw_to typ               instruction_metadata { Some $1, Llabs.Ptrtoint($3, $5, $6) }
| local_eq Kw_va_arg type_value Comma typ                 instruction_metadata { Some $1, Llabs.Va_arg($3, $5, $6) }
| local_eq Kw_getelementptr opt_inbounds type_value_LIST_metadata              { Some $1, Llabs.Getelementptr($3, fst $4, snd $4) }
| local_eq Kw_extractelement type_value_LIST_metadata                          { Some $1, Llabs.Extractelement(fst $3, snd $3) }
| local_eq Kw_insertelement type_value_LIST_metadata                           { Some $1, Llabs.Insertelement(fst $3, snd $3) }
| local_eq Kw_shufflevector type_value_LIST_metadata                           { Some $1, Llabs.Shufflevector(fst $3, snd $3) }
| local_eq Kw_select type_value_LIST_metadata                                  { Some $1, Llabs.Select(fst $3, snd $3) }
| local_eq Kw_phi typ phi_list_metadata                                        { Some $1, Llabs.Phi($3, fst $4, snd $4) }
| local_eq Kw_landingpad typ Kw_personality type_value opt_cleanup landingpad_list
                                                          instruction_metadata { Some $1, Llabs.Landingpad($3, $5, $6, $7, $8) }
| opt_local opt_tail Kw_call opt_callingconv return_attributes typ value Lparen param_list Rparen call_attributes
                                                          instruction_metadata { $1, Llabs.Call($2, $4, $5, $6, $7, $9, $11, $12) }
| local_eq Kw_alloca alloc_metadata                                            { Some $1, $3 }
| local_eq Kw_load opt_atomic opt_volatile type_value scopeandordering
                                                                align_metadata { Some $1, Llabs.Load($3, $4, $5, $6, fst $7, snd $7) }
| Kw_store opt_atomic opt_volatile type_value Comma type_value scopeandordering
                                                                align_metadata { None, Llabs.Store($2, $3, $4, $6, $7, fst $8, snd $8) }
| Kw_cmpxchg opt_weak opt_volatile type_value Comma type_value Comma type_value opt_singlethread ordering ordering
                                                          instruction_metadata { None, Llabs.Cmpxchg($2, $3, $4, $6, $8, $9, $10, $11, $12) }
| Kw_atomicrmw opt_volatile binop type_value Comma type_value opt_singlethread ordering
                                                          instruction_metadata { None, Llabs.Atomicrmw($2, $3, $4, $6, $7, $8, $9) }
| Kw_fence opt_singlethread ordering                      instruction_metadata { None, Llabs.Fence($2, $3, $4) }
| local_eq Kw_extractvalue type_value index_list_metadata                      { Some $1, Llabs.Extractvalue($3, fst $4, snd $4) }
| local_eq Kw_insertvalue type_value Comma type_value index_list_metadata      { Some $1, Llabs.Insertvalue($3, $5, fst $6, snd $6) }
;
binop:
| Kw_xchg { Llabs.Xchg }
| Kw_add  { Llabs.Add  }
| Kw_sub  { Llabs.Sub  }
| Kw_and  { Llabs.And  }
| Kw_nand { Llabs.Nand }
| Kw_or   { Llabs.Or   }
| Kw_xor  { Llabs.Xor  }
| Kw_max  { Llabs.Max  }
| Kw_min  { Llabs.Min  }
| Kw_umax { Llabs.Umax }
| Kw_umin { Llabs.Umin }
;
phi_list_metadata:
| Lsquare value Comma value Rsquare instruction_metadata    { [($2, $4)], $6 }
| Lsquare value Comma value Rsquare Comma phi_list_metadata { ($2, $4)::(fst $7), snd $7 }
;
landingpad_list:
| Kw_catch typ value                  { [Llabs.Catch($2, $3)] }
| Kw_filter typ value                 { [Llabs.Filter($2, $3)] }
| Kw_catch typ value landingpad_list  { (Llabs.Catch($2, $3))::$4 }
| Kw_filter typ value landingpad_list { (Llabs.Filter($2, $3))::$4 }
;
ordering:
| Kw_unordered { Llabs.Unordered }
| Kw_monotonic { Llabs.Monotonic }
| Kw_acquire   { Llabs.Acquire   }
| Kw_release   { Llabs.Release   }
| Kw_acq_rel   { Llabs.Acq_rel   }
| Kw_seq_cst   { Llabs.Seq_cst   }
;
opt_singlethread:
| /* empty */               { false }
| Kw_singlethread           { true }
scopeandordering:
| /* empty */               { None }
| opt_singlethread ordering { Some($1, $2) }
;
alloc_metadata:
| Kw_inalloca typ Comma type_value Comma Kw_align APInt instruction_metadata { Llabs.Alloca(true,  $2, Some $4, Some(int_of_string $7), $8) }
| Kw_inalloca typ Comma type_value                      instruction_metadata { Llabs.Alloca(true,  $2, Some $4, None, $5) }
| Kw_inalloca typ Comma Kw_align APInt                  instruction_metadata { Llabs.Alloca(true,  $2, None, Some(int_of_string $5), $6) }
| Kw_inalloca typ                                       instruction_metadata { Llabs.Alloca(true,  $2, None, None, $3) }
| typ Comma type_value Comma Kw_align APInt             instruction_metadata { Llabs.Alloca(false, $1, Some $3, Some(int_of_string $6), $7) }
| typ Comma type_value                                  instruction_metadata { Llabs.Alloca(false, $1, Some $3, None, $4) }
| typ Comma Kw_align APInt                              instruction_metadata { Llabs.Alloca(false, $1, None, Some(int_of_string $4), $5) }
| typ                                                   instruction_metadata { Llabs.Alloca(false, $1, None, None, $2) }
;
fast_math_flags:
| /* empty */                    { [] }
| fast_math_flag fast_math_flags { $1::$2 }
;
fast_math_flag:
| Kw_fast { Llabs.Fast }
| Kw_nnan { Llabs.Nnan }
| Kw_ninf { Llabs.Ninf }
| Kw_nsz  { Llabs.Nsz  }
| Kw_arcp { Llabs.Arcp }
;
terminator_instruction:
| Kw_unreachable                                                   instruction_metadata { None, Llabs.Unreachable $2 }
| Kw_ret Kw_void                                                   instruction_metadata { None, Llabs.Return(None,$3) } /* we need to distinguish void from all other types else we have a dependent grammar */
| Kw_ret non_void_type value                                       instruction_metadata { None, Llabs.Return(Some($2, $3),$4) }
| Kw_br type_value                                                 instruction_metadata { None, Llabs.Br($2, None, $3) }
| Kw_br type_value Comma type_value Comma type_value               instruction_metadata { None, Llabs.Br($2, Some($4, $6), $7) }
| Kw_indirectbr type_value Comma Lsquare type_value_LIST Rsquare   instruction_metadata { None, Llabs.Indirectbr($2, $5, $7) }
| Kw_resume type_value                                             instruction_metadata { None, Llabs.Resume($2, $3) }
| Kw_switch type_value Comma type_value Lsquare jump_table Rsquare instruction_metadata { None, Llabs.Switch($2, $4, $6, $8) }
| local_eq Kw_invoke opt_callingconv return_attributes typ value Lparen param_list Rparen function_attributes Kw_to type_value Kw_unwind type_value instruction_metadata { Some $1, Llabs.Invoke($3, $4, $5, $6, $8, $10, $12, $14, $15) }
;
call_attributes:
| /* empty */                    { [] }
| call_attribute call_attributes { $1::$2 }
;
call_attribute:
| AttrGrpID   { Llabs.Attrgrp($1) }
| Kw_noreturn { Llabs.Noreturn }
| Kw_nounwind { Llabs.Nounwind }
| Kw_readnone { Llabs.Readnone }
| Kw_readonly { Llabs.Readonly }
;
function_attributes:
| /* empty */                            { [] }
| function_attribute function_attributes { $1::$2 }
;
function_attribute:
| AttrGrpID                                { Llabs.Attrgrp($1) }
| StringConstant Equal StringConstant      { Llabs.Attr($1, Some $3) }
| Kw_alignstack Equal Lparen APInt Rparen  { Llabs.Alignstack(int_of_string $4) }
| Kw_alwaysinline                          { Llabs.Alwaysinline     }
| Kw_builtin                               { Llabs.Builtin          }
| Kw_cold                                  { Llabs.Cold             }
| Kw_inlinehint                            { Llabs.Inlinehint       }
| Kw_jumptable                             { Llabs.Jumptable        }
| Kw_minsize                               { Llabs.Minsize          }
| Kw_naked                                 { Llabs.Naked            }
| Kw_nobuiltin                             { Llabs.Nobuiltin        }
| Kw_noduplicate                           { Llabs.Noduplicate      }
| Kw_noimplicitfloat                       { Llabs.Noimplicitfloat  }
| Kw_noinline                              { Llabs.Noinline         }
| Kw_nonlazybind                           { Llabs.Nonlazybind      }
| Kw_noredzone                             { Llabs.Noredzone        }
| Kw_noreturn                              { Llabs.Noreturn         }
| Kw_nounwind                              { Llabs.Nounwind         }
| Kw_optnone                               { Llabs.Optnone          }
| Kw_optsize                               { Llabs.Optsize          }
| Kw_readnone                              { Llabs.Readnone         }
| Kw_readonly                              { Llabs.Readonly         }
| Kw_returns_twice                         { Llabs.Returns_twice    }
| Kw_ssp                                   { Llabs.Ssp              }
| Kw_sspreq                                { Llabs.Sspreq           }
| Kw_sspstrong                             { Llabs.Sspstrong        }
| Kw_sanitize_address                      { Llabs.Sanitize_address }
| Kw_sanitize_thread                       { Llabs.Sanitize_thread  }
| Kw_sanitize_memory                       { Llabs.Sanitize_memory  }
| Kw_uwtable                               { Llabs.Uwtable          }
;
group_attributes:
| /* empty */                      { [] }
| group_attribute group_attributes { $1::$2 }
;
group_attribute:
| StringConstant                      { Llabs.Attr($1, None) }
| StringConstant Equal StringConstant { Llabs.Attr($1, Some $3) }
| Kw_align Equal APInt                { Llabs.Align(int_of_string $3) }
| Kw_alignstack Equal APInt           { Llabs.Alignstack(int_of_string $3) }
| Kw_alwaysinline                     { Llabs.Alwaysinline    }
| Kw_builtin                          { Llabs.Builtin         }
| Kw_cold                             { Llabs.Cold            }
| Kw_inlinehint                       { Llabs.Inlinehint      }
| Kw_jumptable                        { Llabs.Jumptable        }
| Kw_minsize                          { Llabs.Minsize         }
| Kw_naked                            { Llabs.Naked           }
| Kw_nobuiltin                        { Llabs.Nobuiltin       }
| Kw_noduplicate                      { Llabs.Noduplicate     }
| Kw_noimplicitfloat                  { Llabs.Noimplicitfloat }
| Kw_noinline                         { Llabs.Noinline        }
| Kw_nonlazybind                      { Llabs.Nonlazybind     }
| Kw_noredzone                        { Llabs.Noredzone       }
| Kw_noreturn                         { Llabs.Noreturn        }
| Kw_nounwind                         { Llabs.Nounwind        }
| Kw_optnone                          { Llabs.Optnone         }
| Kw_optsize                          { Llabs.Optsize         }
| Kw_readnone                         { Llabs.Readnone        }
| Kw_readonly                         { Llabs.Readonly        }
| Kw_returns_twice                    { Llabs.Returns_twice   }
| Kw_ssp                              { Llabs.Ssp             }
| Kw_sspreq                           { Llabs.Sspreq          }
| Kw_sspstrong                        { Llabs.Sspstrong       }
| Kw_sanitize_address                 { Llabs.Sanitize_address}
| Kw_sanitize_thread                  { Llabs.Sanitize_thread }
| Kw_sanitize_memory                  { Llabs.Sanitize_memory }
| Kw_uwtable                          { Llabs.Uwtable         }
;
param_list:
| /* empty */            { [] }
| param                  { [$1] }
| param Comma param_list { $1::$3 }
;
param:
| typ param_attribute_list value { ($1, $2, $3) }
;
param_attribute_list:
| /* empty */                          { [] }
| param_attribute param_attribute_list { $1::$2 }
param_attribute:
| Kw_align APInt                         { Llabs.Align(int_of_string $2)           }
| Kw_byval                               { Llabs.Byval                             }
| Kw_dereferenceable Lparen APInt Rparen { Llabs.Dereferenceable(int_of_string $3) }
| Kw_inalloca                            { Llabs.Inalloca                          }
| Kw_inreg                               { Llabs.Inreg                             }
| Kw_nest                                { Llabs.Nest                              }
| Kw_noalias                             { Llabs.Noalias                           }
| Kw_nocapture                           { Llabs.Nocapture                         }
| Kw_nonnull                             { Llabs.Nonnull                           }
| Kw_readnone                            { Llabs.Readnone                          }
| Kw_readonly                            { Llabs.Readonly                          }
| Kw_returned                            { Llabs.Returned                          }
| Kw_signext                             { Llabs.Signext                           }
| Kw_sret                                { Llabs.Sret                              }
| Kw_zeroext                             { Llabs.Zeroext                           }
;
jump_table:
| /* empty */                            { [] }
| type_value Comma type_value jump_table { ($1,$3)::$4 }
;
type_value:
| typ value { ($1, $2) }
;
opt_sideeffect:
| /* empty */   { false }
| Kw_sideeffect { true }
;
opt_alignstack:
| /* empty */   { false }
| Kw_alignstack { true }
;
opt_inteldialect:
| /* empty */     { false }
| Kw_inteldialect { true }
;
opt_exact:
| /* empty */ { false }
| Kw_exact    { true }
;
opt_nuw_nsw:
| /* empty */   { (false, false) }
| Kw_nuw Kw_nsw { (true, true)   }
| Kw_nsw Kw_nuw { (true, true)   }
| Kw_nuw        { (true, false)  }
| Kw_nsw        { (false, true)  }
;
opt_thread_local:
| /* empty */                                   { None }
| Kw_thread_local                               { Some None }
| Kw_thread_local Lparen Kw_localdynamic Rparen { Some (Some Llabs.Localdynamic) }
| Kw_thread_local Lparen Kw_initialexec Rparen  { Some (Some Llabs.Initialexec) }
| Kw_thread_local Lparen Kw_localexec Rparen    { Some (Some Llabs.Localexec) }
;
opt_addrspace:
| /* empty */                      { None }
| Kw_addrspace Lparen APInt Rparen { Some (int_of_string $3) }
;
opt_unnamed_addr:
| /* empty */     { false }
| Kw_unnamed_addr { true }
;
opt_externally_initialized:
| /* empty */               { false }
| Kw_externally_initialized { true }
;
opt_dll_storageclass:
| /* empty */  { None }
| Kw_dllimport { Some Llabs.Dllimport }
| Kw_dllexport { Some Llabs.Dllexport }
;
opt_linkage:
| external_linkage     { Some $1 }
| non_external_linkage { $1 }
;
external_linkage:
| Kw_extern_weak { Llabs.Extern_weak }
| Kw_external    { Llabs.External }
;
non_external_linkage:
| /* empty */             { None }
| Kw_private              { Some Llabs.Private }
| Kw_internal             { Some Llabs.Internal }
| Kw_linker_private       { Some Llabs.Linker_private }
| Kw_linker_private_weak  { Some Llabs.Linker_private_weak }
| Kw_weak                 { Some Llabs.Weak }
| Kw_weak_odr             { Some Llabs.Weak_odr }
| Kw_linkonce             { Some Llabs.Linkonce }
| Kw_linkonce_odr         { Some Llabs.Linkonce_odr }
| Kw_available_externally { Some Llabs.Available_externally }
| Kw_appending            { Some Llabs.Appending }
| Kw_common               { Some Llabs.Common }
;
opt_visibility:
| /* empty */  { None }
| Kw_default   { Some Llabs.Default }
| Kw_hidden    { Some Llabs.Hidden }
| Kw_protected { Some Llabs.Protected }
;
opt_callingconv:
| /* empty */          { None }
| Kw_ccc               { Some Llabs.Ccc               }
| Kw_fastcc            { Some Llabs.Fastcc            }
| Kw_intel_ocl_bicc    { Some Llabs.Intel_ocl_bicc    }
| Kw_coldcc            { Some Llabs.Coldcc            }
| Kw_x86_stdcallcc     { Some Llabs.X86_stdcallcc     }
| Kw_x86_fastcallcc    { Some Llabs.X86_fastcallcc    }
| Kw_x86_thiscallcc    { Some Llabs.X86_thiscallcc    }
| Kw_x86_cdeclmethodcc { Some Llabs.X86_cdeclmethodcc }
| Kw_arm_apcscc        { Some Llabs.Arm_apcscc        }
| Kw_arm_aapcscc       { Some Llabs.Arm_aapcscc       }
| Kw_arm_aapcs_vfpcc   { Some Llabs.Arm_aapcs_vfpcc   }
| Kw_msp430_intrcc     { Some Llabs.Msp430_intrcc     }
| Kw_ptx_kernel        { Some Llabs.Ptx_kernel        }
| Kw_ptx_device        { Some Llabs.Ptx_device        }
| Kw_spir_func         { Some Llabs.Spir_func         }
| Kw_spir_kernel       { Some Llabs.Spir_kernel       }
| Kw_x86_64_sysvcc     { Some Llabs.X86_64_sysvcc     }
| Kw_x86_64_win64cc    { Some Llabs.X86_64_win64cc    }
| Kw_webkit_jscc       { Some Llabs.Webkit_jscc       }
| Kw_anyregcc          { Some Llabs.Anyregcc          }
| Kw_preserve_mostcc   { Some Llabs.Preserve_mostcc   }
| Kw_preserve_allcc    { Some Llabs.Preserve_allcc    }
| Kw_cc                { Some Llabs.Cc                }
;
return_attributes:
| /* empty */                        { [] }
| return_attribute return_attributes { $1::$2 }
;
return_attribute:
| Kw_dereferenceable Lparen APInt Rparen { Llabs.Dereferenceable(int_of_string $3) }
| Kw_inreg                               { Llabs.Inreg                             }
| Kw_noalias                             { Llabs.Noalias                           }
| Kw_nonnull                             { Llabs.Nonnull                           }
| Kw_signext                             { Llabs.Signext                           }
| Kw_zeroext                             { Llabs.Zeroext                           }
;
