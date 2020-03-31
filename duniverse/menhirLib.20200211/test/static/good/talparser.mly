/**********************************************************************
 * (c) Greg Morrisett, Neal Glew, David Walker                        *
 *     June 1998, all rights reserved.                                *
 **********************************************************************/

/* talparser.mly
 *
 * Basic TAL parser
 *
 */

%{
open Utilities;;
open Numtypes;;
open Identifier;;
open Tal;;

let err s =
  Gcdfec.post_error (Gcdfec.mk_err_parse_symbol s)
;;

let parse_error s = err s;;

let errn n s =
  Gcdfec.post_error (Gcdfec.mk_err_parse_rhs n s)
;;

let dword_error n =
  errn n "operand requires dword ptr"
;;

let ck_dword_l ptr_opt n op =
  match ptr_opt with
    None ->
      (match op with
    	Prjl(_,_,_) -> dword_error n
      | _ -> ())
  | Some RPl -> dword_error n
  | Some RPe -> ()
  | _ -> ()
;;

let ck_dword_lr ptr_opt n op =
  match ptr_opt with
    None ->
      (match op with
    	Prjr(_,_,_) | Prjl(_,_,_) -> dword_error n
      | _ -> ())
  | Some RPl -> dword_error n
  | Some RPe -> ()
  | _ -> ()
;;

let ck_dword_b ptr_opt op1 op2 =
  match ptr_opt with
    None ->
      (match op1 with
      	Prjr(_,_,_) | Prjl(_,_,_) ->
      	  (match op2 with
	    Immed _ -> err "dword ptr required"
	  |	_ -> ())
      | _ -> ())
  | Some RPl -> err "dword ptr required"
  | Some RPe -> ()
  | _ -> ()
;;

(* Dave/Dan: The part for genop g,
 * if g is prefixed by a dword ptr, then the size should be 32 bits.
 * if g is prefixed by a byte ptr, then the size should be 8 bits.
 * if g is a 32 bit register or immediate address, the size should be 32 bits.
 * otherwise take the size from the second operand.
 *)
let get_part ptr_opt g g2_part =
  match ptr_opt with
    Some p -> p
  | None   ->
      (match g with
      	Reg  _ -> RPe
      | Addr _ -> RPe
      | _      -> g2_part)
;;

let mk_label_coerce n (o,cs) =
  match o with
    Addr l -> (l,cs)
  | _ -> errn n "bad label coerce"; raise Gcdfec.Exit
;;

let mk_reg_coerce n (o,cs) =
  match o with
    Reg r -> (r,cs)
  | _ -> errn n "bad register coerce"; raise Gcdfec.Exit
;;

let mk_reg n o =
  match o with
    Reg r -> r
  | _ -> errn n "operand must be a register"; raise Gcdfec.Exit
;;

let mk_scale n =
  if n =$ i32_1 then Byte1
  else if n =$ i32_2 then Byte2
  else if n =$ i32_4 then Byte4
  else if n =$ i32_8 then Byte8
  else (err "bad scale"; raise Gcdfec.Exit)
;;

let mk_cc n s =
  match String.uppercase s with
    "A" | "NBE" -> Above                  | "LE" | "NG" -> LessEq
  | "AE" | "NB" | "NC" -> AboveEq	  | "NE" | "NZ" -> NotEq
  | "B" | "NAE" | "C" -> Below		  | "NO" -> NotOverflow
  | "BE" | "NA" -> BelowEq		  | "NS" -> NotSign
  | "E" | "Z" -> Eq			  | "O" -> Overflow
  | "G" | "NLE" -> Greater		  | "PE" | "P" -> ParityEven
  | "GE" | "NL" -> GreaterEq		  | "PO" | "NP" -> ParityOdd
  | "L" | "NGE" -> Less			  | "S" -> Sign
  | _ -> errn n "bad condition code"; raise Gcdfec.Exit
;;

let process_prj (o,cs) n opt =
  match o with
    Reg r -> Prjr ((r,cs),n,opt)
  | Addr l -> Prjl ((l,cs),n,opt)
  | _ -> err "bad projection for genop"; raise Gcdfec.Exit
;;

let chk_arr_reg n (r,part) s =
  if s =$ i32_1 then
    (if part<>RPl then errn n "register part does not match scale")
  else if s =$ i32_2 then
    (if part<>RPx then errn n "register part does not match scale")
  else if s =$ i32_4 then
    (if part<>RPe then errn n "register part does not match scale")
  else errn n "bad scale";
  r
    ;;

type bi = BInum of int32 | BIstr of string;;

let process_byte_list bis =
  let f bi = match bi with BInum _ -> 1 | BIstr s -> String.length s in
  let len = List.fold_left (+) 0 (List.map f bis) in
  let s = String.create len in
  let rec g off bis =
    match bis with
      [] -> Dbytes (s)
    | (BInum n)::bis ->
 	let n = int32_to_int n in
	s.[off] <- Char.chr (n land 255); g (off+1) bis
    | (BIstr s1)::bis ->
 	let l = String.length s1 in
	String.blit s1 0 s off l; g (off+l) bis
  in
  g 0 bis
;;

let do_rep ri =
   ri,ref None

type coercearg =
    CAnone
  | CAgc of genop
  | CAnc of identifier

let process_dd_coerce (go,clist) =
  match go with
    CAnone -> if clist<>[] then err "bad dd coerce"; Djunk
  | CAgc (Immed i) -> D4bytes (i,clist)
  | CAgc (Addr l) -> Dlabel (l,clist)
  | _ -> err "bad dd coerce"; raise Gcdfec.Exit
;;

(* is the float in decimal format? *)
let is_decimal f =
  let l = String.length f in
  let rec aux i =
    if i < l then
      if f.[i] = '.' then true
      else aux (i+1)
    else false in
  aux 0

(* convert strings in hexadecimal format into floating-point values *)
let process_f32 f =
  try hex_to_f32 f
  with Invalid_argument _ -> (err "bad 32-bit float literal.";
			      raise Gcdfec.Exit)
let process_f64 f =
  try hex_to_f64 f
  with Invalid_argument _ -> (err "bad 32-bit float literal.";
			      raise Gcdfec.Exit)

let rec make_tapps (o,cs) tcs =
  match tcs with
    [] -> (o,cs)
  | c::tcs -> make_tapps (o,(Tapp c)::cs) tcs
;;

type kmu_item =
   MU of (identifier*kind) list
 | PR of (identifier * identifier * kind * identifier * kind * con) list

type int_item =
    INTabbrev of identifier*con
  | INTcon of int_con
  | INTval of identifier*con
  | INTkmuabbrev of kmu_item
  | INTkindabbrev of identifier*kind
;;

 (* rename kind variables within the mu
    so they will be distinct from the abbreviations *)
let do_mu ikl kinds =
   (* create renaming dictionary kd, and rename first component of ikl *)
   let iikl,kd = List.fold_left (fun (ikl,d) (i,k) ->
      let name = (id_new (id_to_string i)) in
      (i,name,k)::ikl, Dict.insert d i (kvar name)) ([], Dict.empty id_compare) ikl in
   (* use dictionary to rename second component of ikl *)
   let ikl = List.map (fun (oldi,newi,k) -> (newi, Talcon.ksubsts kd k)) iikl in
   let iil = List.map (fun (oldi,newi,k) -> (oldi,newi)) iikl in
   (* add abbreviations to kinds *)
   kinds := (List.fold_right (fun (oldi,newi) l ->
      (oldi,defkind(Kmu(ikl,newi))) ::l) iil (!kinds))

(* rename both the kind variable, j, and the con variable f *)
let do_pr l abbrevs =
   (* create dictionary, and rename f&j in l
      Careful --- j could appear multiple times in l *)
   let (_,l,kd,cd) = List.fold_left (fun (jd,l,kd,cd) (j,a,k,f,k',c) ->
      let newj = try Dict.lookup jd j with
	 Dict.Absent -> (id_new (id_to_string j)) in
      let newf = (id_new (id_to_string f)) in
      Dict.insert jd j newj,
      (f, newj,a,k,newf,k',c)::l,
      Dict.insert kd j (kvar newj),
      Dict.insert cd f (cvar newf))
	 (Dict.empty id_compare, [],
	    Dict.empty id_compare, Dict.empty id_compare) l in
   (* substitute within k,k' and c *)
   let ffl = List.map (fun (oldf, j,a,k,f,k',c) -> (oldf,f)) l in
   let l = List.map (fun (oldf, j,a,k,f,k',c) ->
      (j,a,Talcon.ksubsts kd k, f, Talcon.ksubsts kd k',
	 Talcon.substs (kd,cd) c)) l in
   abbrevs := (List.fold_right
		    (fun (oldf,newf) ab ->
		       (oldf,defcon(Cpr(newf, l)))::ab) ffl (!abbrevs))

let process_int_items is =
  let abbrevs = ref []
  and cons = ref []
  and vals = ref []
(* LX *)
  and kinds = ref []
(* end LX *)
in
  let rec loop is =
    match is with
      [] -> ()
    | (INTabbrev (v,c))::is -> abbrevs := (v,c) :: !abbrevs; loop is
    | (INTcon lkcd)::is -> cons := lkcd :: !cons; loop is
    | (INTval (l,c))::is -> vals := (l,c) :: !vals; loop is
(* LX *)
    | (INTkmuabbrev km)::is ->
	 (match km with
	    MU ikl -> do_mu ikl kinds
	  | PR l -> do_pr l abbrevs);
	 loop is
    | (INTkindabbrev (a,b))::is -> kinds := (a,b) :: !kinds; loop is
(* end LX *)
in
  loop is;
  { int_abbrevs=Array.of_list (List.rev !abbrevs);
(* LX *)
     int_kindabbrevs = Array.of_list (List.rev !kinds);
(* end LX *)
     int_cons=Array.of_list (List.rev !cons);
     int_vals=Array.of_list (List.rev !vals)
  }
;;

type imp_item =
    IMPimport of string
  | IMPexport of string
  | IMPabbrev of (identifier*con)
(* LX *)
  | IMPkindabbrev of (identifier*kind)
  | IMPkmuabbrev of kmu_item
(* LX *)
  | IMPconblock of (identifier*kind*con)
  | IMPcode
  | IMPdata
  | IMPlabel of identifier
  | IMPlabeltype of con
  | IMPinst of instruction
  | IMPcoerce of unit coerce
  | IMPalign of Numtypes.int32
  | IMPdi of data_item
(* Cyclone *)
  | IMPtemplate
  | IMPtemplate_start of identifier * con
  | IMPtemplate_end
(* End Cyclone *)
;;

(* Allowable patterns:
 *   i*a*((.c (l lc i* )* )|(.d (l lc? di* )* ))*
 *   exports allowed anywhere, conblocks allowed anywhere after typedefs
 *)

(* Implement as a state machine:
 *   0: looking for imports
 *   1: seen an abbrev
 *   2: seen a .code
 *   3: seen a .data
 *   5: seen a con block but no code or data
(* Cyclone *)
 *   6: in .code, template
(* End Cyclone *)
 *)

let process_mod_items items =
  let imports = ref []
  and exports = ref []
  and abbrevs = ref []
  and con_blocks = ref []
  and code_blocks = ref []
  and data_blocks = ref [] in
(* Cyclone *)
  let templates = ref []
  and template_labels = ref None
  and template_blocks = ref []
(* End Cyclone *)
(* LX *)
  and kinds = ref []
  and kmus = ref []
(* end LX *)
in
  let code_block l is =
    let (c,is) =
      match is with
      	(IMPlabeltype c)::is -> (Some c,is)
      |	is -> (None,is)
    and insts = ref [] in
    let rec loop is =
      match is with
      	(IMPinst i)::is -> insts := i :: !insts; loop is
      |	_ -> is in
    let is = loop is in
    ((l,c,Array.of_list (List.rev !insts)),is) in
  let data_block l is =
    let (align,co,is) =
      match is with
      |	(IMPlabeltype c)::(IMPalign n)::is -> (    n,Some c,is)
      |	(IMPalign n)::(IMPlabeltype c)::is -> (    n,Some c,is)
      |	(IMPlabeltype c)              ::is -> (i32_4,Some c,is)
      |	(IMPalign n)                  ::is -> (    n,None  ,is)
      |	_                                  -> (i32_4,None  ,is) in
    let (clist,is ) =
      match is with
	(IMPcoerce ((),clist))::is -> (clist,is)
      |	_ -> ([],is) in
    let dis = ref [] in
    let rec loop is =
      match is with
	(IMPdi di)::is -> dis := di :: !dis; loop is
      |	_ -> is in
    let is = loop is in
    ((l,align,co,(List.rev !dis,clist)),is) in
  let rec loop s is =
    match s,is with
      _,[] -> ()
    | 0,(IMPimport intref)::is -> imports := intref :: !imports; loop s is
    | _,(IMPexport intref)::is -> exports := intref :: !exports; loop s is
    | (0 | 1),(IMPabbrev lc)::is -> abbrevs := lc :: !abbrevs; loop 1 is
(* LX *)
    | _,(IMPkmuabbrev km)::is ->
	 (match km with
	    MU ikl -> do_mu ikl kinds
	  | PR l -> do_pr l abbrevs);
	 loop s is
    | _,(IMPkindabbrev a)::is -> kinds := a :: !kinds; loop s is
(* end LX *)
    | (0 | 1 | 2 | 3 | 5),(IMPconblock lkc)::is ->
	con_blocks := lkc :: !con_blocks; loop (if s<2 then 5 else s) is
    | (0 | 1 | 2 | 3 | 5),(IMPcode)::is -> loop 2 is
    | (0 | 1 | 2 | 3 | 5),(IMPdata)::is -> loop 3 is
    | 2,(IMPlabel l)::is ->
 	let (cb,is) = code_block l is in
	code_blocks := cb :: !code_blocks; loop s is
    | 3,(IMPlabel l)::is ->
 	let (db,is) = data_block l is in
	data_blocks := db :: !data_blocks; loop s is
(* Cyclone *)
    | 2,(IMPtemplate_start(lengthlabel,con))::is ->
        template_labels := Some(lengthlabel,con);
        template_blocks := [];
        loop 6 is
    | 6,IMPtemplate_end::is ->
        begin
          let cbs = List.rev(!template_blocks) in
          match !template_labels with
            Some(lengthlabel,con) ->
              template_labels := None;
              template_blocks := [];
              templates := (lengthlabel,con,cbs) :: !templates;
              loop 2 is
          | None -> failwith "TEMPLATE_END without TEMPLATE_START"
        end
    | 6,(IMPlabel l)::is ->
 	let (cb,is) = code_block l is in
	template_blocks := cb :: !template_blocks; loop s is
(* End Cyclone *)
    | _,_ -> err "bad items"; raise Gcdfec.Exit
  in
  loop 0 items;
  { import_refs=Array.of_list
      (List.map (function s -> Int_filename s) (List.rev !imports));
    export_refs=Array.of_list
      (List.map (function s -> Int_filename s) (List.rev !exports));
    pre_imp = { imp_abbrevs=Array.of_list (List.rev !abbrevs);
(* LX *)
		imp_kindabbrevs = Array.of_list (List.rev !kinds);
(* end LX *)
		con_blocks=Array.of_list (List.rev !con_blocks);
		code_blocks=Array.of_list (List.rev !code_blocks);
		data_blocks=Array.of_list (List.rev !data_blocks);
(* Cyclone *)
		templates=Array.of_list (List.rev !templates);
(* End Cyclone *)
	  };
  }
;;

let process_coerce n (go,clist) =
  match go with
    CAnone -> IMPcoerce ((),clist)
  | CAgc ((Reg r) as gop) -> IMPinst (Coerce (gop,clist))
  | CAgc ((Prjr (_,_,_)) as gop)-> IMPinst (Coerce (gop,clist))
  | CAnc name -> IMPinst (CoerceName (name,clist))
  | _ -> errn n "bad coerce directive"; raise Gcdfec.Exit
;;

let process_coerce_in_list (go,clist) =
  match go with
    CAnone ->
      err "can't coerce data inside virtual instruction list";
      raise Gcdfec.Exit
  | CAgc ((Reg r) as gop) -> Coerce (gop,clist)
  | CAgc ((Prjr (_,_,_)) as gop)-> Coerce (gop,clist)
  | CAnc name -> CoerceName (name,clist)
  | _ -> err "bad coerce directive"; raise Gcdfec.Exit
;;

let empty_regs = Dict.empty (compare);;

let mk_id s =
  let l = String.length s in
  let i = ref (l - 1) in
  while !i>=0 & s.[!i]>='0' & s.[!i]<='9' do decr i done;
  if !i>=0 & !i<l-1 & s.[!i]='$' then begin
    (* Check for leading zeros *)
    if !i<l-2 & s.[!i+1]='0' then err "bad identifier";
    id_make (String.sub s 0 !i) (int_of_string (String.sub s (!i+1) (l- !i-1)))
  end else
    id_of_string s
;;

(* Floating Point *)

let fpbinop_to_pop op =
  match op with
    Fadd  -> Faddp
  | Fdiv  -> Fdivp
  | Fdivr -> Fdivrp
  | Fmul  -> Fmulp
  | Fsub  -> Fsubp
  | Fsubr -> Fsubrp
  | _ -> invalid_arg "op_to_pop: cant convert"
%}

%token <string> Tident
%token <Numtypes.int32> Tnumber
%token <string> Tstring
%token <Tal.variance> Tvar
%token <Tal.reg> Treg

%token Teol Teof Tlab Trab Tlb Trb Tlsb Trsb Tlcb Trcb Tbar Tcomma Tdot Tstar
%token Tplus Tcolon Tequal Tarrow Tcons Tbackquote Thash Tquestion Tleq
%token Tcaret

%token <string> Tinclude Ttal_export Ttal_import
%token <Tal.scale> TB
%token <Tal.scale> TJB
%token <Tal.condition> Tcmov Tset Tj
%token <Tal.kind> Tbk /* base kinds */

%token T_begin_TAL T_end_TAL Tadc Tadd Tah Tal Talign TAll Tand
%token Tarray Tasub Taupd Tax
%token Tbh Tbl Tbool Tbp Tbswap Tbtagi Tbtagvar Tbx
%token Tcall Tcbw Tcdq Tch Tcl Tclc Tcmc
%token Tcmp Tcode Tcoerce Tcwd Tcwde Tcx
%token Tdata Tdb Tdd Tdec Tdh Tdi Tdiv Tdl Tdw Tdword Tdx Tbyte
%token Tend TExist Tfallthru Tfn
%token Tidiv Timul Tinc Tint Tinto Tjecxz Tjmp Tjunk
%token Tlabeltype Tlahf Tlea
%token Tloopd Tlooped Tloopned Tmalloc Tmov Tmovsx Tmovzx
%token Tmul Tneg Tnop Tnot
%token Tor Tpack Tpop Tpopad Tpopfd Tptr Tpush Tpushad Tpushfd
%token TR TR16 Trcl Trcr Trdtsc Trec Tretn TRH TRL
%token Trol Troll Trollsum Tror TS Tsahf Tsal Tsar Tsbb Tse
%token Tshl Tshld Tshr Tshrd Tsi Tslot Tsp Tsptr Tstc Tsub Tsubsume Tsum
%token Tsunpack
%token Ttal_struct Ttal_ends Ttapp Ttest TTm TT Ttype Ttypeof
%token Tunpack Tunroll Tval Tvirtual Txchg Txor

%token Tname Tnameobj Tforgetunique Tremovename Tforgetname TNm
%token Ttagof Tamper Tamperlsb Tcap Tbang

/* Additions for Arithmetic and logical operators */
%token Ttrue Tfalse
%token Tplusplus Tmuls Tmulu Tminus
%token Tcand Tcnot Tcimplies Tciff Tlts Tltes
%token Tprove Tproof

/* Floating point */
%token <string> Tfloat
%token <string> Tliteral
%token <Tal.fpnoargs> Tfp
%token <Tal.fpsomeargs> Tfpbin
%token <Tal.fpsomeargs> Tfpsst
%token <Tal.fpsomeargs> Tfpunary
%token <Tal.fpsomeargs> Tfpmem
%token <Tal.fpsomeargs> Tfpnone_or_reg
%token <Tal.fpsomeargs> Tfcom
%token <int> Tfpreg Tfpregq
%token <Tal.fpsomeargs> Tfstsw
%token <Tal.fpsomeargs> Tfpregs
%token Tffree
%token Tst Tword Tqword
%token Tfloat32 Tfloat64        /* float con of given size */
%token Tdfloat32 Tdfloat64      /* float data of given size */

/* Cyclone */
%token T_begin_CYCLONE Ttemplate_start Ttemplate_end
%token Tcgstart Tcgdump Tcgend Tcgforget
%token Tcgfill Tcgfilljmp Tcgfilljcc
%token Tcghole Tcgholejmp Tcgholejcc
%token Ttptr Tcgregion Ttmpl Tecg
/* End Cyclone */

/* Additions for LX */
%token Tinj
%token Tcase
%token Tprfn
%token Tletprod Tletroll Tvcase Tvoid
%token Tkind Tkindrec Tandkind Tconrec Tandcon
/* %token Tprnat %/
/* end LX */
%token Trep Tdr Tlabel


%right Tarrow

%start tal_int
%start tal_pre_mod

/**//* Added by RLS 5/13/99 */
%start coerce
%start con

%type <Tal.genop Tal.coerce> coerce
%type <Tal.con> con
/**/

%type <Tal.tal_int> tal_int
%type <Tal.tal_pre_mod> tal_pre_mod
%type <Tal.machine_state> machine_state
%type <Tal.genop> unary_op
%type <Tal.genop Tal.coerce> unary_op_coerce
%type <Tal.genop> anop
%type <Tal.genop Tal.coerce> anop_coerce
%%

tal_int:
  int_items Teof {process_int_items $1}
;

int_items:
   {[]}
| int_item int_items {$1::$2}
| Teol int_items {$2}
;

int_item:
  Ttype Tlab tvar Tequal con Trab Teol {INTabbrev ($3,$5)}
| Ttype Tlab label Tcolon kind Trab Teol {INTcon ($3,$5,AbsCon)}
| Ttype Tlab label Tcolon kind Tequal con Trab Teol {INTcon ($3,$5,ConcCon $7)}
| Ttype Tlab label Tcolon kind Tleq con Trab Teol {INTcon ($3,$5,BoundCon $7)}
| Tval label Tcomma econ Teol {INTval ($2,$4)}
/* LX */
| Tkind Tlab kvar Tequal kind Trab Teol {INTkindabbrev ($3,$5)}
| Tkindrec Tlab kvar Tequal kind Trab Teol andkinds
     {INTkmuabbrev (MU(($3, $5)::$8)) }
| Tconrec prcon andcons
     {INTkmuabbrev (PR($2::$3)) }
;

prcon:
 /*  < f : j -> k' = fn a : k . c > */
    Tlab tvar Tcolon kvar Tarrow kind Tequal Tfn tvar Tcolon kind Tdot con Trab Teol
       /* (j,a,k,f,k') */
    { ($4, $9, $11, $2, $6, $13) }

andkinds: {[]} | andkind  andkinds { $1::$2 }
andkind: Tandkind Tlab kvar Tequal kind Trab Teol { ($3, $5) }
andcons: {[]} | Tandcon prcon andcons { $2::$3 }
/* end LX */
;

tal_pre_mod:
  prolog imp_items epilog {process_mod_items $2}
;

prolog:
  eols Tinclude Teol eols T_begin_TAL Teol
       {if (String.lowercase $2)<>"tal.inc" then err "invalid prolog"}
/* Cyclone */
| eols Tinclude Teol eols Tinclude Teol eols T_begin_TAL Teol
  eols T_begin_CYCLONE Teol
   { if (String.lowercase $2)<>"tal.inc" then failwith "invalid prolog";
     if (String.lowercase $5)<>"cyclone.inc" then failwith "invalid prolog" }
/* End Cyclone */
;

epilog:
  T_end_TAL Teol eols Tend eols Teof {()}
;

eols: {()} | Teol eols {()};

imp_items:
   {[]}
| imp_item imp_items {$1::$2}
| Teol imp_items {$2}
;

imp_item:
  Ttal_import Teol {IMPimport $1}
| Ttal_export Teol {IMPexport $1}
| Ttype Tlab tvar Tequal con Trab Teol {IMPabbrev ($3,$5)}
| Ttype Tlab label Tcolon kind Tequal con Trab Teol {IMPconblock ($3,$5,$7)}
| Tcode Teol {IMPcode}
| Tdata Teol {IMPdata}
| label Tcolon {IMPlabel $1}
| Tlabeltype econ Teol {IMPlabeltype $2}
| instruction Teol {IMPinst $1}
| Tcoerce coerce1 Teol {process_coerce 2 $2}
| Talign Tnumber Teol {IMPalign $2}
| data_item Teol {IMPdi $1}
/* Cyclone */
| Ttemplate_start label Tcomma econ {IMPtemplate_start($2,$4)}
| Ttemplate_end {IMPtemplate_end}
/* End Cyclone */
/* LX */
| Tkind Tlab kvar Tequal kind Trab Teol {IMPkindabbrev ($3,$5)}
| Tkindrec Tlab kvar Tequal kind Trab Teol andkinds
     {IMPkmuabbrev (MU(($3, $5)::$8)) }
| Tconrec prcon andcons
     {IMPkmuabbrev (PR($2::$3)) }
/* end LX */
;

/* TODO: movsx/movzx */

instructionlist:
  instruction { [$1] }
| instruction instructionlist { $1::$2 }
| Tcoerce coerce1 { [process_coerce_in_list $2] }
| Tcoerce coerce1 instructionlist { (process_coerce_in_list $2)::$3 }

instruction:
/* Generic x86 instructions */
  Tadc binop {ArithBin (Adc,fst $2,snd $2)}
| Tadd binop {ArithBin (Add,fst $2,snd $2)}
| Tand binop {ArithBin (And,fst $2,snd $2)}
| Tbswap reg {Bswap $2}
| Tcall unary_op_coerce {Call $2}
| Tcbw {Conv Cbw}
| Tcdq {Conv Cdq}
| Tclc {Clc}
| Tcmc {Cmc}
| Tcmov reg Tcomma anop_coerce {Cmovcc ($1,$2,$4)}
| Tcmp binop3 {Cmp (fst $2,snd $2)}
| Tcwd {Conv Cwd}
| Tcwde {Conv Cwde}
| Tdec unary_op {ArithUn (Dec,$2)}
| Tdiv unary_op {ArithMD (Div,$2)}
| Tidiv unary_op {ArithMD (Idiv,$2)}
| imul {$1}
| Tinc unary_op {ArithUn (Inc,$2)}
| Tint Tnumber {Int (int32_to_int8 $2)}
| Tinto {Into}
| Tj coerce {Jcc ($1,mk_label_coerce 2 $2,None)}
| Tj coerce Tvirtual Tlab instructionlist Trab
    {Jcc ($1, mk_label_coerce 2 $2, Some $5)}
| Tjecxz coerce {Jecxz (mk_label_coerce 2 $2,None)}
| Tjecxz coerce Tvirtual Tlab instructionlist Trab
    {Jecxz (mk_label_coerce 2 $2, Some $5)}
| Tjmp anop_coerce {Jmp $2}
| Tlahf {Lahf}
| Tlea reg Tcomma anop {Lea ($2,$4)}
| Tloopd coerce {Loopd (mk_label_coerce 2 $2,None)}
| Tlooped coerce {Loopd (mk_label_coerce 2 $2,Some true)}
| Tloopned coerce {Loopd (mk_label_coerce 2 $2,Some false)}
| Tmov binop2 {Mov (fst $2,snd $2)}
| Tmov binop_part
    { let ((g1,p1),(g2,p2)) = $2 in
    if p1<>p2 then err "Mov requires operands to have the same size.";
    Movpart(false,g1,p1,g2,p2)
    }
| Tmovsx binop_part {let ((g1,p1),(g2,p2)) = $2 in Movpart(false,g1,p1,g2,p2) }
| Tmovzx binop_part {let ((g1,p1),(g2,p2)) = $2 in Movpart(true ,g1,p1,g2,p2) }
| Tmul unary_op {ArithMD (Mul,$2)}
| Tneg unary_op {ArithUn (Neg,$2)}
| Tnop {Nop}
| Tnot unary_op {ArithUn (Not,$2)}
| Tor binop {ArithBin (Or,fst $2,snd $2)}
| Tpop unary_op {Pop $2}
| Tpopad {Popad}
| Tpopfd {Popfd}
| Tpush unary_op_coerce {Push $2}
| Tpushad {Pushad}
| Tpushfd {Pushfd}
| Trcl unary_op Tcomma shift_amount {ArithSR (Rcl,$2,$4)}
| Trcr unary_op Tcomma shift_amount {ArithSR (Rcr,$2,$4)}
| Trdtsc { Rdtsc }
| Tretn {Retn None}
| Tretn Tnumber {Retn (Some $2)}
| Trol unary_op Tcomma shift_amount {ArithSR (Rol,$2,$4)}
| Tror unary_op Tcomma shift_amount {ArithSR (Ror,$2,$4)}
| Tsahf {Sahf}
| Tsal unary_op Tcomma shift_amount {ArithSR (Sal,$2,$4)}
| Tsar unary_op Tcomma shift_amount {ArithSR (Sar,$2,$4)}
| Tsbb binop {ArithBin (Sbb,fst $2,snd $2)}
| Tset regpart
    {if snd $2 <> RPl then
       (err "set requires low byte register"; raise Gcdfec.Exit)
     else Setcc ($1,Reg (fst $2))}
| Tshl unary_op Tcomma shift_amount {ArithSR (Shl,$2,$4)}
| Tshld anop Tcomma reg Tcomma shift_amount {Shld ($2,$4,$6)}
| Tshr unary_op Tcomma shift_amount {ArithSR (Shr,$2,$4)}
| Tshrd anop Tcomma reg Tcomma shift_amount {Shrd ($2,$4,$6)}
| Tstc {Stc}
| Tsub binop {ArithBin (Sub,fst $2,snd $2)}
| Ttest binop {Test (fst $2,snd $2)}
| Txchg anop Tcomma reg {Xchg ($2,$4)}
| Txor binop {ArithBin (Xor,fst $2,snd $2)}
/* TAL specific instructions */
/* coerce processed with imp items */
| Tfallthru {Fallthru []}
| Tfallthru Tlab Trab {Fallthru []}
| Tfallthru econlist {Fallthru $2}
| Tmalloc tvar Tcomma Tnumber Tcomma mallocarg {Malloc ($2,$4,Some $6)}
| Tmalloc tvar Tcomma Tnumber {Malloc ($2,$4,None)}
| Tproof Tlab proofop Trab {Proof $3}
| Tunpack tvar Tcomma reg Tcomma anop_coerce {Unpack ($2,$4,$6)}
| Tsunpack tvar Tcomma genop {Sunpack ($2,$4)}
| Tnameobj tvar Tcomma anop {Nameobj($2,$4)}
| Tforgetunique tvar {ForgetUnique($2)}
| Tremovename tvar {RemoveName($2)}
/* Floating Point */
| Tfp           {FPnoargs $1}
| Tfpbin        {FPsomeargs (fpbinop_to_pop $1,FPstack2 (false,1))}
| Tfpbin fpregs {let (b,i) = $2 in FPsomeargs ($1,FPstack2 (b,i))}
| Tfpbin fpmem  {FPsomeargs ($1,$2)}
| Tfpmem fpmem  {FPsomeargs ($1,$2)}
| Tfpsst fpregs
    {let (b,i) = $2 in
     if b then (errn 3 "floating point operation requires 2nd arg ST"; Nop)
     else FPsomeargs ($1,FPstack2(b,i))}
| Tfcom {FPsomeargs ($1,FPstack2 (false,1))}
| Tfcom fpreg {FPsomeargs ($1,FPstack2 (false, $2))}
| Tfcom fpmem {FPsomeargs ($1,$2)}
| Tfpnone_or_reg {FPsomeargs ($1,FPstack 1)}
| Tfpnone_or_reg fpreg {FPsomeargs ($1,FPstack $2)}
| Tfpunary fpreg {FPsomeargs ($1,FPstack $2)}
| Tfpunary fpmem {FPsomeargs ($1,$2)}
| Tffree fpreg {FPsomeargs (Ffree, FPstack $2)}
| Tfpregs fpregs {let (b,i) = $2 in FPsomeargs ($1,FPstack2 (b,i))}
| Tfstsw Tax {FPsomeargs ($1,FPgenop (Byte2,Reg Eax))}
| Tfstsw fpmem {FPsomeargs ($1,$2)}
/* Cyclone */
| Tcgstart Tlab tvar Tcomma con Trab {CgStart ($3,$5)}
| Tcgdump reg Tcomma tvar Tcomma reg Tcomma label {CgDump ($2, $4, $6, $8)}
| Tcgfill reg Tcomma reg Tcomma label Tcomma label Tcomma reg {CgFill ($2, $4, $6, $8, $10)}
| Tcgfilljmp reg Tcomma reg Tcomma label Tcomma label Tcomma
   reg Tcomma label Tcomma label
   { CgFillJmp($2,$4,$6,$8,$10,$12,$14) }
| Tcgfilljcc reg Tcomma reg Tcomma label Tcomma label Tcomma
   reg Tcomma label Tcomma label
   { CgFillJcc($2,$4,$6,$8,$10,$12,$14) }
| Tcgforget tvar Tcomma tvar {CgForget ($2,$4)}
| Tcgend reg {CgEnd ($2)}
| Tcghole reg Tcomma label Tcomma label {CgHole($2,$4,$6)}
| Tcgholejmp label Tcomma coerce {CgHoleJmp($2,mk_label_coerce 4 $4)}
| Tcgholejcc Tident Tcomma label Tcomma coerce
    {CgHoleJcc(mk_cc 2 $2,$4,mk_label_coerce 4 $6,None)}
| Tcgholejcc Tident Tcomma label Tcomma coerce Tvirtual Tlab instructionlist Trab
    {CgHoleJcc(mk_cc 2 $2,$4,mk_label_coerce 4 $6,Some $9)}
/* End Cyclone */
/* LX specific instructions */
| Tletprod Tlsb tvars Trsb Tcomma con { Letprod($3,$6) }
| Tletroll tvar Tcomma con { Letroll($2,$4) }
| Tvcase Tnumber Tcomma tvar Tcomma con Tcomma coerce { Vcase($2,$6,$4,$8) }
/* end LX */

;

/* Because Imul has three cases we need to do the dword checks here to
 * avoid parsing conflicts
 */

imul:
  Timul ptr_opt genop {ck_dword_lr $2 3 $3; ArithMD (Imul1,$3)}
| Timul ptr_opt genop Tcomma ptr_opt genop
    {ck_dword_b $2 $3 $6;
     ck_dword_b $5 $6 $3;
     ArithBin (Imul2,$3,$6)}
| Timul ptr_opt genop Tcomma ptr_opt genop Tcomma Tnumber
    {(match $2 with
     Some _ -> errn 2 "imul register has ptr";
    | None  -> ());
     ck_dword_l $5 6 $6;
     Imul3 (mk_reg 3 $3,$6,$8)}
;

mallocarg:
  Tlab mallocarg_aux Trab {$2}
;

mallocarg_aux:
  Tlsb mallocarg_auxs0 Trsb {Mprod $2}
| Tcolon Tnumber {Mbytes (mk_scale $2)}
| Tarray Tlb Tnumber Tcomma TB Trb {Mbytearray ($5,$3)}
;

mallocarg_auxs0:
    {[]}
| mallocarg_auxs {$1}
;

mallocarg_auxs:
  mallocarg_aux  {[$1]}
| mallocarg_aux Tcomma mallocarg_auxs {$1::$3}
;

shift_amount:
  Tnumber {Some $1}
| Tcl {None}
;

anop: ptr_opt genop {ck_dword_l $1 2 $2; $2};
anop_coerce: ptr_opt coerce {ck_dword_l $1 2 (fst $2); $2};
unary_op: ptr_opt genop {ck_dword_lr $1 2 $2; $2};
unary_op_coerce: ptr_opt coerce {ck_dword_lr $1 2 (fst $2); $2};
binop:
  ptr_opt genop Tcomma ptr_opt genop
    {ck_dword_b $1 $2 $5;
     ck_dword_b $4 $5 $2;
     ($2,$5)}
;
binop3:
  ptr_opt coerce Tcomma ptr_opt coerce
    { ck_dword_b $1 (fst $2) (fst $5);
      ck_dword_b $4 (fst $5) (fst $2);
      ($2,$5)}
;

binop2:
  ptr_opt genop Tcomma ptr_opt coerce
    {ck_dword_b $1 $2 (fst $5);
     ck_dword_b $4 (fst $5) $2;
     ($2,$5)}
;

binop_part:
| ptr_opt genop Tcomma binop_part_side { (($2, get_part $1 $2 (snd $4)),$4) }
| binop_part_side Tcomma ptr_opt genop { ($1,($4, get_part $3 $4 (snd $1))) }
| binop_part_side Tcomma binop_part_side { ($1,$3) }
;

binop_part_side:
| Tbyte Tptr genop { ($3, RPl) }
| genop_part       { $1 }
;

ptr_opt:
   {None}
| Tdword Tptr {Some RPe}
;

genop:
  Tnumber {Immed $1}
| reg {Reg $1}
| label {Addr $1}
/* [r1 + c] */
| Tlsb coerce Trsb {process_prj $2 i32_0 None}
| Tlsb coerce Tplus Tnumber Trsb {process_prj $2 $4 None}
/* [r1 + s*r2 + c] */
| Tlsb coerce Tplus Tnumber Tstar reg Trsb
    {process_prj $2 i32_0 (Some (mk_scale $4,$6))}
| Tlsb coerce Tplus Tnumber Tstar reg Tplus Tnumber Trsb
    {process_prj $2 $8 (Some (mk_scale $4,$6))}
/* [r1 + r2 + c] */
| Tlsb coerce Tplus reg Trsb
    {process_prj $2 i32_0 (Some (Byte1,$4))}
| Tlsb coerce Tplus reg Tplus Tnumber Trsb
    {process_prj $2 $6 (Some (Byte1,$4))}
;

genop_part:
  part { ( Reg (fst $1),snd $1) }
;

proofop:
  Tident Tlab conlist0 Trab { [(mk_id $1, $3)] }
| Tident Tlab conlist0 Trab proofop { (mk_id $1, $3) :: $5 }

coerce:
  coerce1
    {let (go,clist) = $1 in
    match go with
      CAnone -> err "bad coercion"; raise Gcdfec.Exit
    | CAgc g -> (g,clist)
    | CAnc n -> err "bad name coercion"; raise Gcdfec.Exit }
;

coerce1:
  genop {(CAgc $1,[])}
| Tquestion {(CAnone,[])}
| Tname Tlb Tident Trb {(CAnc (mk_id $3),[])}
| Tpack Tlb econ Tcomma coerce1 Tcomma econ Trb
    {let (o,cs) = $5 in (o,Pack ($3,$7)::cs)}
| Ttapp Tlb coerce1 Tcomma eannotation_list Trb {make_tapps $3 $5}
| Troll Tlb econ Tcomma coerce1 Trb
    {let (o,cs) = $5 in (o,Roll $3::cs)}
| Tunroll Tlb coerce1 Trb
    {let (o,cs) = $3 in (o,Unroll ::cs)}
| Tsum Tlb econ Tcomma coerce1 Trb
    {let (o,cs) = $5 in (o,Tosum $3::cs)}
| Trollsum Tlb econ Tcomma coerce1 Trb
    {let (o,cs) = $5 in (o,RollTosum $3::cs)}
| Trec Tlb coerce1 Trb
    {let (o,cs) = $3 in (o,Fromsum ::cs)}
| Tarray Tlb Tnumber Tcomma Tnumber Tcomma econ Tcomma coerce1 Trb
    {let (o,cs) = $9 in (o,Toarray ($3,int32_to_int $5,$7)::cs)}
| Tslot Tlb Tnumber Tcomma Tnumber Tcomma coerce1 Trb
    {let (o,cs) = $7 in (o,Slot ($3,$5)::cs)}
| Tsubsume Tlb econ Tcomma coerce1 Trb
    {let (o,cs) = $5 in (o,Subsume $3::cs)}
| Tforgetname Tlb coerce1 Trb
    {let (o,cs) = $3 in (o,Forgetname::cs)}
| Tprove Tlb coerce1 Trb
    {let (o,cs) = $3 in (o,Prove::cs)}
;

reg:
| Treg {$1}
| TR Tlb Tident Trb {Virt (mk_id $3)}
;

regpart:
| Treg {($1,RPe)}
| TR Tlb Tident Trb {(Virt (mk_id $3),RPe)}
| part { $1 }
;

part:
| Tax  {(Eax,RPx)} | Tbx  {(Ebx,RPx)} | Tcx  {(Ecx,RPx)} | Tdx  {(Edx,RPx)}
| Tsi  {(Esi,RPx)} | Tdi  {(Edi,RPx)} | Tbp  {(Ebp,RPx)} | Tsp  {(Esp,RPx)}
| Tal  {(Eax,RPl)} | Tbl  {(Ebx,RPl)} | Tcl  {(Ecx,RPl)} | Tdl  {(Edx,RPl)}
| Tah  {(Eax,RPh)} | Tbh  {(Ebx,RPh)} | Tch  {(Ecx,RPh)} | Tdh  {(Edx,RPh)}
| TR16 Tlb Tident Trb {(Virt (mk_id $3),RPx)}
| TRL Tlb Tident Trb {(Virt (mk_id $3),RPl)}
| TRH Tlb Tident Trb {(Virt (mk_id $3),RPh)}
;

/* Floating point operands */

fpreg:
  Tst {0}
| Tst Tlb Tnumber Trb {int32_to_int $3}

fpregs:
  Tst Tcomma fpreg {(true,$3)}
| Tst Tlb Tnumber Trb Tcomma Tst {(false,int32_to_int $3)}

fpmem:
  Tword Tptr genop  {FPgenop (Byte2,$3)}
| Tdword Tptr genop {FPgenop (Byte4,$3)}
| Tqword Tptr genop {FPgenop (Byte8,$3)}

/* todo: dd label_coerce */

data_item:
  Tdb byte_list {process_byte_list $2}
| Tdw Tnumber {D2bytes (int32_to_int16 $2)}
| Tdd coerce1 {process_dd_coerce $2}
| Tdr rep_item {let x = do_rep $2 in Drep (fst x, snd x)}
| Tdfloat32 Tliteral {Dfloat32 (process_f32 $2)}
| Tdfloat64 Tliteral {Dfloat64 (process_f64 $2)}
| Tdfloat32 Tfloat {Dfloat32 (dec_to_f32 $2)}
| Tdfloat64 Tfloat {Dfloat64 (dec_to_f64 $2)}
| Ttal_struct {Dup}
| Ttal_ends {Ddown}
;

rep_item:
  Ttype con {(RCon $2)}
| Tkind kind {(RKind $2)}
| Tlabel label {(RLabel $2)}
byte_list:
  byte_item byte_list_rest {$1::$2}
;

byte_list_rest:
   {[]}
| Tcomma byte_item byte_list_rest {$2::$3}
;

byte_item:
  Tnumber {BInum $1}
| Tstring {BIstr $1}
;

econ:
  Tlab con Trab {$2}
;

econlist:
  Tlab conlist Trab {$2}
;
eannotation_list:
  Tlab annotation_list Trab {$2 (* Dan added for annotations *)}
;
annotation_list:
  annotation {[$1]}
| annotation Tcomma annotation_list {$1::$3}
;
annotation:
  con                     {Con       $1}
| reg                     {AReg      $1}
| reg Tnumber             {StackTail ($1 ,int32_to_int $2)}
| reg Tnumber Tnumber con {StackSlice($1, int32_to_int $2, int32_to_int $3, $4)}
;

/* NG - not used right now
ekind:
  Tlab kind Trab {$2}
;
*/

conlist0:
   {[]}
| conlist {$1}
;

conlist:
  con {[$1]}
| con Tcomma conlist {$1::$3}
;

con: con1 {$1};

con1:
  con2 {$1}
/* | Tprnat tvar kind con lb tvar con rb
   { defcon (Cprnat ($2,$3,$4, $6, $7)) }  */
| Tfn vck vcks Tdot con1
    {List.fold_right (fun (v,k) c -> defcon(Clam (v,k,c))) ($2::$3) $5}
;

con2:
  con3 {$1}
| con2 con3 {defcon (Capp ($1,$2))}
| Tsptr con3 {defcon (Csptr $2)}
/* Cyclone */
| Ttptr tvar {defcon (Ctptr $2)}
/* End Cyclone */
;

con3:
  con4 {$1}
| con4 Tdot Tnumber {defcon(Cproj (int32_to_int $3,$1))}
/* LX */
| Tinj Tnumber con4 Tlsb kind Trsb { defcon(Cinj (int32_to_int $2, $3, $5))}
| Troll Tlsb kind Trsb con4 { defcon (Cfold($3, $5)) }
/* end LX */
;

con4:
  con5 {$1}
| TAll Tlsb vck vcks Trsb Tdot con4
    {List.fold_right (fun (v,k) c -> defcon(Cforall (v,k,c))) ($3::$4) $7}
| TAll Tlsb vck vcks Tbar con4 Trsb Tdot con4
    {match List.rev ($3::$4) with
      (v,k)::vks ->
	List.fold_left (fun c (v,k) -> cforall v k c)
	  (cforall v k (cif $6 $9)) vks
    | [] -> failwith "impossible"}
| TExist Tlsb vck vcks Trsb Tdot con4
    {List.fold_right (fun (v,k) c -> cexist v k c) ($3::$4) $7}
| TExist Tlsb vck vcks Tbar con4 Trsb Tdot con4
    {match List.rev ($3::$4) with
      (v,k)::vks ->
	List.fold_left (fun c (v,k) -> cexist v k c)
	  (cexistp v k $6 $9) vks
    | [] -> failwith "impossible"}
| Tcode con4     {defcon(Ccode $2)} /* FMS : con to con4 */
| Tcaret TT Tlsb tags Trsb {chptr $4 None None}
| Tcaret opt_tags con4 {chptr $2 (Some $3) None}
| Tcaret TT Tvar Tlb con Trb con4 {chptr [] (Some $7) (Some ($5,$3))}
| Tcaret TT Tvar Tlb con Tcomma tags Trb con4
    {chptr $7 (Some $9) (Some ($5,$3))}
;

con5:
  con6 {$1}
| con6 Tciff con5 {ciff $1 $3}
| con6 Tcimplies con5 {cimplies $1 $3}
;

con6:
  con7          {$1}
| con7 Tamper con8 {defcon(Cmsjoin($1,$3))}
| con7 Thash con6 {defcon(Cappend ($1,$3))}
| con7 Tcand con6 {cand [$1; $3]}
;

con7:
  con8 {$1}
| con8 Tcons con7 {defcon(Ccons ($1,$3))}
| con8 Tbar con7 {cor [$1; $3]}
;

con8:
  con9 {$1}
| con8 Tvar {defcon (Cfield ($1,$2))}
;

con9:
  con10 {$1}
| con10 Tlts con9 {clts $1 $3}
| con10 Tlab con9 {cltu $1 $3}
| con10 Tltes con9 {cltes $1 $3}
| con10 Tleq con9 {clteu $1 $3}
| con10 Tequal con9 {ceq $1 $3}
| con10 Tnot Tequal con9 {cne $1 $4}
;

con10:
  con11 {$1}
| con11 Tplusplus con10 {cadd [$1; $3]}
| con11 Tminus con10 {csub $1 $3}

con11:
  con12 {$1}
| Tnumber Tmuls con11 {cmuls $1 $3}
| Tnumber Tmulu con11 {cmulu $1 $3}

con12:
  con100 {$1}
| Tcnot con12 {cnot  $2}

con100:
  tvar {defcon(Cvar $1)}
| Tlsb conlist0 Trsb {defcon(Ctuple $2)}
| Tbackquote label {defcon(Clab $2)}
| Ttypeof label {defcon (Ctypeof $2) }
| pcon {defcon(Cprim $1)}
| Trec Tlb reclist Trb {defcon(Crec $3)}
| machine_state {defcon(Cms $1)}
| Tstar Tlsb conlist0 Trsb {defcon(Cprod $3)}
| Tplus Tlsb conlist0 Trsb {defcon(Csum $3)}
| Tarray Tlb con Tcomma con Trb {defcon (Carray ($3,$5))}
| TS Tlb con Trb {defcon (Csing $3)}
| Trep Tlb rep_item Trb {defcon (Cr ($3))}
| TNm Tlb con Trb {defcon (Cname $3)}
| Ttagof Tlb con Trb {defcon (Ctagof $3)}
| Tamperlsb conlist0 Trsb {defcon(Cjoin $2)}
| Tcap Tlsb caplist Trsb
    {defcon(Ccap (Dict.inserts (Dict.empty id_compare) $3))}
| Tse {defcon(Cempty)}
| Tlb con Trb {$2}
/* Cyclone */
| Ttmpl Tlb con Tcomma
    con_opt Tcomma
    lab_con_list Tcomma
    lab_con_list Trb  {defcon (Ctmpl ($3,$5,$7,$9))}
| Tcgregion Tlb con Tcomma
    con_opt Tcomma
    lab_hole_list
    Trb  {defcon (Ctrgn ($3,$5,$7))}
/* LX */
| Tvoid Tlsb kind Trsb {defcon(Cvoid $3)}
| Tcase Tlb con Trb tvar Tlsb conlist Trsb {defcon(Ccase ($3,$5,$7))}
/* end LX */

;

caplist:
           { [] }
| caplist0 { $1 }
;

caplist0:
  capentry { [$1] }
| capentry Tcomma caplist0 { $1::$3 }
;

capentry:
  tvar Tcolon con { ($1,(MayAlias,$3)) }
| tvar Tbang con  { ($1,(Unique,$3)) }
;

con_opt:
  Tstar {None}
| con   {Some $1}

lab_con_list:
  Tlcb Trcb {[]}
| Tlcb lab_con_list0 Trcb {$2}
;

lab_con_list0:
| label Tcolon con {[($1,$3)]}
| label Tcolon con Tcomma lab_con_list0 {($1,$3)::($5)}
;

labels_and_holes:
  Tlb tvar Tcomma lab_con_list Tcomma lab_con_list Trb
    { ($2,$4,$6) }
;

lab_hole_list:
  Tlcb Trcb {[]}
| Tlcb lab_hole_list0 Trcb {$2}
;

lab_hole_list0:
  labels_and_holes {[$1]}
| labels_and_holes Tcomma lab_hole_list0 {($1)::($3)}
/* End Cyclone */
;

vcks:
   {[]}
| vck vcks {$1::$2}
;

vck:
  tvar Tcolon kind {($1,$3)}
;

opt_tags:
   {[]}
| TT Tlb tags Trb {$3}
;

tags:
  Tnumber {[$1]}
| Tnumber Tcomma tags {$1::$3}
;

pcon:
  TB {PCbytes $1}
| Tfloat32 {PCfloat32}
| Tfloat64 {PCfloat64}
| Tjunk Tnumber {PCjunk $2}
| TJB {PCjunkbytes $1}
| Tnumber {PCint $1}
| Ttrue {PCtrue}
| Tfalse {PCfalse}
;

reclist:
  recitem {[$1]}
| recitem Tcomma reclist {$1::$3}
;

recitem:
  tvar Tcolon kind Tdot con {($1,$3,$5)}
;

machine_state:
  Tlcb rccs Trcb {$2}
;

rccs:
   {ms_empty}
| rccs1 {$1}
;

rccs1:
  reg Tcolon con {ms_set_reg ms_empty $1 $3}
| Tcap Tcolon con {ms_set_cap ms_empty $3}
| Tfpreg  {ms_set_fpstack ms_empty (fpstack_init_reg fpstack_empty $1)}
| Tfpregq {ms_set_fpstack ms_empty (fpstack_hide_reg fpstack_empty $1)}
| Tfpreg Tquestion {ms_set_fpstack ms_empty (fpstack_hide_reg fpstack_empty $1)}
| rccs1 Tcomma reg Tcolon con {ms_set_reg $1 $3 $5}
| rccs1 Tcomma Tcap Tcolon con {ms_set_cap $1 $5}
| rccs1 Tcomma Tfpreg
    {ms_set_fpstack $1 (fpstack_init_reg (ms_get_fpstack $1) $3)}
| rccs1 Tcomma Tfpregq
    {ms_set_fpstack $1 (fpstack_hide_reg (ms_get_fpstack $1) $3)}
| rccs1 Tcomma Tfpreg Tquestion
    {ms_set_fpstack $1 (fpstack_hide_reg (ms_get_fpstack $1) $3)}
;

kind:
  Tlb kind Trb {$2}
| Tbk {$1}
| TT  {ktype}
| TTm {kmem}
| TTm Tnumber {kmemi $2}
| kind Tarrow kind {karrow $1 $3}
| Tstar Tlsb kind_list Trsb {kprod $3}
/* LX */
| Tplus Tlsb kind_list Trsb {ksum $3}
| kvar {kvar $1}
/* end LX */

;

kind_list:
   {[]}
| kind kind_list_rest {$1::$2}
;

kind_list_rest:
   {[]}
| Tcomma kind kind_list_rest {$2::$3}
;

label:
  Tident {mk_id $1}
;

tvar:
  Tident {mk_id $1}
;


/* LX */
tvars:
  tvar {[ $1 ] }
| tvar Tcomma tvars { $1 :: $3 }


kvar:
  Tident {mk_id $1}
;
/* end LX */

/* EOF: talparser.mly */
