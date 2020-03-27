%{

(* Not clear how to allow function declarations to specify a return type
and how to allow both to be specified as static, because they are in
different rules.  The rules seem to have to be combined, which would allow
functions to be declared as local variables *)

(* Not clear how to let a function have a parameter of type void.  At the
moment, void is allowed to be the type of a variable, which is wrong, and a
parameter needs both a type and an identifier *)
module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module P = Parse_aux

%}

%token EOF

%token TIdentifier TExpression TStatement TFunction TLocal TType TParameter
%token Tlist TFresh TConstant TError TWords TWhy0 TPlus0 TBang0
%token TPure TContext
%token TTypedef TDeclarer
%token TUsing TExtends TDepends TOn
%token TNothing
%token<string> TRuleName

%token<Data.clt> Tchar Tshort Tint Tdouble Tfloat Tlong
%token<Data.clt> Tvoid Tstruct Tunion
%token<Data.clt> Tunsigned Tsigned

%token<Data.clt> Tstatic Tauto Tregister Textern Tinline
%token<Data.clt> Tconst Tvolatile
%token<string * Data.clt> Tattr

%token <Data.clt> TIf TElse TWhile TFor TDo TSwitch TCase TDefault TReturn
%token <Data.clt> TBreak TContinue
%token <Data.clt> TSizeof
%token <Data.clt> TFunDecl
%token <string * Data.clt> TIdent TTypeId TDeclarerId
%token <(string * string) * Ast0_cocci.pure * Data.clt> TMetaId TMetaType
%token <(string * string) * Ast0_cocci.pure * Data.clt> TMetaErr
%token <(string * string) * Ast0_cocci.pure * Data.clt>
                                                  TMetaParam TMetaParamList
%token <(string * string) * Ast0_cocci.pure * Data.clt> TMetaStm TMetaStmList
%token <(string * string) * Ast0_cocci.pure * Data.clt>
                                                  TMetaFunc TMetaLocalFunc
%token <(string * string) * Ast0_cocci.pure * Data.clt> TMetaExpList
%token <(string * string) * Ast0_cocci.pure * Type_cocci.typeC list option *
          Data.clt> TMetaExp TMetaConst
%token TArob TArobArob

%token <Data.clt> TEllipsis TOEllipsis TCEllipsis
%token <Data.clt> TWhen TLineEnd
/*
%token <Data.clt> TCircles TOCircles TCCircles
%token <Data.clt> TStars TOStars TCStars
*/

%token <Data.clt> TWhy TDotDot TBang TOPar TOPar0
%token <Data.clt> TMid0 TCPar TCPar0

%token <string>  TPragma
%token <string * Data.clt> TIncludeL TIncludeNL
%token <Data.clt * token> TDefine
%token <Data.clt * token * int> TDefineParam
%token <string * Data.clt> TMinusFile TPlusFile

%token <Data.clt> TInc TDec

%token <string * Data.clt> TString TChar TFloat TInt

%token <Data.clt> TOrLog
%token <Data.clt> TAndLog
%token <Data.clt> TOr
%token <Data.clt> TXor
%token <Data.clt> TAnd
%token <Data.clt> TEqEq TNotEq
%token <Data.clt> TInf TSup TInfEq TSupEq
%token <Data.clt> TShl TShr
%token <Data.clt> TPlus TMinus
%token <Data.clt> TMul TDiv TMod TTilde

%token <Data.clt> TOBrace TCBrace
%token <Data.clt> TOCro TCCro

%token <Data.clt> TPtrOp

%token TMPtVirg
%token <Data.clt> TEq TDot TComma TPtVirg
%token <Ast_cocci.assignOp * Data.clt> TAssign

%token TIso TRightIso TIsoExpression TIsoStatement TIsoDeclaration TIsoType
%token TIsoTopLevel

%token TInvalid

/* operator precedence */
%nonassoc TIf
%nonassoc TElse

%left TOrLog
%left TAndLog
%left TOr
%left TXor
%left TAnd
%left TEqEq TNotEq
%left TInf TSup TInfEq TSupEq
%left TShl TShr
%left TPlus TMinus
%left TMul TDiv TMod

%type <unit> reinit

%start minus_main
%type <Ast0_cocci.rule> minus_main

%start plus_main
%type <Ast0_cocci.rule> plus_main

%start rule_name
%type <string * Ast_cocci.dependency list * string option> rule_name

%start meta_main
%type <(Ast_cocci.metavar,Ast_cocci.metavar) Common.either list> meta_main

%start iso_main
%type <Ast0_cocci.anything list list> iso_main

%start iso_meta_main
%type <(Ast_cocci.metavar,Ast_cocci.metavar) Common.either list> iso_meta_main

%start never_used
%type <unit> never_used

%%

reinit: { }
minus_main: minus_body EOF { $1 } | m=minus_body TArobArob { m }
| m=minus_body TArob { m }
plus_main: plus_body EOF { $1 } | p=plus_body TArobArob { p }
| p=plus_body TArob { p }
meta_main: m=metadec   { m (!Ast0.rule_name) }
iso_meta_main: m=metadec { m "" }

/*****************************************************************************
*
*
*****************************************************************************/

pure:
  TPure       { Ast0.Pure }
| TContext    { Ast0.Context }
| /* empty */ { Ast0.Impure }

rule_name:
  nm=pure_ident extends d=depends i=ioption(choose_iso) TArob
    { let n = P.id2name nm in
    (try let _ =  Hashtbl.find Data.all_metadecls n in
    raise (Semantic_cocci.Semantic ("repeated rule name"))
    with Not_found -> ());
    (n,d,i) }

extends:
  /* empty */                                     { () }
| TExtends parent=TRuleName
    { !Data.install_bindings (parent) }

depends:
  /* empty */                                     { [] }
| TDepends TOn parents=separated_nonempty_list(TAndLog,pnrule) { parents }

pnrule:
  TRuleName       { Ast.Dep $1 }
| TBang TRuleName { Ast.AntiDep $2 }

choose_iso:
  TUsing TString     { P.id2name $2 }

metadec:
  ar=arity ispure=pure
  kindfn=metakind ids=comma_list(pure_ident_or_meta_ident) TMPtVirg
    { function current_rule ->
      List.concat
	(List.map
	   (function (rule,nm) ->
	     let (rule,checker) =
	       match rule with
		 None -> ((current_rule,nm),function x -> [Common.Left x])
	       | Some rule ->
		   ((rule,nm),
		    function x -> P.check_meta x; [Common.Right x]) in
	     kindfn ar rule ispure checker)
	   ids) }

%inline metakind:
  TIdentifier
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaIdDecl(arity,name)) in
      !Data.add_id_meta name pure; tok) }
| TFresh TIdentifier
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaFreshIdDecl(arity,name)) in
      !Data.add_id_meta name pure; tok) }
| TType
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaTypeDecl(arity,name)) in
      !Data.add_type_meta name pure; tok) }
| TParameter
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaParamDecl(arity,name)) in
      !Data.add_param_meta name pure; tok) }
| TParameter Tlist
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaParamListDecl(arity,name)) in
      !Data.add_paramlist_meta name pure; tok)}
| TError
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaErrDecl(arity,name)) in
      !Data.add_err_meta name pure; tok) }
| TExpression
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaExpDecl(arity,name,None)) in
      !Data.add_exp_meta None name pure; tok) }
| TExpression Tlist
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaExpListDecl(arity,name)) in
      !Data.add_explist_meta name pure; tok) }
| TExpression m=nonempty_list(TMul)
    { (fun arity name pure check_meta ->
      let ty = Some [P.ty_pointerify Type_cocci.Unknown m] in
      let tok = check_meta(Ast.MetaExpDecl(arity,name,ty)) in
      !Data.add_exp_meta ty name pure; tok) }
| TStatement
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaStmDecl(arity,name)) in
      !Data.add_stm_meta name pure; tok) }
| TStatement Tlist
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaStmListDecl(arity,name)) in
      !Data.add_stmlist_meta name pure; tok) }
| TFunction
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaFuncDecl(arity,name)) in
      !Data.add_func_meta name pure; tok) }
| TLocal TFunction
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaLocalFuncDecl(arity,name)) in
      !Data.add_local_func_meta name pure;
      tok) }
| vl=meta_exp_type // no error if use $1 but doesn't type check
    { (fun arity name pure check_meta ->
      let ty = Some vl in
      let tok = check_meta(Ast.MetaExpDecl(arity,name,ty)) in
      !Data.add_exp_meta ty name pure; tok) }
| vl=meta_exp_type TOCro TCCro
    { (fun arity name pure check_meta ->
      let ty = Some (List.map (function x -> Type_cocci.Array x) vl) in
      let tok = check_meta(Ast.MetaExpDecl(arity,name,ty)) in
      !Data.add_exp_meta ty name pure; tok) }
| TConstant ty=ioption(meta_exp_type)
    { (fun arity name pure check_meta ->
      let tok = check_meta(Ast.MetaConstDecl(arity,name,ty)) in
      !Data.add_const_meta ty name pure; tok) }
| TTypedef
    { (fun arity (_,name) pure check_meta ->
      if arity = Ast.NONE && pure = Ast0.Impure
      then (!Data.add_type_name name; [])
      else raise (Semantic_cocci.Semantic "bad typedef")) }
| TDeclarer
    { (fun arity (_,name) pure check_meta ->
      if arity = Ast.NONE && pure = Ast0.Impure
      then (!Data.add_declarer_name name; [])
      else raise (Semantic_cocci.Semantic "bad declarer")) }

meta_exp_type:
  t=ctype
    { [Ast0_cocci.ast0_type_to_type t] }
| TOBrace t=comma_list(ctype) TCBrace m=list(TMul)
    { List.map
	(function x -> P.ty_pointerify (Ast0_cocci.ast0_type_to_type x) m)
	t }

arity: TBang0 { Ast.UNIQUE }
     | TWhy0  { Ast.OPT }
     | TPlus0 { Ast.MULTI }
     | /* empty */ { Ast.NONE }

generic_ctype:
       q=ctype_qualif
         { Ast0.wrap(Ast0.ImplicitInt(q)) }
     | q=ioption(ctype_qualif) ty=Tchar
         { Ast0.wrap(Ast0.BaseType(P.clt2mcode Ast.CharType ty, q)) }
     | q=ioption(ctype_qualif) ty=Tshort
         { Ast0.wrap(Ast0.BaseType(P.clt2mcode Ast.ShortType ty, q)) }
     | q=ioption(ctype_qualif) ty=Tint
         { Ast0.wrap(Ast0.BaseType(P.clt2mcode Ast.IntType ty, q)) }
     | t=Tdouble
         { Ast0.wrap(Ast0.BaseType(P.clt2mcode Ast.DoubleType t, None)) }
     | t=Tfloat
         { Ast0.wrap(Ast0.BaseType(P.clt2mcode Ast.FloatType t, None)) }
     | q=ioption(ctype_qualif) ty=Tlong
         { Ast0.wrap(Ast0.BaseType(P.clt2mcode Ast.LongType ty, q)) }
     | s=struct_or_union i=ident
	 { Ast0.wrap(Ast0.StructUnionName(s, i)) }
     | s=struct_or_union i=ident l=TOBrace d=struct_decl_list r=TCBrace
	 { Ast0.wrap(Ast0.StructUnionDef(Ast0.wrap(Ast0.StructUnionName(s, i)),
					 P.clt2mcode "{" l,
					 d, P.clt2mcode "}" r)) }
     | s=TMetaType l=TOBrace d=struct_decl_list r=TCBrace
	 { let (nm,pure,clt) = s in
	 let ty = Ast0.wrap(Ast0.MetaType(P.clt2mcode nm clt,pure)) in
	 Ast0.wrap
	   (Ast0.StructUnionDef(ty,P.clt2mcode "{" l,d,P.clt2mcode "}" r)) }
     | r=TRuleName TDot p=TIdent
	 { let nm = (r,P.id2name p) in
	 (* this is only possible when we are in a metavar decl.  Otherwise,
	    it will be represented already as a MetaType *)
	 let _ = P.check_meta(Ast.MetaTypeDecl(Ast.NONE,nm)) in
	 Ast0.wrap(Ast0.MetaType(P.clt2mcode nm (P.id2clt p),
				 Ast0.Impure (*will be ignored*))) }
     | p=TTypeId
	 { Ast0.wrap(Ast0.TypeName(P.id2mcode p)) }
     | p=TMetaType
	 { let (nm,pure,clt) = p in
	 Ast0.wrap(Ast0.MetaType(P.clt2mcode nm clt,pure)) }

struct_or_union:
       s=Tstruct { P.clt2mcode Ast.Struct s }
     | u=Tunion  { P.clt2mcode Ast.Union u }

struct_decl:
       t=ctype d=d_ident pv=TPtVirg
	 { let (id,fn) = d in
	 Ast0.wrap(Ast0.UnInit(None,fn t,id,P.clt2mcode ";" pv)) }
    | t=fn_ctype lp1=TOPar st=TMul d=d_ident rp1=TCPar
	lp2=TOPar p=decl_list(name_opt_decl) rp2=TCPar pv=TPtVirg
        { let (id,fn) = d in
        let t =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp1,P.clt2mcode "*" st,P.clt2mcode ")" rp1,
		P.clt2mcode "(" lp2,p,P.clt2mcode ")" rp2)) in
        Ast0.wrap(Ast0.UnInit(None,fn t,id,P.clt2mcode ";" pv)) }
     | cv=ioption(const_vol) i=pure_ident d=d_ident pv=TPtVirg
	 { let (id,fn) = d in
	 let idtype = P.make_cv cv (Ast0.wrap (Ast0.TypeName(P.id2mcode i))) in
	 Ast0.wrap(Ast0.UnInit(None,fn idtype,id,P.clt2mcode ";" pv)) }

struct_decl_list:
   struct_decl_list_start { Ast0.wrap(Ast0.DOTS($1)) }

struct_decl_list_start:
  struct_decl                        { [$1] }
| struct_decl struct_decl_list_start { $1::$2 }
| d=edots_when(TEllipsis,struct_decl) r=continue_struct_decl_list
    { (P.mkddots "..." d)::r }

continue_struct_decl_list:
  /* empty */                        { [] }
| struct_decl struct_decl_list_start { $1::$2 }
| struct_decl                        { [$1] }

ctype:
       cv=ioption(const_vol) ty=generic_ctype m=list(TMul)
	 { P.pointerify (P.make_cv cv ty) m }
     | cv=ioption(const_vol) t=Tvoid m=nonempty_list(TMul)
         { let ty =
	     Ast0.wrap(Ast0.BaseType(P.clt2mcode Ast.VoidType t, None)) in
	   P.pointerify (P.make_cv cv ty) m }

fn_ctype: // allows metavariables
       ty=generic_ctype m=list(TMul) { P.pointerify ty m }
     | t=Tvoid m=list(TMul)
         { P.pointerify
	     (Ast0.wrap(Ast0.BaseType(P.clt2mcode Ast.VoidType t, None)))
	     m }

ctype_qualif:
       Tunsigned   { P.clt2mcode Ast.Unsigned $1 }
     | Tsigned     { P.clt2mcode Ast.Signed $1 }

/*****************************************************************************/

/* have to inline everything to avoid conflicts? switch to proper
declarations, statements, and expressions for the subterms */

minus_body:
    f=loption(filespec) i=list(includes)
    b=loption(minus_function_decl_statement_or_expression)
    ew=loption(error_words)
    { match f@i@b@ew with
      [] -> raise (Semantic_cocci.Semantic "minus slice can't be empty")
    | code -> Top_level.top_level code }

plus_body:
    f=loption(filespec) i=list(includes)
    b=loption(plus_function_decl_statement_or_expression)
    ew=loption(error_words)
    { Top_level.top_level (f@i@b@ew) }

filespec:
  TMinusFile TPlusFile
    { [Ast0.wrap
	  (Ast0.FILEINFO(P.id2mcode $1,
			 P.id2mcode $2))] }

includes:
  TIncludeL
    { Ast0.wrap
	(Ast0.DECL
	   (Ast0.wrap
	      (Ast0.Include(P.clt2mcode "#include" (P.drop_aft (P.id2clt $1)),
			    let (arity,ln,lln,offset,col,strbef,straft) =
			      P.id2clt $1 in
			    let clt = (arity,ln,lln,offset,0,strbef,straft) in
			    P.clt2mcode
			      (Ast.Local (Parse_aux.str2inc (P.id2name $1)))
			      (P.drop_bef clt))))) }
| TIncludeNL
    { Ast0.wrap
	(Ast0.DECL
	   (Ast0.wrap
	      (Ast0.Include(P.clt2mcode "#include" (P.drop_aft (P.id2clt $1)),
			    let (arity,ln,lln,offset,col,strbef,straft) =
			      P.id2clt $1 in
			    let clt = (arity,ln,lln,offset,0,strbef,straft) in
			    P.clt2mcode
			      (Ast.NonLocal (Parse_aux.str2inc (P.id2name $1)))
			      (P.drop_bef clt))))) }
| d=defineop t=ctype // TLineEnd
    { let ty = Ast0.wrap(Ast0.Ty(t)) in
      Ast0.wrap(Ast0.DECL(d (Ast0.wrap(Ast0.DOTS([ty]))))) }
| defineop b=statement_dots(TEllipsis) // TLineEnd
    { Ast0.wrap
	(Ast0.DECL
	   ($1 (Ast0.wrap(Ast0.DOTS(b (P.mkdots "...")))))) }

defineop:
  TDefine
    { let (clt,ident) = $1 in
      function body ->
	Ast0.wrap
	  (Ast0.Define(P.clt2mcode "#define" clt,
		       (match ident with
			 TMetaId((nm,pure,clt)) ->
			   Ast0.wrap(Ast0.MetaId(P.clt2mcode nm clt,pure))
		       | TIdent(nm_pure) ->
			   Ast0.wrap(Ast0.Id(P.id2mcode nm_pure))
		       | _ ->
			   raise
			     (Semantic_cocci.Semantic
				"unexpected name for a #define")),
		       Ast0.wrap Ast0.NoParams,
		       body)) }
| TDefineParam define_param_list_option TCPar
    { let (clt,ident,parenoff) = $1 in
      let (arity,line,lline,offset,col,strbef,straft) = clt in
      let lp = P.clt2mcode "(" (arity,line,lline,parenoff,0,[],[]) in
      function body ->
	Ast0.wrap
	  (Ast0.Define
	     (P.clt2mcode "#define" clt,
	      (match ident with
		TMetaId((nm,pure,clt)) ->
		  Ast0.wrap(Ast0.MetaId(P.clt2mcode nm clt,pure))
	      | TIdent(nm_pure) ->
		  Ast0.wrap(Ast0.Id(P.id2mcode nm_pure))
	      | _ ->
		  raise
		    (Semantic_cocci.Semantic
		       "unexpected name for a #define")),
	      Ast0.wrap (Ast0.DParams (lp,$2,P.clt2mcode ")" $3)),body)) }

/* ---------------------------------------------------------------------- */

define_param_list: define_param_list_start
     {let circle x =
       match Ast0.unwrap x with Ast0.DPcircles(_) -> true | _ -> false in
     if List.exists circle $1
     then Ast0.wrap(Ast0.CIRCLES($1))
     else Ast0.wrap(Ast0.DOTS($1)) }

define_param_list_start:
    ident { [Ast0.wrap(Ast0.DParam $1)] }
  | ident TComma define_param_list_start
      { Ast0.wrap(Ast0.DParam $1)::
	Ast0.wrap(Ast0.DPComma(P.clt2mcode "," $2))::$3 }
  | d=TEllipsis r=list(dp_comma_args(TEllipsis))
      { (P.mkdpdots "..." d)::
	(List.concat (List.map (function x -> x (P.mkdpdots "...")) r)) }
/*
  | d=edots_when(TCircles,ident)
	r=list(dp_comma_args(edots_when(TCircles,ident)))
      { (P.mkdpdots "ooo" d)::
	(List.concat (List.map (function x -> x (P.mkdpdots "ooo")) r)) }
*/

dp_comma_args(dotter):
  c=TComma d=dotter
    { function dot_builder ->
      [Ast0.wrap(Ast0.DPComma(P.clt2mcode "," c)); dot_builder d] }
| TComma ident
    { function dot_builder ->
      [Ast0.wrap(Ast0.DPComma(P.clt2mcode "," $1));
	Ast0.wrap(Ast0.DParam $2)] }

define_param_list_option: define_param_list { $1 }
         | /* empty */     { Ast0.wrap(Ast0.DOTS([])) }

/*****************************************************************************/

funproto:
  s=ioption(storage) t=ctype
  id=func_ident lp=TOPar d=decl_list(decl) rp=TCPar pt=TPtVirg
      { Ast0.wrap
	  (Ast0.UnInit
	     (s,
	      Ast0.wrap
		(Ast0.FunctionType(Some t,
				   P.clt2mcode "(" lp, d, P.clt2mcode ")" rp)),
	      id, P.clt2mcode ";" pt)) }
| s=ioption(storage) t=Tvoid
  id=func_ident lp=TOPar d=decl_list(decl) rp=TCPar pt=TPtVirg
    { let t = Ast0.wrap(Ast0.BaseType(P.clt2mcode Ast.VoidType t, None)) in
      Ast0.wrap
        (Ast0.UnInit
	   (s,
	    Ast0.wrap
	      (Ast0.FunctionType(Some t,
				 P.clt2mcode "(" lp, d, P.clt2mcode ")" rp)),
	    id, P.clt2mcode ";" pt)) }


fundecl:
  f=fninfo
  TFunDecl i=func_ident lp=TOPar d=decl_list(decl) rp=TCPar
  lb=TOBrace b=pre_post_decl_statement_and_expression_opt rb=TCBrace
      { Ast0.wrap(Ast0.FunDecl((Ast0.default_info(),Ast0.context_befaft()),
			       f, i,
			       P.clt2mcode "(" lp, d,
			       P.clt2mcode ")" rp,
			       P.clt2mcode "{" lb, b,
			       P.clt2mcode "}" rb)) }

fninfo:
    /* empty */ { [] }
  | storage  fninfo
      { try
	let _ =
	  List.find (function Ast0.FStorage(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "duplicate storage")
      with Not_found -> (Ast0.FStorage($1))::$2 }
  | t=fn_ctype r=fninfo_nt { (Ast0.FType(t))::r }
  | Tinline  fninfo
      { try
	let _ = List.find (function Ast0.FInline(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "duplicate inline")
      with Not_found -> (Ast0.FInline(P.clt2mcode "inline" $1))::$2 }
  | Tattr    fninfo
      { try
	let _ = List.find (function Ast0.FAttr(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "multiple attributes")
      with Not_found -> (Ast0.FAttr(P.id2mcode $1))::$2 }

fninfo_nt:
    /* empty */ { [] }
  | storage  fninfo_nt
      { try
	let _ =
	  List.find (function Ast0.FStorage(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "duplicate storage")
      with Not_found -> (Ast0.FStorage($1))::$2 }
  | Tinline  fninfo_nt
      { try
	let _ = List.find (function Ast0.FInline(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "duplicate inline")
      with Not_found -> (Ast0.FInline(P.clt2mcode "inline" $1))::$2 }
  | Tattr    fninfo_nt
      { try
	let _ = List.find (function Ast0.FAttr(_) -> true | _ -> false) $2 in
	raise (Semantic_cocci.Semantic "duplicate init")
      with Not_found -> (Ast0.FAttr(P.id2mcode $1))::$2 }

storage:
         s=Tstatic      { P.clt2mcode Ast.Static s }
       | s=Tauto        { P.clt2mcode Ast.Auto s }
       | s=Tregister    { P.clt2mcode Ast.Register s }
       | s=Textern      { P.clt2mcode Ast.Extern s }

decl: t=ctype i=ident
	{ Ast0.wrap(Ast0.Param(t, Some i)) }
    | t=fn_ctype lp=TOPar s=TMul i=ident rp=TCPar
	lp1=TOPar d=decl_list(name_opt_decl) rp1=TCPar
        { let fnptr =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp,P.clt2mcode "*" s,P.clt2mcode ")" rp,
		P.clt2mcode "(" lp1,d,P.clt2mcode ")" rp1)) in
	Ast0.wrap(Ast0.Param(fnptr, Some i)) }
    | t=Tvoid
	{ let ty = Ast0.wrap(Ast0.BaseType(P.clt2mcode Ast.VoidType t, None)) in
          Ast0.wrap(Ast0.VoidParam(ty)) }
    | TMetaParam
	{ let (nm,pure,clt) = $1 in
	Ast0.wrap(Ast0.MetaParam(P.clt2mcode nm clt,pure)) }

name_opt_decl:
      decl  { $1 }
    | t=ctype { Ast0.wrap(Ast0.Param(t, None)) }
    | t=fn_ctype lp=TOPar s=TMul rp=TCPar
	lp1=TOPar d=decl_list(name_opt_decl) rp1=TCPar
        { let fnptr =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp,P.clt2mcode "*" s,P.clt2mcode ")" rp,
		P.clt2mcode "(" lp1,d,P.clt2mcode ")" rp1)) in
	Ast0.wrap(Ast0.Param(fnptr, None)) }

const_vol:
      Tconst       { P.clt2mcode Ast.Const $1 }
    | Tvolatile    { P.clt2mcode Ast.Volatile $1 }

/*****************************************************************************/

statement:
  TMetaStm
    { P.meta_stm $1 }
| expr TPtVirg
    { P.exp_stm $1 $2 }
| TIf TOPar eexpr TCPar single_statement %prec TIf
    { P.ifthen $1 $2 $3 $4 $5 }
| TIf TOPar eexpr TCPar single_statement TElse single_statement
    { P.ifthenelse $1 $2 $3 $4 $5 $6 $7 }
| TFor TOPar option(eexpr) TPtVirg option(eexpr) TPtVirg
    option(eexpr) TCPar single_statement
    { P.forloop $1 $2 $3 $4 $5 $6 $7 $8 $9 }
| TWhile TOPar eexpr TCPar single_statement
    { P.whileloop $1 $2 $3 $4 $5 }
| TDo single_statement TWhile TOPar eexpr TCPar TPtVirg
    { P.doloop $1 $2 $3 $4 $5 $6 $7 }
| TSwitch TOPar eexpr TCPar TOBrace list(case_line) TCBrace
    { P.switch $1 $2 $3 $4 $5 $6 $7 }
| TReturn eexpr TPtVirg { P.ret_exp $1 $2 $3 }
| TReturn TPtVirg { P.ret $1 $2 }
| TBreak TPtVirg { P.break $1 $2 }
| TContinue TPtVirg { P.cont $1 $2 }
| TOBrace pre_post_decl_statement_and_expression_opt TCBrace
    { P.seq $1 $2 $3 }
| TOEllipsis w=option(whenppdecs) b=statement_dots(TEllipsis) c=TCEllipsis
    { Ast0.wrap(Ast0.Nest(P.clt2mcode "<..." $1,
			  Ast0.wrap(Ast0.DOTS(b (P.mkdots "..."))),
			  P.clt2mcode "...>" c, w)) }
/*
| TOCircles w=option(whenppdecs) b=statement_dots(TCircles) c=TCCircles
    { Ast0.wrap(Ast0.Nest(P.clt2mcode "<ooo" $1,
			  Ast0.wrap(Ast0.CIRCLES(b (P.mkdots "ooo"))),
			  P.clt2mcode "ooo>" c, w)) }
| TOStars w=option(whenppdecs) b=statement_dots(TStars) c=TCStars
    { Ast0.wrap(Ast0.Nest(P.clt2mcode "<***" $1,
			  Ast0.wrap(Ast0.STARS(b (P.mkdots "***"))),
			  P.clt2mcode "***>" c, w)) }
*/

whenppdecs: TWhen TNotEq w=pre_post_decl_statement_or_expression TLineEnd
    { w }

/* a statement that fits into a single rule_elem.  should nests be included?
what about statement metavariables? */
rule_elem_statement:
  expr TPtVirg { P.exp_stm $1 $2 }
| TReturn eexpr TPtVirg { P.ret_exp $1 $2 $3 }
| TReturn TPtVirg { P.ret $1 $2 }
| TBreak TPtVirg { P.break $1 $2 }
| TContinue TPtVirg { P.cont $1 $2 }
| TOPar0 midzero_list(rule_elem_statement) TCPar0
    { let (mids,code) = $2 in
    Ast0.wrap
      (Ast0.Disj(P.clt2mcode "(" $1,
		 List.map (function x -> Ast0.wrap(Ast0.DOTS([x]))) code,
		 mids, P.clt2mcode ")" $3)) }

statement_dots(dotter):
  r=no_dot_start_end(exp_decl_statement_list,
		     dots_when(dotter,pre_post_decl_statement_or_expression,
			       rule_elem_statement))
  { function dot_builder ->
    List.concat (r (function x -> [dot_builder x])) }

/* a statement on its own */
single_statement:
    statement                         { $1 }
  | TOPar0 midzero_list(statement) TCPar0
      /* degenerate case, elements are single statements and thus don't
	contain dots */
      { let (mids,code) = $2 in
        Ast0.wrap
	  (Ast0.Disj(P.clt2mcode "(" $1,
		     List.map (function x -> Ast0.wrap(Ast0.DOTS([x]))) code,
		     mids, P.clt2mcode ")" $3)) }

case_line:
    TDefault TDotDot pre_post_decl_statement_and_expression_opt
      { Ast0.wrap(Ast0.Default(P.clt2mcode "default" $1,P.clt2mcode ":" $2,$3)) }
  | TCase eexpr TDotDot pre_post_decl_statement_and_expression_opt
      { Ast0.wrap(Ast0.Case(P.clt2mcode "case" $1,$2,P.clt2mcode ":" $3,$4)) }

/* In the following, an identifier as a type is not fully supported.  Indeed,
the language is ambiguous: what is foo * bar; */
/* The AST DisjDecl cannot be generated because it would be ambiguous with
a disjunction on a statement with a declaration in each branch */
decl_var:
    t=ctype pv=TPtVirg
      { [Ast0.wrap(Ast0.TyDecl(t,P.clt2mcode ";" pv))] }
  | s=ioption(storage) t=ctype d=comma_list(d_ident) pv=TPtVirg
      { List.map
	  (function (id,fn) ->
	    Ast0.wrap(Ast0.UnInit(s,fn t,id,P.clt2mcode ";" pv)))
	  d }
  | f=funproto { [f] }
  | s=ioption(storage) t=ctype d=d_ident q=TEq e=initialize pv=TPtVirg
      { let (id,fn) = d in
      [Ast0.wrap(Ast0.Init(s,fn t,id,P.clt2mcode "=" q,e,P.clt2mcode ";" pv))] }
  /* type is a typedef name */
  | s=ioption(storage) cv=ioption(const_vol) i=pure_ident
      d=comma_list(d_ident) pv=TPtVirg
      { List.map
	  (function (id,fn) ->
	    let idtype = P.make_cv cv (Ast0.wrap (Ast0.TypeName(P.id2mcode i))) in
	    Ast0.wrap(Ast0.UnInit(s,fn idtype,id,P.clt2mcode ";" pv)))
	  d }
  | s=ioption(storage) cv=ioption(const_vol) i=pure_ident d=d_ident q=TEq
      e=initialize pv=TPtVirg
      { let (id,fn) = d in
      !Data.add_type_name (P.id2name i);
      let idtype = P.make_cv cv (Ast0.wrap (Ast0.TypeName(P.id2mcode i))) in
      [Ast0.wrap(Ast0.Init(s,fn idtype,id,P.clt2mcode "=" q,e,
			   P.clt2mcode ";" pv))] }
  /* function pointer type */
  | s=ioption(storage)
    t=fn_ctype lp1=TOPar st=TMul d=d_ident rp1=TCPar
    lp2=TOPar p=decl_list(name_opt_decl) rp2=TCPar
    pv=TPtVirg
      { let (id,fn) = d in
        let t =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp1,P.clt2mcode "*" st,P.clt2mcode ")" rp1,
		P.clt2mcode "(" lp2,p,P.clt2mcode ")" rp2)) in
        [Ast0.wrap(Ast0.UnInit(s,fn t,id,P.clt2mcode ";" pv))] }
  | TDeclarerId TOPar eexpr_list_option TCPar TPtVirg
      { [Ast0.wrap(Ast0.MacroDecl(P.id2mcode $1,P.clt2mcode "(" $2,$3,
				  P.clt2mcode ")" $4,P.clt2mcode ";" $5))] }
  | s=ioption(storage)
    t=fn_ctype lp1=TOPar st=TMul d=d_ident rp1=TCPar
    lp2=TOPar p=decl_list(name_opt_decl) rp2=TCPar
    q=TEq e=initialize pv=TPtVirg
      { let (id,fn) = d in
        let t =
	  Ast0.wrap
	    (Ast0.FunctionPointer
	       (t,P.clt2mcode "(" lp1,P.clt2mcode "*" st,P.clt2mcode ")" rp1,
		P.clt2mcode "(" lp2,p,P.clt2mcode ")" rp2)) in
      [Ast0.wrap(Ast0.Init(s,fn t,id,P.clt2mcode "=" q,e,P.clt2mcode ";" pv))] }


d_ident:
    ident list(array_dec)
      { ($1,
	 function t ->
	   List.fold_right
	     (function (l,i,r) ->
	       function rest ->
		 Ast0.wrap
		   (Ast0.Array(rest,P.clt2mcode "[" l,i,P.clt2mcode "]" r)))
	     $2 t) }

array_dec: l=TOCro i=option(eexpr) r=TCCro { (l,i,r) }

initialize:
    eexpr
      { Ast0.wrap(Ast0.InitExpr($1)) }
  | TOBrace initialize_list TCBrace
      { Ast0.wrap(Ast0.InitList(P.clt2mcode "{" $1,$2,P.clt2mcode "}" $3)) }
  | TOBrace TCBrace
      { Ast0.wrap
	  (Ast0.InitList(P.clt2mcode "{" $1,Ast0.wrap(Ast0.DOTS []),
			 P.clt2mcode "}" $2)) }

initialize2:
  /*arithexpr and not eexpr because can have ambiguity with comma*/
  /*dots and nests probably not allowed at top level, haven't looked into why*/
  arith_expr(eexpr,invalid) { Ast0.wrap(Ast0.InitExpr($1)) }
| TOBrace initialize_list TCBrace
    { Ast0.wrap(Ast0.InitList(P.clt2mcode "{" $1,$2,P.clt2mcode "}" $3)) }
| TOBrace TCBrace
    { Ast0.wrap
	(Ast0.InitList(P.clt2mcode "{" $1,Ast0.wrap(Ast0.DOTS []),
		       P.clt2mcode "}" $2)) }
           /* gccext:, labeled elements */
| TDot ident TEq initialize2
    { Ast0.wrap(Ast0.InitGccDotName(P.clt2mcode "." $1,$2,P.clt2mcode "=" $3,$4)) }
| ident TDotDot initialize2
    { Ast0.wrap(Ast0.InitGccName($1,P.clt2mcode ":" $2,$3)) } /* in old kernel */
| TOCro eexpr TCCro TEq initialize2
    { Ast0.wrap(Ast0.InitGccIndex(P.clt2mcode "[" $1,$2,P.clt2mcode "]" $3,
				  P.clt2mcode "=" $4,$5)) }
| TOCro eexpr TEllipsis eexpr TCCro TEq initialize2
    { Ast0.wrap(Ast0.InitGccRange(P.clt2mcode "[" $1,$2,P.clt2mcode "..." $3,
				  $4,P.clt2mcode "]" $5,P.clt2mcode "=" $6,$7)) }

initialize_list:
   initialize_list_start { Ast0.wrap(Ast0.DOTS($1)) }

initialize_list_start:
  initialize2 TComma { [$1;Ast0.wrap(Ast0.IComma(P.clt2mcode "," $2))] }
| initialize2 TComma initialize_list_start
    { $1::Ast0.wrap(Ast0.IComma(P.clt2mcode "," $2))::$3 }
| d=edots_when(TEllipsis,initialize)
      r=comma_initializers(edots_when(TEllipsis,initialize))
    { (P.mkidots "..." d)::
      (List.concat(List.map (function x -> x (P.mkidots "...")) r)) }

comma_initializers(dotter):
  /* empty */ { [] }
| d=dotter r=comma_initializers2(dotter)
      { (function dot_builder -> [dot_builder d])::r }
| i=initialize2 c=TComma r=comma_initializers(dotter)
    { (function dot_builder -> [i; Ast0.wrap(Ast0.IComma(P.clt2mcode "," c))])::
      r }

comma_initializers2(dotter):
  /* empty */ { [] }
| i=initialize2 c=TComma r=comma_initializers(dotter)
    { (function dot_builder -> [i; Ast0.wrap(Ast0.IComma(P.clt2mcode "," c))])::
      r }

/* a statement that is part of a list */
decl_statement:
    TMetaStmList
      { let (nm,pure,clt) = $1 in
      [Ast0.wrap(Ast0.MetaStmt(P.clt2mcode nm clt,pure))] }
  | decl_var
      { List.map
	  (function x ->
	    Ast0.wrap
	      (Ast0.Decl((Ast0.default_info(),Ast0.context_befaft()),x)))
	  $1 }
  | statement { [$1] }
  /* this doesn't allow expressions at top level, because the parser doesn't
	know whether there is one.  If there is one, this is not sequencible.
	If there is not one, then it is.  It seems complicated to get around
    this at the parser level.  We would have to have a check afterwards to
    allow this.  One case where this would be useful is for a when.  Now
	we allow a sequence of whens, so one can be on only statements and
    one can be on only expressions. */
  | TOPar0 pre_post_decl_statement_and_expression_opt_mid TCPar0
      { let (first,rest) = $2 in
        let (mids,code) = List.split rest in
	let code = first :: code in
	if List.for_all
	    (function x ->
	      match Ast0.unwrap x with Ast0.DOTS([]) -> true | _ -> false)
	    code
      then []
      else [Ast0.wrap(Ast0.Disj(P.clt2mcode "(" $1,
				code, mids,
				P.clt2mcode ")" $3))] }

/*****************************************************************************/

/* The following cannot contain <... ...> at the top level.  This can only
be allowed as an expression when the expression is delimited on both sides
by expression-specific markers.  In that case, the rule eexpr is used, which
allows <... ...> anywhere.  Hopefully, this will not be too much of a problem
in practice. */
expr:  basic_expr(expr,invalid) { $1 }
/* allows ... and nests */
eexpr: basic_expr(eexpr,dot_expressions) { $1 }
/* allows nests but not .... */
dexpr: basic_expr(eexpr,nest_expressions) { $1 }

invalid:
  TInvalid { raise (Semantic_cocci.Semantic "not matchable") }

dot_expressions:
  TEllipsis { Ast0.wrap(Ast0.Edots(P.clt2mcode "..." $1,None)) }
| nest_expressions { $1 }

nest_expressions:
  TOEllipsis w=option(whenexp) e=expr_dots(TEllipsis) c=TCEllipsis
    { Ast0.wrap(Ast0.NestExpr(P.clt2mcode "<..." $1,
			      Ast0.wrap(Ast0.DOTS(e (P.mkedots "..."))),
			      P.clt2mcode "...>" c, w)) }
/*
| TOCircles w=option(whenexp) e=expr_dots(TCircles) c=TCCircles
    { Ast0.wrap(Ast0.NestExpr(P.clt2mcode "<ooo" $1,
			      Ast0.wrap(Ast0.CIRCLES(e (P.mkedots "ooo"))),
			      P.clt2mcode "ooo>" c, w)) }
| TOStars w=option(whenexp) e=expr_dots(TStars) c=TCStars
    { Ast0.wrap(Ast0.NestExpr(P.clt2mcode "<***" $1,
			      Ast0.wrap(Ast0.STARS(e (P.mkedots "***"))),
			      P.clt2mcode "***>" c, w)) }
*/

whenexp: TWhen TNotEq w=eexpr TLineEnd { w }

basic_expr(recurser,primary_extra):
  assign_expr(recurser,primary_extra)                        { $1 }

assign_expr(r,pe):
    cond_expr(r,pe)                        { $1 }
  | unary_expr(r,pe) TAssign assign_expr(r,pe)
      { let (op,clt) = $2 in
      Ast0.wrap(Ast0.Assignment($1,P.clt2mcode op clt,$3)) }
  | unary_expr(r,pe) TEq assign_expr(r,pe)
      { Ast0.wrap
	  (Ast0.Assignment
	     ($1,P.clt2mcode Ast.SimpleAssign $2,$3)) }

cond_expr(r,pe):
    arith_expr(r,pe)                         { $1 }
  | l=arith_expr(r,pe) w=TWhy t=option(eexpr) dd=TDotDot r=cond_expr(r,pe)
      { Ast0.wrap(Ast0.CondExpr (l, P.clt2mcode "?" w, t,
				 P.clt2mcode ":" dd, r)) }

arith_expr(r,pe):
    cast_expr(r,pe)                         { $1 }
  | arith_expr(r,pe) TMul    arith_expr(r,pe)
      { P.arith_op Ast.Mul $1 $2 $3 }
  | arith_expr(r,pe) TDiv    arith_expr(r,pe)
      { P.arith_op Ast.Div $1 $2 $3 }
  | arith_expr(r,pe) TMod    arith_expr(r,pe)
      { P.arith_op Ast.Mod $1 $2 $3 }
  | arith_expr(r,pe) TPlus   arith_expr(r,pe)
      { P.arith_op Ast.Plus $1 $2 $3 }
  | arith_expr(r,pe) TMinus  arith_expr(r,pe)
      { P.arith_op Ast.Minus $1 $2 $3 }
  | arith_expr(r,pe) TShl    arith_expr(r,pe)
      { P.arith_op Ast.DecLeft $1 $2 $3 }
  | arith_expr(r,pe) TShr    arith_expr(r,pe)
      { P.arith_op Ast.DecRight $1 $2 $3}
  | arith_expr(r,pe) TInf    arith_expr(r,pe)
      { P.logic_op Ast.Inf $1 $2 $3 }
  | arith_expr(r,pe) TSup    arith_expr(r,pe)
      { P.logic_op Ast.Sup $1 $2 $3 }
  | arith_expr(r,pe) TInfEq  arith_expr(r,pe)
      { P.logic_op Ast.InfEq $1 $2 $3 }
  | arith_expr(r,pe) TSupEq  arith_expr(r,pe)
      { P.logic_op Ast.SupEq $1 $2 $3 }
  | arith_expr(r,pe) TEqEq   arith_expr(r,pe)
      { P.logic_op Ast.Eq $1 $2 $3 }
  | arith_expr(r,pe) TNotEq  arith_expr(r,pe)
      { P.logic_op Ast.NotEq $1 $2 $3 }
  | arith_expr(r,pe) TAnd    arith_expr(r,pe)
      { P.arith_op Ast.And $1 $2 $3 }
  | arith_expr(r,pe) TOr     arith_expr(r,pe)
      { P.arith_op Ast.Or $1 $2 $3 }
  | arith_expr(r,pe) TXor    arith_expr(r,pe)
      { P.arith_op Ast.Xor $1 $2 $3 }
  | arith_expr(r,pe) TAndLog arith_expr(r,pe)
      { P.logic_op Ast.AndLog $1 $2 $3 }
  | arith_expr(r,pe) TOrLog  arith_expr(r,pe)
      { P.logic_op Ast.OrLog $1 $2 $3 }

cast_expr(r,pe):
    unary_expr(r,pe)                      { $1 }
  | lp=TOPar t=ctype rp=TCPar e=cast_expr(r,pe)
      { Ast0.wrap(Ast0.Cast (P.clt2mcode "(" lp, t,
			     P.clt2mcode ")" rp, e)) }

unary_expr(r,pe):
    postfix_expr(r,pe)                   { $1 }
  | TInc unary_expr(r,pe)
      { Ast0.wrap(Ast0.Infix ($2, P.clt2mcode Ast.Inc $1)) }
  | TDec unary_expr(r,pe)
      { Ast0.wrap(Ast0.Infix ($2, P.clt2mcode Ast.Dec $1)) }
  | unary_op unary_expr(r,pe)
      { let mcode = $1 in Ast0.wrap(Ast0.Unary($2, mcode)) }
  | TSizeof unary_expr(r,pe)
      { Ast0.wrap(Ast0.SizeOfExpr (P.clt2mcode "sizeof" $1, $2)) }
  | s=TSizeof lp=TOPar t=ctype rp=TCPar
      { Ast0.wrap(Ast0.SizeOfType (P.clt2mcode "sizeof" s,
                                   P.clt2mcode "(" lp,
                                   t,
                                   P.clt2mcode ")" rp)) }


unary_op: TAnd    { P.clt2mcode Ast.GetRef $1 }
	| TMul    { P.clt2mcode Ast.DeRef $1 }
	| TPlus   { P.clt2mcode Ast.UnPlus $1 }
	| TMinus  { P.clt2mcode Ast.UnMinus $1 }
	| TTilde  { P.clt2mcode Ast.Tilde $1 }
	| TBang   { P.clt2mcode Ast.Not $1 }

postfix_expr(r,pe):
   primary_expr(r,pe)                            { $1 }
 | postfix_expr(r,pe) TOCro eexpr TCCro
     { Ast0.wrap(Ast0.ArrayAccess ($1,P.clt2mcode "[" $2,$3,
				       P.clt2mcode "]" $4)) }
 | postfix_expr(r,pe) TDot   ident
     { Ast0.wrap(Ast0.RecordAccess($1, P.clt2mcode "." $2, $3)) }
 | postfix_expr(r,pe) TPtrOp ident
     { Ast0.wrap(Ast0.RecordPtAccess($1, P.clt2mcode "->" $2,
				     $3)) }
 | postfix_expr(r,pe) TInc
     { Ast0.wrap(Ast0.Postfix ($1, P.clt2mcode Ast.Inc $2)) }
 | postfix_expr(r,pe) TDec
     { Ast0.wrap(Ast0.Postfix ($1, P.clt2mcode Ast.Dec $2)) }
 | postfix_expr(r,pe) TOPar eexpr_list_option TCPar
     { Ast0.wrap(Ast0.FunCall($1,P.clt2mcode "(" $2,$3,
			      P.clt2mcode ")" $4)) }

primary_expr(recurser,primary_extra):
   func_ident   { Ast0.wrap(Ast0.Ident($1)) }
 | TInt
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (P.clt2mcode (Ast.Int x) clt)) }
 | TFloat
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (P.clt2mcode (Ast.Float x) clt)) }
 | TString
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (P.clt2mcode (Ast.String x) clt)) }
 | TChar
     { let (x,clt) = $1 in
     Ast0.wrap(Ast0.Constant (P.clt2mcode (Ast.Char x) clt)) }
 | TMetaConst
     { let (nm,pure,ty,clt) = $1 in
     Ast0.wrap(Ast0.MetaConst(P.clt2mcode nm clt,ty,pure)) }
 | TMetaErr
     { let (nm,pure,clt) = $1 in
     Ast0.wrap(Ast0.MetaErr(P.clt2mcode nm clt,pure)) }
 | TMetaExp
     { let (nm,pure,ty,clt) = $1 in
     Ast0.wrap(Ast0.MetaExpr(P.clt2mcode nm clt,ty,pure)) }
 | TOPar eexpr TCPar
     { Ast0.wrap(Ast0.Paren(P.clt2mcode "(" $1,$2,
			    P.clt2mcode ")" $3)) }
 | TOPar0 midzero_list(recurser) TCPar0
     { let (mids,code) = $2 in
       Ast0.wrap(Ast0.DisjExpr(P.clt2mcode "(" $1,
			       code, mids,
			       P.clt2mcode ")" $3)) }
 | primary_extra { $1 }

expr_dots(dotter):
    r=no_dot_start_end(dexpr,edots_when(dotter,eexpr)) { r }

/*****************************************************************************/

pure_ident:
     TIdent { $1 }

pure_ident_or_meta_ident:
       pure_ident                { (None,P.id2name $1) }
     | TRuleName TDot pure_ident { (Some $1,P.id2name $3) }

func_ident: pure_ident
         { Ast0.wrap(Ast0.Id(P.id2mcode $1)) }
     | TMetaId
         { let (nm,pure,clt) = $1 in
           Ast0.wrap(Ast0.MetaId(P.clt2mcode nm clt,pure)) }
     | TMetaFunc
         { let (nm,pure,clt) = $1 in
           Ast0.wrap(Ast0.MetaFunc(P.clt2mcode nm clt,pure)) }
     | TMetaLocalFunc
	 { let (nm,pure,clt) = $1 in
           Ast0.wrap(Ast0.MetaLocalFunc(P.clt2mcode nm clt,pure)) }

ident: pure_ident
         { Ast0.wrap(Ast0.Id(P.id2mcode $1)) }
     | TMetaId
         { let (nm,pure,clt) = $1 in
           Ast0.wrap(Ast0.MetaId(P.clt2mcode nm clt,pure)) }

/*****************************************************************************/

decl_list(decl):
   decl_list_start(decl)
     {let circle x =
       match Ast0.unwrap x with Ast0.Pcircles(_) -> true | _ -> false in
     if List.exists circle $1
     then Ast0.wrap(Ast0.CIRCLES($1))
     else Ast0.wrap(Ast0.DOTS($1)) }

decl_list_start(decl):
  one_dec(decl)  { [$1] }
| one_dec(decl) TComma decl_list_start(decl)
    { $1::Ast0.wrap(Ast0.PComma(P.clt2mcode "," $2))::$3 }
| TEllipsis list(comma_decls(TEllipsis,decl))
    { Ast0.wrap(Ast0.Pdots(P.clt2mcode "..." $1))::
      (List.concat(List.map (function x -> x (P.mkpdots "...")) $2)) }
/*
| TCircles list(comma_decls(TCircles,decl))
    { Ast0.wrap(Ast0.Pdots(P.clt2mcode "ooo" $1))::
      (List.concat(List.map (function x -> x (P.mkpdots "ooo")) $2)) }
*/

one_dec(decl):
  decl  { $1 }
| TMetaParamList
    { let (nm,pure,clt) = $1 in
    Ast0.wrap(Ast0.MetaParamList(P.clt2mcode nm clt,pure)) }

comma_decls(dotter,decl):
  TComma dotter
    { function dot_builder ->
      [Ast0.wrap(Ast0.PComma(P.clt2mcode "," $1));
	dot_builder $2] }
| TComma one_dec(decl)
    { function dot_builder ->
      [Ast0.wrap(Ast0.PComma(P.clt2mcode "," $1)); $2] }

/* must be a list of declarations or statements, with no ... or expressions
for "and" case */
pure_decl_statement_list:
    nonempty_list(decl_statement)           { List.concat $1 }

/* as above, but allows a single expression - for "or" case */
exp_decl_statement_list:
    TNothing { [] } /* only in + code, between dots */
  | expr                                    { [Ast0.wrap(Ast0.Exp($1))] }
  | expr TOEllipsis b=statement_dots(TEllipsis) TCEllipsis
    exp_decl_statement_list
      /* HACK!!! */
    { (Ast0.wrap(Ast0.Exp($1)))::
      (Ast0.wrap(Ast0.Nest(P.clt2mcode "<..." $2,
			  Ast0.wrap(Ast0.DOTS(b (P.mkdots "..."))),
			  P.clt2mcode "...>" $4, None)))::
      $5 }
  | expr TOEllipsis b=statement_dots(TEllipsis) TCEllipsis
      /* HACK!!! */
    { [(Ast0.wrap(Ast0.Exp($1)));
	(Ast0.wrap(Ast0.Nest(P.clt2mcode "<..." $2,
			     Ast0.wrap(Ast0.DOTS(b (P.mkdots "..."))),
			     P.clt2mcode "...>" $4, None)))] }

  | pure_decl_statement_list                { $1 }

fun_exp_decl_statement_list:
    TNothing { [] } /* only in + code, between dots */
  | t=ctype
      /* This rule could be in exp_decl_statement_list, which would allow
         it to be ain a... sequence.  But it is not clear whether that makes
         sense, so for now it is here. */
      { [Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Ty(t))))] }
  | lp=TOPar0 t=midzero_list(ctype) rp=TCPar0
      /* more hacks */
    { let (mids,code) = t in
    let s =
      Ast0.wrap(Ast0.DisjType(P.clt2mcode "(" lp,code,mids, P.clt2mcode ")" rp)) in
    [Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Ty(s))))]}
  | expr                 { [Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Exp($1))))] }
  | expr TOEllipsis b=statement_dots(TEllipsis) TCEllipsis
    fun_exp_decl_statement_list
      /* HACK!!! */
    { (Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Exp($1)))))::
      (Ast0.wrap
	 (Ast0.OTHER
	    (Ast0.wrap
	       (Ast0.Nest(P.clt2mcode "<..." $2,
			  Ast0.wrap(Ast0.DOTS(b (P.mkdots "..."))),
			  P.clt2mcode "...>" $4, None)))))::
      $5 }

  | expr TOEllipsis b=statement_dots(TEllipsis) TCEllipsis
      /* HACK!!! */
    { [(Ast0.wrap(Ast0.OTHER(Ast0.wrap(Ast0.Exp($1)))));
      (Ast0.wrap
	 (Ast0.OTHER
	    (Ast0.wrap
	       (Ast0.Nest(P.clt2mcode "<..." $2,
			  Ast0.wrap(Ast0.DOTS(b (P.mkdots "..."))),
			  P.clt2mcode "...>" $4, None)))))] }

  | f=nonempty_list(fun_decl_statement)        { List.concat f }

fun_decl_statement:
    d=decl_statement { List.map (function x -> Ast0.wrap(Ast0.OTHER x)) d }
  | f=fundecl        { [Ast0.wrap(Ast0.DECL(f))] }

/* ---------------------------------------------------------------------- */

error_words:
    TError TWords TEq TOCro cl=comma_list(dexpr) TCCro
      { [Ast0.wrap(Ast0.ERRORWORDS(cl))] }

/* ---------------------------------------------------------------------- */
/* sequences of statements and expressions */

/* a mix of declarations, statements and expressions.  an expression may
appear by itself.  always nonempty and cannot just be dots.  allows fns too. */

minus_function_decl_statement_or_expression: /* doesn't allow just ... */
    opt_dot_start_end(fun_exp_decl_statement_list,
		      pre_post_decl_statement_or_expression,
		      rule_elem_statement,
		      fun_exp_decl_statement_list)
    { List.concat
	($1 (function x -> function y ->
	      [Ast0.wrap(Ast0.OTHER (P.mkdots x y))])) }

plus_function_decl_statement_or_expression: /* does allow just ... */
    first=fun_exp_decl_statement_list { first }
  | first=loption(fun_exp_decl_statement_list)
      second=required_dot_start_with_ender(fun_exp_decl_statement_list,
				    pre_post_decl_statement_or_expression,
				    rule_elem_statement,
				    fun_exp_decl_statement_list)
      { List.concat
	   (first ::
	    (second
	       (function x -> function y ->
		 [Ast0.wrap(Ast0.OTHER (P.mkdots x y))]))) }


/* a mix of declarations, statements and expressions.  an expression may
appear by itself.  always nonempty and cannot just be dots. */

pre_post_decl_statement_or_expression:
  opt_dot_start_end(exp_decl_statement_list,
		    pre_post_decl_statement_or_expression,
		    rule_elem_statement,
		    exp_decl_statement_list)
  { P.top_dots(List.concat ($1 (function x -> function y -> [P.mkdots x y]))) }

/* a mix of declarations, statements and expressions.  an expression must
be surrounded by ... */

pre_post_decl_statement_and_expression:
    first=pure_decl_statement_list { P.top_dots first }
  | first=loption(pure_decl_statement_list)
      second=required_dot_start_with_ender(exp_decl_statement_list,
				    pre_post_decl_statement_or_expression,
				    rule_elem_statement,
				    pure_decl_statement_list)
      { P.top_dots
	  (List.concat
	     (first::(second (function x -> function y -> [P.mkdots x y])))) }

pre_post_decl_statement_and_expression_opt:
    /* empty */                             { Ast0.wrap(Ast0.DOTS([])) }
  | pre_post_decl_statement_and_expression  { $1 }

pre_post_decl_statement_and_expression_opt_mid:
    pre_post_decl_statement_and_expression       { ($1,[]) }
  | /* empty */                          { (Ast0.wrap(Ast0.DOTS([])),[]) }
  | pre_post_decl_statement_and_expression TMid0
      pre_post_decl_statement_and_expression_opt_mid
      { let (first,rest) = $3 in
        ($1,(P.clt2mcode "|" $2,first)::rest) }
  | TMid0
      pre_post_decl_statement_and_expression_opt_mid
      { let (first,rest) = $2 in
        (Ast0.wrap(Ast0.DOTS([])),
	 (P.clt2mcode "|" $1,first)::rest) }

/* ---------------------------------------------------------------------- */

eexpr_list:
  eexpr_list_start
     {let circle x =
       match Ast0.unwrap x with Ast0.Ecircles(_) -> true | _ -> false in
     let star x =
       match Ast0.unwrap x with Ast0.Estars(_) -> true | _ -> false in
     if List.exists circle $1
     then Ast0.wrap(Ast0.CIRCLES($1))
     else
       if List.exists star $1
       then Ast0.wrap(Ast0.STARS($1))
       else Ast0.wrap(Ast0.DOTS($1)) }

/* arg expr.  may contain a type or a explist metavariable */
aexpr:
    dexpr
      { $1 }
  | TMetaExpList
      { let (nm,pure,clt) = $1 in
      Ast0.wrap(Ast0.MetaExprList(P.clt2mcode nm clt,pure)) }
  | generic_ctype
      { Ast0.wrap(Ast0.TypeExp($1)) }

eexpr_list_start:
    aexpr { [$1] }
  | aexpr TComma eexpr_list_start
      { $1::Ast0.wrap(Ast0.EComma(P.clt2mcode "," $2))::$3 }
  | d=edots_when(TEllipsis,eexpr)
	r=list(comma_args(edots_when(TEllipsis,eexpr)))
      { (P.mkedots "..." d)::
	(List.concat (List.map (function x -> x (P.mkedots "...")) r)) }
/*
  | d=edots_when(TCircles,eexpr)
	r=list(comma_args(edots_when(TCircles,eexpr)))
      { (P.mkedots "ooo" d)::
	(List.concat (List.map (function x -> x (P.mkedots "ooo")) r)) }
  | d=edots_when(TStars,eexpr)
	r=list(comma_args(edots_when(TStars,eexpr)))
      { (P.mkedots "***" d)::
	(List.concat (List.map (function x -> x (P.mkedots "***")) r)) }
*/

comma_args(dotter):
  c=TComma d=dotter
    { function dot_builder ->
      [Ast0.wrap(Ast0.EComma(P.clt2mcode "," c)); dot_builder d] }
| TComma aexpr
    { function dot_builder ->
      [Ast0.wrap(Ast0.EComma(P.clt2mcode "," $1)); $2] }

eexpr_list_option: eexpr_list { $1 }
         | /* empty */     { Ast0.wrap(Ast0.DOTS([])) }

/****************************************************************************/

// non-empty lists - drop separator
comma_list(elem):
  separated_nonempty_list(TComma,elem) { $1 }

midzero_list(elem):
  a=elem b=list(mzl(elem))
     { let (mids,code) = List.split b in (mids,(a::code)) }

mzl(elem):
  a=TMid0 b=elem { (P.clt2mcode "|" a, b) }

// SEQ1
// at least one instance of grammar/ender

opt_dot_start_end(grammar,when_grammar,simple_when_grammar,ender):
   start=ender { function dot_builder -> [start] }
 | r=opt_dot_start_end_pattern(grammar,
			       dots_when(TEllipsis,when_grammar,
					 simple_when_grammar),
     ender,opt_dot_end_dots(TEllipsis,grammar,when_grammar,
				simple_when_grammar,ender))
   { function dot_builder -> r (dot_builder "...") }
/*
 | r=opt_dot_start_end_pattern(grammar,dots_when(TCircles,when_grammar,
						 simple_when_grammar),
     ender,opt_dot_end_dots(TCircles,grammar,when_grammar,
			       simple_when_grammar,ender))
   { function dot_builder -> r (dot_builder "ooo") }
 | r=opt_dot_start_end_pattern(grammar,dots_when(TStars,when_grammar,
						 simple_when_grammar),
     ender,opt_dot_end_dots(TStars,grammar,when_grammar,
			     simple_when_grammar,ender))
   { function dot_builder -> r (dot_builder "***") }
*/

opt_dot_start_end_pattern(grammar,dotter,ender,continue):
   g=grammar d=dotter
     { function dot_builder -> [g; (dot_builder d)] }
 | g=grammar d=dotter c=continue
     { function dot_builder -> g :: (dot_builder d) :: (c dot_builder) }
 | d=dotter c=continue // continue is never empty
     { function dot_builder -> (dot_builder d) :: (c dot_builder) }

opt_dot_end_dots(dots,grammar,when_grammar,simple_when_grammar,ender):
   g=ender { function dot_builder -> [g] }
 | g=grammar d=dots_when(dots,when_grammar,simple_when_grammar)
     { function dot_builder -> [g ; dot_builder d ] }
 | g=grammar d=dots_when(dots,when_grammar,simple_when_grammar)
     r=opt_dot_end_dots(dots,grammar,when_grammar,simple_when_grammar,ender)
     { function dot_builder -> g :: (dot_builder d) :: (r dot_builder) }

// SEQ2, ender optional
required_dot_start_with_ender(grammar,when_grammar,
				      simple_when_grammar,ender):
 | start=dots_when(TEllipsis,when_grammar,simple_when_grammar)
     finish=no_dot_start_dots
       (TEllipsis,grammar,when_grammar,simple_when_grammar,ender)
   { (function dot_builder ->
       (dot_builder "..." start) :: (finish (dot_builder "..."))) }
/*
 | start=dots_when(TCircles,when_grammar,simple_when_grammar)
     finish=no_dot_start_dots
       (TCircles,grammar,when_grammar,simple_when_grammar,ender)
   { (function dot_builder ->
       (dot_builder "ooo" start) :: (finish (dot_builder "ooo"))) }
 | start=dots_when(TStars,when_grammar,simple_when_grammar)
     finish=no_dot_start_dots
       (TStars,grammar,when_grammar,simple_when_grammar,ender)
   { (function dot_builder ->
       (dot_builder "***" start) :: (finish (dot_builder "***"))) }
*/

no_dot_start_dots(dots,grammar,when_grammar,simple_when_grammar,ender):
   /* empty */    { function dot_builder -> [] }
 | e=ender
       { function dot_builder -> [e] }
 | g=grammar d=dots_when(dots,when_grammar,simple_when_grammar)
       r=no_dot_start_dots(dots,grammar,when_grammar,simple_when_grammar,ender)
       { function dot_builder -> g::(dot_builder d)::(r dot_builder) }

edots_when(dotter,when_grammar):
    d=dotter                                      { (d,None) }
  | d=dotter TWhen TNotEq w=when_grammar TLineEnd { (d,Some w) }

dots_when(dotter,when_grammar,simple_when_grammar):
    d=dotter w=list(whens(when_grammar,simple_when_grammar))
      { (d,w) }

whens(when_grammar,simple_when_grammar):
    TWhen TNotEq w=when_grammar TLineEnd { Ast0.WhenNot w }
  | TWhen TEq w=simple_when_grammar TLineEnd { Ast0.WhenAlways w }

// used in NEST
no_dot_start_end(grammar,dotter):
  g=grammar dg=list(pair(dotter,grammar))
  { function dot_builder ->
      g :: (List.concat(List.map (function (d,g) -> [dot_builder d;g]) dg)) }

/*****************************************************************************
*
*
*****************************************************************************/

iso_main:
  TIsoExpression e1=dexpr el=list(iso(dexpr)) EOF
    { P.iso_adjust (function x -> Ast0.ExprTag x) e1 el }
| TIsoStatement s1=single_statement sl=list(iso(single_statement)) EOF
    { P.iso_adjust (function x -> Ast0.StmtTag x) s1 sl }
| TIsoType t1=ctype tl=list(iso(ctype)) EOF
    { P.iso_adjust (function x -> Ast0.TypeCTag x) t1 tl }
| TIsoTopLevel e1=xstatement_dots(TEllipsis)
    el=list(iso(xstatement_dots(TEllipsis))) EOF
    { P.iso_adjust (function x -> Ast0.DotsStmtTag x) e1 el }
| TIsoDeclaration d1=decl_var dl=list(iso(decl_var)) EOF
    { let check_one = function
	[x] -> x
      | _ ->
	  raise
	    (Semantic_cocci.Semantic
	       "only one variable per declaration in an isomorphism rule") in
    let d1 = check_one d1 in
    let dl =
      List.map
	(function
	    Common.Left x -> Common.Left(check_one x)
	  | Common.Right x -> Common.Right(check_one x))
	dl in
    P.iso_adjust (function x -> Ast0.DeclTag x) d1 dl }

xstatement_dots(dotter): b=statement_dots(dotter)
    { Ast0.wrap(Ast0.DOTS(b (P.mkdots "..."))) }

iso(term):
    TIso t=term { Common.Left t }
  | TRightIso t=term { Common.Right t }

/*****************************************************************************
*
*
*****************************************************************************/

never_used: TPragma { () }
