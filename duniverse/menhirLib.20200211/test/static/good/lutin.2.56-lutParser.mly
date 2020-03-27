(* Original file: lutin.2.56/lutin.2.56/lutin/src/lutParser.mly *)
%{
(** SYNTAXE : analyse syntaxique

----------------------------------------------------------------*)
open Lexing
open Lexeme
open Syntaxe
open LutErrors
open Format
open Str


(* init parse *)

let lettab = ref (Hashtbl.create 50)
let nodetab = ref (Hashtbl.create 50)
let excepttab = ref (Hashtbl.create 50)
let deflist    = ref []
let included_files_to_handle = ref []

let add_info
   (htbl   : (string, 'a ) Hashtbl.t)
   (id  : Syntaxe.ident)   (* le lexeme en question        *)
   (info : 'a)         (* l'info en question           *)
= (
   try
      Some (Hashtbl.find htbl id.it)
   with Not_found -> (
      Hashtbl.add htbl id.it info ;
		None
   )
)


let add_except (idl : ident list) = (
  let f id = (
    match (add_info !excepttab id id) with
	None -> deflist := (ExceptDef id)  :: !deflist
      | Some ei -> raise (
	  Compile_error ( id.src ,
			  (sprintf "bad exception declaration, ident already linked %s"
			     (LutErrors.lexeme_line_col ei.src)))
	)
  ) in
    List.iter f idl
)

(* N.B. let et extern sont dans le meme espace de nom
	le seule différence est qu'on tient une liste à part des externes
*)
let add_extern (id : Syntaxe.ident) (ci : let_info) = (
	match ( add_info !lettab id ci) with
	None -> deflist := (ExternDef id) :: !deflist
	| Some mi -> raise (
		Compile_error ( id.src ,
			(sprintf "bad macro declaration, ident already linked %s"
				(LutErrors.lexeme_line_col mi.lti_ident.src))
      )
	)
)

let (add_include : Lexeme.t -> unit) =
  fun file ->
    (* remove '"' from string. *)
    let f = String.sub file.str 1 ((String.length file.str) - 2) in
    included_files_to_handle := (f)::!included_files_to_handle

let add_letdef (id : Syntaxe.ident) (ci : let_info) = (
	match ( add_info !lettab id ci) with
	None -> deflist := (LetDef id) :: !deflist
	| Some mi -> raise (
		Compile_error ( id.src ,
			(sprintf "bad macro declaration, ident already linked %s"
				(LutErrors.lexeme_line_col mi.lti_ident.src))
      )
	)
)

let add_node (id : Syntaxe.ident) (ci : node_info) = (
	match ( add_info !nodetab id ci ) with
	None -> deflist := (NodeDef id) :: !deflist
	| Some ni -> raise (
		Compile_error ( id.src ,
			(sprintf "bad node declaration, ident already linked %s"
				(LutErrors.lexeme_line_col ni.ndi_ident.src))
      )
	)
)

let distrib_type idlst texp = (
	let attach_type id = (id, texp) in
	List.map attach_type idlst
)

let distrib_type_and_init idlst texp range_opt vexp = (
	let attach_type id = (id, texp, vexp, range_opt) in
	List.map attach_type idlst
)

let parse_end () = (
   let res = {
      pck_lettab = !lettab;
      pck_nodetab  = !nodetab;
      pck_excepttab  = !excepttab;
      pck_deflist  = (List.rev !deflist);
      pck_included_files_to_handle = !included_files_to_handle
   } in
   lettab := Hashtbl.create 50;
   nodetab := Hashtbl.create 50;
   deflist := [];
   res
)

let ident_of_token lxm = (
	Lexeme.flagit lxm.str lxm
)

let make_val_exp ven lxm = {
	it = ven;
	src = lxm
}


%}

%token TK_EOF

%token <Lexeme.t> TK_ERROR

%token <Lexeme.t> TK_IDENT
%token <Lexeme.t> TK_STRING

%token <Lexeme.t> TK_LET
%token <Lexeme.t> TK_IN
%token <Lexeme.t> TK_EXTERN
%token <Lexeme.t> TK_NODE
%token <Lexeme.t> TK_SYSTEM
%token <Lexeme.t> TK_RETURNS

%token <Lexeme.t> TK_EXIST
%token <Lexeme.t> TK_RUN
%token <Lexeme.t> TK_ERUN
%token <Lexeme.t> TK_WEAK
%token <Lexeme.t> TK_STRONG
%token <Lexeme.t> TK_ASSERT
%token <Lexeme.t> TK_RAISE
%token <Lexeme.t> TK_TRY
%token <Lexeme.t> TK_CATCH
%token <Lexeme.t> TK_TRAP
%token <Lexeme.t> TK_PARA
%token <Lexeme.t> TK_DO
%token <Lexeme.t> TK_FBY
%token <Lexeme.t> TK_LOOP

%token <Lexeme.t> TK_TYPE
%token <Lexeme.t> TK_BOOL
%token <Lexeme.t> TK_INT
%token <Lexeme.t> TK_REAL
%token <Lexeme.t> TK_TRACE
%token <Lexeme.t> TK_REF
%token <Lexeme.t> TK_EXCEPTION
%token <Lexeme.t> TK_INCLUDE

%token <Lexeme.t> TK_PRE
%token <Lexeme.t> TK_FALSE
%token <Lexeme.t> TK_TRUE

%token <Lexeme.t> TK_RCONST
%token <Lexeme.t> TK_ICONST

%token <Lexeme.t> TK_EQ
%token <Lexeme.t> TK_NEQ

%token <Lexeme.t> TK_BARSUP

%token <Lexeme.t> TK_PLUS
%token <Lexeme.t> TK_MINUS
%token <Lexeme.t> TK_TIMES
%token <Lexeme.t> TK_SLASH

%token <Lexeme.t> TK_DIV
%token <Lexeme.t> TK_MOD

%token <Lexeme.t> TK_LT
%token <Lexeme.t> TK_LTE
%token <Lexeme.t> TK_GT
%token <Lexeme.t> TK_GTE

%token <Lexeme.t> TK_BAR

%token <Lexeme.t> TK_DOT
%token <Lexeme.t> TK_COMA
%token <Lexeme.t> TK_SEMICOL
%token <Lexeme.t> TK_COLON
%token <Lexeme.t> TK_TILDA
%token <Lexeme.t> TK_OPEN_BRACE
%token <Lexeme.t> TK_CLOSE_BRACE
%token <Lexeme.t> TK_OPEN_BRACKET
%token <Lexeme.t> TK_CLOSE_BRACKET
%token <Lexeme.t> TK_OPEN_PAR
%token <Lexeme.t> TK_CLOSE_PAR

%token <Lexeme.t> TK_NOT
%token <Lexeme.t> TK_OR
%token <Lexeme.t> TK_XOR
%token <Lexeme.t> TK_AND
%token <Lexeme.t> TK_IMPL
%token <Lexeme.t> TK_ASSIGN
%token <Lexeme.t> TK_ARROW
%token <Lexeme.t> TK_IF
%token <Lexeme.t> TK_THEN
%token <Lexeme.t> TK_ELSE


/* PRIORITIES */

/* dans les traces (statements) */
%nonassoc TK_TRY TK_IN
%left NO_DO_PART
%nonassoc TK_DO
%left TK_FBY
%nonassoc TK_LOOP


/* dans les expressions */
%left TK_ELSE
%nonassoc TK_IMPL
%left TK_OR
%left TK_XOR
%left TK_AND
%left TK_EQ TK_NEQ
%left TK_LT TK_LTE TK_GTE TK_GT
%left TK_PLUS TK_MINUS
%left TK_TIMES TK_SLASH TK_PCENT TK_MOD TK_DIV
%nonassoc TK_NOT
%nonassoc TK_INT TK_REAL
%nonassoc TK_UMINUS TK_PRE TK_DIESE TK_NOR
%left TK_HAT TK_FIELD TK_DOT

/* %nonassoc TK_OPEN_PAR */

/* affreux hack pour traiter le
   problème de loop ~id ( ...
	On met des priorités de telle
	manière que chaque fois qu'on a
	un '(' qui suit un id, on considère
	ça comme un (début) de call
*/
%nonassoc HACK_CALL
%nonassoc HACK_ID
%nonassoc TK_OPEN_PAR

/* %nonassoc TK_TILDA TK_COLON */

/* lutEntry point
*/
%start lutFileTop
%type <Syntaxe.package> lutFileTop


%%

/*-------------------------------------------------------
	GRAMMAR
---------------------------------------------------------
-------------------------------------------------------*/

/* ebnf:group=ignore */

lutFileTop:
	|   lutFile TK_EOF
			{ parse_end () }
	;

/* ebnf:group=decls */

/* lutDeclarations */

lutFile:
			{ }
	|   lutFile lutOneDecl
			{ }
	;


lutInclude:
  TK_INCLUDE TK_STRING
            { $2 }
;


lutOneDecl:
	|   lutInclude
		{ add_include $1 }
	|	lutLetDecl
		{ add_letdef (fst $1) (snd $1) }
	|	lutExceptDecl
		{ add_except $1 }
	|	lutExternNodeDecl
		{ add_extern (fst $1) (snd $1) }
	|	lutNodeDecl
		{ add_node (fst $1) (snd $1) }
	;

lutExceptDecl:
	TK_EXCEPTION lutIdentList
			{ List.rev $2 }
	;

/* top level macro def */
lutLetDecl:
  TK_LET lutIdent lutOptParams lutOptType TK_EQ lutTraceExp
  {
	  (* let id = ident_of_token $2 in *)
	  let id = $2 in
	  ( id,
		  {
			  lti_ident = id;
			  lti_inputs = $3;
			  lti_type = $4;
			  lti_def = Some $6;
		  }
	  )
  }
;

/* top level extern def */
lutExternNodeDecl:
  TK_EXTERN lutIdent lutOptParams lutOptType
  {
	  (* let id = ident_of_token $2 in *)
	  let id = $2 in
	  ( id,
		  {
			  lti_ident = id;
			  lti_inputs = $3;
			  lti_type = $4;
			  lti_def = None;
		  }
	  )
  }
;

/* top level node def */

lutNodeStart: /* ebnf:print=short */
	TK_NODE
		{}
|	TK_SYSTEM
		{}
;

lutNodeDecl:

	lutNodeStart lutIdent
	TK_OPEN_PAR lutTypedIdentListOpt TK_CLOSE_PAR
  TK_RETURNS TK_OPEN_PAR lutTypedIdentList TK_CLOSE_PAR
	TK_EQ lutTraceExp
	{
		(* let id = ident_of_token $2 in *)
		let id = $2 in
		(id,
			{
				ndi_ident = id;
				ndi_inputs = List.rev $4;
				ndi_outputs = List.rev $8;
				ndi_def = $11
			}
		)
	}
	;



/* lutIdentifiers and lists */

/* ebnf:group=varparams */

lutIdent: /* ebnf:print=ignore */
	TK_IDENT
		{ ident_of_token $1 }
	;

lutIdentList:
		lutIdent
			{ [$1] }
	|   lutIdentList TK_COMA lutIdent
			{ $3::$1 }
	;

/* opt parenthesis */
lutIdentTuple:
	lutIdentList
		{ List.rev $1 }
|  TK_OPEN_PAR lutIdentList TK_CLOSE_PAR
		{ List.rev $2 }
;

lutERunVars:
	TK_OPEN_PAR lutERunVarList TK_CLOSE_PAR
		{ $2 }
	|	lutERunVarList
		{ $1 }
	;

lutERunVarList:
	lutERunVar
		{ [$1] }
	| lutERunVarList TK_COMA lutERunVar
		{ $3::$1 }
	;


lutRangeOpt: /* ebnf:print=expand */
/* nada */
	{ None }
|	TK_OPEN_BRACKET lutExp TK_SEMICOL lutExp TK_CLOSE_BRACKET
	{ Some($2,$4) }
;

lutTypedIdent:
	lutIdentList TK_COLON lutType lutRangeOpt
		{ distrib_type_and_init $1 $3 $4  None}
|	lutIdentList TK_COLON lutType lutRangeOpt TK_EQ lutExp
		{ distrib_type_and_init $1 $3 $4 (Some $6) }
	;

lutTypedIdentListOpt: /* nada */
			{ [] }
	|	lutTypedIdentList
			{ $1 }
	;


lutTypedIdentList: lutTypedIdentListA
			{ $1 }
	|   lutTypedIdentListA TK_SEMICOL
			{ $1 }
	;

lutTypedIdentListA: /* ebnf:print=expand */  lutTypedIdent
			{ $1 }
	|   lutTypedIdentListA TK_SEMICOL lutTypedIdent
			{ $3@$1 }
	;

/*
lutTypedIdentList: lutTypedIdent
			{ $1 }
	|   lutTypedIdentList TK_SEMICOL lutTypedIdent lutOptSemicol
			{ $3@$1 }
	;

lutOptSemicol:
		{}
	| TK_SEMICOL
		{}
	;
*/
lutOptParams: /* ebnf:print=expand */
	  /* nada */
	  { None }
| TK_OPEN_PAR TK_CLOSE_PAR
	  { Some [] }
| TK_OPEN_PAR lutTypedParamList TK_CLOSE_PAR
		{ Some (List.rev $2) }
;

lutTypedParamList: lutTypedParam
	{ $1 }
|   lutTypedParamList TK_SEMICOL lutTypedParam
	{ $3@$1 }
;

lutTypedParam:
lutIdentList TK_COLON lutParamType
	{ distrib_type $1 $3}
;

lutERunVar:
	lutIdent lutOptType lutOptInit
		{ ($1, $2, $3) }
	;

lutOptInit:  /* ebnf:print=expand */
	/* nada */
		{ None }
	| TK_EQ lutExp
		{ Some $2 }
	;


/* Immediate type */

/* ebnf:group=types */

lutOptType:  /* ebnf:print=expand */
/* nada */
	{ None }
|	TK_COLON lutType
	{ Some $2 }
;


lutType: /* ebnf:print=short */
	lutPredefType
		{TEXP_predef $1}
|	TK_TRACE
		{ (TEXP_trace) }
;

lutPredefType: /* ebnf:print=short */
  TK_BOOL
	  { Bool }
|  TK_INT
	  { Int }
|  TK_REAL
	  { Real }
;

/* Parameter type */
lutParamType: /* ebnf:print=short */
	lutType
		{$1}
|	lutPredefType TK_REF
		{TEXP_ref $1}
;
/*

lutFunctionType:
		lutFunctionInType TK_ARROW lutPredefType
		{ (List.rev $1, TEXP_predef $3) }
	|	TK_OPEN_PAR TK_CLOSE_PAR TK_ARROW lutPredefType
		{ ([], TEXP_predef $4) }
;
lutFunctionInType:
		lutPredefType
		{ [TEXP_predef $1] }
	|  lutFunctionInType TK_TIMES lutPredefType
		{ (TEXP_predef $3)::$1 }
;
*/


/* TraceExp :
- lutTraceExp = a priori un statement, c'est-à-dire
  une expression parenthésée en {}
- lutExp = une expression algébrique classique
  parenthésée en ()
- cas particulier : les idents et plus généralement
  les "call" sont traités dans lutExp
  (voir + bas)
*/

/* ebnf:group=statements */

lutAssertFlag:  /* ebnf:print=expand */
	TK_STRONG
		{ Strong }
|	TK_WEAK
		{ Weak }
|	/* nada */
		{ Weak }
;

lutTraceExp:
/* ``feuilles''
*/
		lutExp
			{ $1 }
	|	TK_RAISE lutIdent
			{ make_val_exp (RAISE_n $2) $1 }
/* les combinateurs de traces referment dès que possible, e.g.:
	loop x fby y   <-->  {loop {x}} fby y
*/
	|	lutTraceExp TK_FBY lutTraceExp
			{ make_val_exp (FBY_n ($1,$3)) $2}
	|	lutLoopExp
			{ $1 }
	|	lutLoopStatExp
			{ $1 }
/*
	les combinateurs entre accolades
*/
	|	lutBraceExp
			{ $1 }
/*
	en général les statement en "in" referment AU PLUS TARD, e.g.
	assert e in x fby y fby
*/
	|	lutLetDecl TK_IN lutTraceExp
			{ make_val_exp (LET_n (snd $1,$3)) ((fst $1).src) }
	|	lutAssertFlag TK_ASSERT lutExp TK_IN lutTraceExp
			{ make_val_exp (ASSERT_n ($1,$3,$5)) $2 }
/* first run attemps: exist + run = erun */
	|	TK_ERUN lutERunVars TK_ASSIGN lutExp TK_IN lutTraceExp
			{ make_val_exp (ERUN_n (List.rev $2, $4, $6)) $1 }
/* RUN, id list already in right order */
	|	TK_RUN lutIdentTuple TK_ASSIGN lutExp TK_IN lutTraceExp
			{ make_val_exp (RUN_n ($2, $4, Some $6)) $1 }
/* RUN without "in" is a shortcut for "in loop { true }" */
	|	TK_RUN lutIdentTuple TK_ASSIGN lutExp
			{ make_val_exp (RUN_n ($2, $4, None)) $1 }
	|	TK_EXIST lutTypedIdentList TK_IN lutTraceExp
			{ make_val_exp (EXIST_n (List.rev $2,$4)) $1 }
	|	TK_EXCEPTION lutIdentList TK_IN lutTraceExp
			{ make_val_exp (EXCEPT_n (List.rev $2,$4)) $1 }
/*
	ceux qui ont une continuation
*/
/*
	|	TK_TRY lutTraceExp
			{ make_val_exp (TRY_n ($2,None)) $1 }
	|	TK_TRY lutTraceExp TK_DO lutTraceExp
			{ make_val_exp (TRY_n ($2,Some $4)) $1 }
	|	TK_CATCH lutIdent TK_IN lutTraceExp
			{ make_val_exp (CATCH_n($2,$4,None)) $1}
	|	TK_CATCH lutIdent TK_IN lutTraceExp TK_DO lutTraceExp
			{ make_val_exp (CATCH_n($2,$4,Some $6)) $1}
	|	TK_TRAP lutIdent TK_IN lutTraceExp
			{ make_val_exp (TRAP_n($2,$4,None)) $1}
	|	TK_TRAP lutIdent TK_IN lutTraceExp TK_DO lutTraceExp
			{ make_val_exp (TRAP_n($2,$4,Some $6)) $1}
*/
	|	TK_TRY lutTraceExp lutDoPart
			{ make_val_exp (TRY_n ($2,$3)) $1 }
	|	TK_CATCH lutIdent TK_IN lutTraceExp lutDoPart
			{ make_val_exp (CATCH_n($2,$4, $5)) $1}
	|	TK_TRAP lutIdent TK_IN lutTraceExp lutDoPart
			{ make_val_exp (TRAP_n($2,$4,$5)) $1}

/*
	|	TK_PARAHEAD lutTraceExp TK_PARA lutParaList1
			{ make_val_exp (PARA_n ($2::(List.rev $4))) $1 }
	|	TK_PARAHEAD TK_OPEN_BRACE lutTraceExp TK_PARA lutParaList1
				TK_CLOSE_BRACE
			{ make_val_exp (PARA_n ($3::(List.rev $5))) $1 }
*/
	;

lutDoPart :  /* ebnf:print=expand */
	/* nada */ %prec NO_DO_PART
		{ None }
	|	TK_DO lutTraceExp
		{ Some $2 }
	;

lutLoopExp:
	TK_LOOP lutTraceExp
		{  make_val_exp (LOOP_n (Weak,$2)) $1 }
|  TK_STRONG TK_LOOP lutTraceExp
		{  make_val_exp (LOOP_n (Strong,$3)) $2 }
|  TK_WEAK TK_LOOP lutTraceExp
		{  make_val_exp (LOOP_n (Weak,$3)) $2 }

;

lutLoopStatExp:
		TK_LOOP lutAverage lutTraceExp
			{ make_val_exp (LOOPI_n (fst $2, snd $2, $3)) $1 }
	|	TK_LOOP lutGaussian  lutTraceExp
			{ make_val_exp (LOOPA_n (fst $2, snd $2, $3)) $1 }
	;

/* lutExact:
		lutExp { ($1) } ;
*/

lutAverage:
	TK_OPEN_BRACKET lutExp TK_CLOSE_BRACKET
		{ ($2, $2) }
	| TK_OPEN_BRACKET lutExp TK_COMA lutExp TK_CLOSE_BRACKET
		{ ($2, $4) }
	;

lutGaussian:
		TK_TILDA lutExp %prec TK_NOT
		{ ($2, None) }
	|	TK_TILDA lutExp TK_COLON lutExp %prec TK_NOT
		{ ($2, Some $4) }
	;

/* VIELLE SYNTAXE */
/* OBSOLETE
lutWeightOptOBSO:
	//nada
		{ None }
	|	TK_WEIGHT lutExp
		{ Some {it = $2; src = $1} }
	;
lutChoiceList:
		lutTraceExp lutWeightOptOBSO
			{ [ ($1, $2) ] }
	|	lutChoiceList TK_BAR lutTraceExp lutWeightOptOBSO
			{ ($3,$4) :: $1 }
	;
lutPrioList:
	lutTraceExp
		{ [$1] }
	|	lutPrioList TK_BARSUP lutTraceExp
		{ $3::$1 }
	;
lutParaList:
		lutTraceExp
			{ [$1] }
	|	lutParaList TK_PARA lutTraceExp
			{ $3::$1 }
	;

lutBraceExpOBSO:
		TK_OPEN_BRACE lutTraceExp TK_CLOSE_BRACE
		{ $2 }
	|	TK_OPEN_BRACE lutTraceExp TK_BARSUP lutPrioList TK_CLOSE_BRACE
		{
			let args = $2::(List.rev $4) in
			make_val_exp (PRIO_n args) $1
		}
	|	TK_OPEN_BRACE TK_BARSUP lutPrioList TK_CLOSE_BRACE
		{
			let args = (List.rev $3) in
			make_val_exp (PRIO_n args) $1
		}
	|	TK_OPEN_BRACE lutTraceExp lutWeightOptOBSO TK_BAR lutChoiceList TK_CLOSE_BRACE
		{
			let args = ($2, $3)::(List.rev $5) in
			make_val_exp (CHOICE_n args) $1
		}
	|	TK_OPEN_BRACE TK_BAR lutChoiceList TK_CLOSE_BRACE
		{
			let args = (List.rev $3) in
			make_val_exp (CHOICE_n args) $1
		}
	|	TK_OPEN_BRACE lutTraceExp TK_PARA lutParaList TK_CLOSE_BRACE
		{
			let args = $2::(List.rev $4) in
			make_val_exp (PARA_n args) $1
		}
	|	TK_OPEN_BRACE TK_PARA lutParaList TK_CLOSE_BRACE
		{
			let args = (List.rev $3) in
			make_val_exp (PARA_n args) $1
		}
	;

*/

lutChoice:
		TK_BAR lutTraceExp
			{ [ ($2, None) ] }
	|	TK_BAR lutExp TK_COLON lutTraceExp
			{ let w = Some {it = $2; src = $3} in [ ($4, w) ] }
	|	lutChoice TK_BAR lutTraceExp
			{ ($3, None)::$1 }
	|	lutChoice TK_BAR lutExp TK_COLON lutTraceExp
			{ let w = Some {it = $3; src = $4} in ($5,w)::$1 }
	;

/* trop de conflits ...
lutWeightOpt:
	//nada
		{ None }
	|	lutExp TK_COLON
		{ Some {it = $1; src = $2} }
	;
lutChoice1:
		TK_BAR lutWeightOpt lutTraceExp
			{ [ ($3, $2) ] }
	|	lutChoice TK_BAR lutWeightOpt lutTraceExp
			{ ($4,$3)::$1 }
	;
*/

lutPrio:
		TK_BARSUP lutTraceExp
			{ [ $2 ] }
	|	lutPrio TK_BARSUP lutTraceExp
			{ $3::$1 }
	;

lutPara:
		TK_PARA lutTraceExp
			{ [ $2 ] }
	|	lutPara TK_PARA lutTraceExp
			{ $3::$1 }
	;


/* on peut omettre le premier symbole, mais du coup pas de poids possible pour Choice */
lutBraceExp:
		TK_OPEN_BRACE lutTraceExp TK_CLOSE_BRACE
		{ $2 }
	|	TK_OPEN_BRACE lutPrio TK_CLOSE_BRACE
		{
			let args = (List.rev $2) in
			make_val_exp (PRIO_n args) $1
		}
	|	TK_OPEN_BRACE lutTraceExp lutPrio TK_CLOSE_BRACE
		{
			let args = $2::(List.rev $3) in
			make_val_exp (PRIO_n args) $1
		}
	|	TK_OPEN_BRACE lutChoice TK_CLOSE_BRACE
		{
			let args = (List.rev $2) in
			make_val_exp (CHOICE_n args) $1
		}
	|	TK_OPEN_BRACE lutTraceExp lutChoice TK_CLOSE_BRACE
		{
			let args = ($2,None)::(List.rev $3) in
			make_val_exp (CHOICE_n args) $1
		}
	|	TK_OPEN_BRACE lutPara TK_CLOSE_BRACE
		{
			let args = (List.rev $2) in
			make_val_exp (PARA_n args) $1
		}
	|	TK_OPEN_BRACE lutTraceExp lutPara TK_CLOSE_BRACE
		{
			let args = $2::(List.rev $3) in
			make_val_exp (PARA_n args) $1
		}
	;


/* Expresssions algébriques classique s
*/


/* ebnf:group=expressions */

lutExp:
	/* identificateurs, constantes et call */
		lutConst { $1 }
	|	lutIdentRef { $1 }
	|	TK_PRE lutIdent { make_val_exp (PRE_n $2) $1 }

	/* simple parenthese */
	|	TK_OPEN_PAR lutExp TK_CLOSE_PAR { $2 }

	/* unaire */
	|	lutUnExp { $1 }

	/* opérateurs binaires infixés */
	|	lutBinExp { $1 }

	/* opérateurs ternaires */
	|	TK_IF lutExp TK_THEN lutExp TK_ELSE lutExp
			{ make_val_exp (CALL_n (flagit "ite" $1, [$2;$4;$6])) $1 }
	;

lutUnExp:
		TK_MINUS lutExp %prec TK_UMINUS { make_val_exp (CALL_n (flagit "uminus" $1, [$2])) $1 }
	|	TK_NOT lutExp { make_val_exp (CALL_n (flagit "not" $1, [$2])) $1 }
	;


lutBinExp:
		lutExp TK_EQ lutExp { make_val_exp (CALL_n (flagit "eq" $2, [$1;$3])) $2 }
	|	lutExp TK_NEQ lutExp { make_val_exp (CALL_n (flagit "neq" $2, [$1;$3])) $2 }
	|	lutExp TK_OR lutExp { make_val_exp (CALL_n (flagit "or" $2, [$1;$3])) $2 }
	|	lutExp TK_XOR lutExp { make_val_exp (CALL_n (flagit "xor" $2, [$1;$3])) $2 }
	|	lutExp TK_AND lutExp { make_val_exp (CALL_n (flagit "and" $2, [$1;$3])) $2 }
	|	lutExp TK_IMPL lutExp { make_val_exp (CALL_n (flagit "impl" $2, [$1;$3])) $2 }
	|	lutExp TK_PLUS lutExp { make_val_exp (CALL_n (flagit "plus" $2, [$1;$3])) $2 }
	|	lutExp TK_MINUS lutExp { make_val_exp (CALL_n (flagit "minus" $2, [$1;$3])) $2 }
	|	lutExp TK_TIMES lutExp { make_val_exp (CALL_n (flagit "times" $2, [$1;$3])) $2 }
	|	lutExp TK_SLASH lutExp { make_val_exp (CALL_n (flagit "slash" $2, [$1;$3])) $2 }
	|	lutExp TK_DIV lutExp { make_val_exp (CALL_n (flagit "div" $2, [$1;$3])) $2 }
	|	lutExp TK_MOD lutExp { make_val_exp (CALL_n (flagit "mod" $2, [$1;$3])) $2 }
	|	lutExp TK_LT lutExp { make_val_exp (CALL_n (flagit "lt" $2, [$1;$3])) $2 }
	|	lutExp TK_LTE lutExp { make_val_exp (CALL_n (flagit "lte" $2, [$1;$3])) $2 }
	|	lutExp TK_GT lutExp { make_val_exp (CALL_n (flagit "gt" $2, [$1;$3])) $2 }
	|	lutExp TK_GTE lutExp { make_val_exp (CALL_n (flagit "gte" $2, [$1;$3])) $2 }
	;

lutInteger: /* ebnf:print=ignore */
	TK_ICONST
		{ make_val_exp (ICONST_n (ident_of_token $1)) $1 }
	;

lutFloating: /* ebnf:print=ignore */
	TK_RCONST
		{ make_val_exp (RCONST_n (ident_of_token $1)) $1 }
	;

lutConst: /* ebnf:print=short */
		TK_TRUE
		{ make_val_exp TRUE_n $1 }
	|   TK_FALSE
		{ make_val_exp FALSE_n $1}
	|	lutInteger
		{ $1 }
	|	lutFloating
		{ $1 }
	;

/* statement ou exp */

/* ebnf:group=identref */

lutIdentRef:
		lutIdent %prec HACK_ID
			{ make_val_exp (IDENT_n $1) $1.src }
	|	lutIdent TK_OPEN_PAR  TK_CLOSE_PAR %prec HACK_CALL
			{ make_val_exp (CALL_n ($1, [])) $1.src }
	|	lutIdent TK_OPEN_PAR lutArgList TK_CLOSE_PAR %prec HACK_CALL
			{ make_val_exp (CALL_n ($1, List.rev $3)) $1.src }
	;


/*
lutArgListOpt:
			{ [] }
	|   lutArgList
			{ $1 }
	;
*/

lutArgList:
		lutArg
			{ [$1] }
	| lutArgList TK_COMA lutArg
			{ $3::$1 }
;

lutArg: lutTraceExp
		{ $1 }
;
