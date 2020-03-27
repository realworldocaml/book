/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: parse_xquery.mly,v 1.72 2007/11/16 21:16:52 mff Exp $ */

/*
 *  XQuery Parser
 */

%{

open Namespace_names
open Datatypes
open Datatypes_util

open Xquery_common_ast

open Xquery_ast
open Xquery_ast_util

open Xquery_type_ast
open Xquery_type_ast_util

open Error
open Finfo

open Format

(* Checks the XQuery version *)

let parsing_interface = ref false

let check_version s e =
  begin
    match e with
    | None -> ()
    | Some e ->
	ignore(Encoding.encoding_of_string e)
  end;
  begin
    if s = Conf.xquery_version
    then ()
    else raise (Query (Unknown ("XQuery version " ^ s ^ " not supported")))
  end

(* Checks consitency between opening and closing tags *)

let same_tag os cs =
  if (os = cs)
  then ()
  else
    raise (Query (Parsing (Finfo.parsing_locinfo (),
			   "Opening tag: "
			   ^ (Namespace_names.string_of_uqname os)
			   ^ " and closing tag: "
			   ^ (Namespace_names.string_of_uqname cs)
			   ^ " mismatched")))

(* Checks only one statement in main module *)

let single_statement () =
  if (!Conf.xquery_conformance)
  then
    raise (Query (Unknown "Main module should have a single expression"))
  else
    ()

(* In case of abbreviated syntax, tells you which axis to use from the nodekind test *)

let make_axis_from_nt nt =
  match nt with
  | (PNodeKindTest (AttributeKind _)) -> Attribute
  | _ -> Child

(* Deals with entity references in XQuery *)

let xquery_parse_context =
  let proc_ctxt = Processing_context.default_processing_context() in
  Parse_context.build_xquery_parse_context proc_ctxt

let wrap_p f =
  if Conf.is_xqueryp() || Conf.is_dxq()
  then f ()
  else raise (Query (Toplevel_Error "XQueryP tokens found, please use -language xqueryp or -language dxq"))

let wrap_dxq f =
  if Conf.is_dxq()
  then f ()
  else raise (Query (Toplevel_Error "DXQ tokens found, please use -language dxq"))

let check_pragma_content content =
  if (((String.length content) > 0) && (String.get content 0 != ' ')) then
    raise (Query (Parsing (Finfo.parsing_locinfo (),
			   "Pragma QName and content must be separated by a whitespace")))


%}

/**********/
/* tokens */
/**********/

%token <Namespace_names.uqname> XQNAME
%token <Namespace_names.ncname> XNCNAME
%token <Datatypes.xs_string> XSTRING

%token XMLPI XVERSION XSDDECL XSINGLEQUOTE XDOUBLEQUOTE
%token <string> XDECLNAME
%token <string> XENCODING

%token DOCTYPE DOCTYPECLOSE
%token DTDDECLOPEN DTDDECLCLOSE

/* XML whitespace */

%token S

/* XML markup tokens */

%token LOPENINGTAG LCLOSINGTAG
%token ROPENINGTAG RCLOSINGTAG
%token REMPTYELEMENT
%token <Datatypes.xs_untyped> TEXTLCLOSINGTAG
%token <Datatypes.xs_untyped> TEXTLOPENINGTAG
%token <Datatypes.xs_untyped> TEXTLCURLY

%token <Namespace_names.ncname> OPENINGPI
%token LOPENINGCOMMENT
%token <Datatypes.xs_untyped> TEXTCLOSINGPI
%token <Datatypes.xs_untyped> TEXTRCLOSINGCOMMENT
%token <Datatypes.xs_untyped * Namespace_names.ncname> TEXTOPENINGPI
%token <Datatypes.xs_untyped> TEXTLOPENINGCOMMENT

%token <Datatypes.xs_untyped> ATTRIBUTETEXT ATTRIBUTETEXTLCURLY

%token <Datatypes.xs_untyped * int> TEXTCHARREF
%token <Datatypes.xs_untyped * string> TEXTENTITYREF

/* DTD tokens */

%token <Namespace_names.ncname> PEREF
%token PERCENT
%token <Namespace_names.ncname> NMTOKEN
%token REQUIRED IMPLIED FIXED PCDATA
%token ELEMENTDECL ATTLISTDECL ENTITYDECL NOTATIONDECL
%token BEGINDECL ENDDECL BODYDECL

/* Query prolog tokens */

%token DECLAREDEFAULTCOLLATION DECLARENAMESPACE DECLAREBASEURI MODULENAMESPACE INTERFACENAMESPACE
%token DECLAREDEFAULTELEMENT DECLAREDEFAULTFUNCTION IMPORTSCHEMA IMPORTMODULE IMPORTINTERFACE IMPORTSERVICE
%token NAMESPACE DEFAULTELEMENT
%token DECLAREVALUEINDEX DECLARENAMEINDEX
%token DECLAREFUNCTION EXTERNAL DECLAREUPDATINGFUNCTION
%token DECLAREVARIABLE
%token XQUERYVERSION
%token ENCODING
%token PRESERVE NOPRESERVE INHERIT NOINHERIT STRIP
%token ORDERED UNORDERED
%token DECLARECONSTRUCTION DECLAREBOUNDARYSPACE DECLAREORDERING DECLAREOPTION DECLARESERVER DECLAREDEFAULTORDER DECLARECOPYNAMESPACES

/* Type declaration tokens */

%token DECLARESCHEMALCURLY
%token <Namespace_names.uqname> DECLAREATTRIBUTE
%token <Namespace_names.uqname> DECLAREELEMENT
%token <Namespace_names.uqname> DECLARESIMPLETYPE
%token <Namespace_names.uqname> DECLARECOMPLEXTYPE
%token <Namespace_names.uqname> DECLAREGROUP
%token <Namespace_names.uqname> DECLAREATTRGROUP
%token GROUP ATTRGROUP
%token OFTYPE OFSIMPLETYPE NILLABLE MIXED SUBSTITUTESFOR
%token RESTRICTS EXTENDS LISTOF UNIONOF

/* Constructors */

%token <Namespace_names.uqname> ATTRIBUTEQNAMECURLY ELEMENTQNAMECURLY
%token <Namespace_names.ncname> NAMESPACENCNAMECURLY PINCNAMECURLY
%token DOCUMENTCURLY TEXTCURLY PICURLY COMMENTCURLY
%token ATTRIBUTECURLY ELEMENTCURLY

%token <Namespace_names.uqname * string> PRAGMA

/* Sequence types */
%token DOCUMENTNODELPAR ELEMENTLPAR ATTRIBUTELPAR TEXTLPAR
%token SCHEMAELEMENTLPAR SCHEMAATTRIBUTELPAR
%token EMPTYSEQUENCELPAR TYPELPAR ITEMLPAR NUMERICLPAR ANYSTRINGLPAR

/* native type expressions */
%token ELEMENT ATTRIBUTE DOCUMENT
%token TYPE NONE QUESTION NODE ITEM TEXT PROCESSINGINSTRUCTION COMMENT

/* XQuery tokens */

%token ASCENDING DESCENDING EMPTY GREATEST LEAST
%token OR AND
%token STAR MULT DIV IDIV MOD MINUS PLUS
%token IPLUS ISTAR
%token <Namespace_names.ncname> STARNCNAME NCNAMESTAR
%token IN SATISFIES RETURN THEN ELSE TO WHERE
%token INTERSECT UNION EXCEPT
%token PRECEDES FOLLOWS
%token CASE INSTANCEOF DEFAULT
%token IFLPAR TYPESWITCHLPAR STABLEORDERBY ORDERBY COLLATION
%token ASSERTAS CASTAS CASTABLEAS TREATAS
%token VALIDATELCURLY VALIDATESTRICTLCURLY VALIDATELAXLCURLY
%token ORDEREDCURLY UNORDEREDCURLY
%token SOMEDOLLAR EVERYDOLLAR FORDOLLAR LETDOLLAR
%token AS AT

/* ULTF tokens */

%token DELETENODE INSERTNODE RENAMENODE REPLACENODE REPLACEVALUEOFNODE
%token ASFIRST ASLAST
%token INTO BEFORE AFTER
%token WITH

%token COPYDOLLAR MODIFY

/* XQuery! tokens */

%token COPYLCURLY SNAPLCURLY SNAPLCURLYORDERED

%token SNAPDELETENODE SNAPINSERTNODE SNAPRENAMENODE
%token SNAPREPLACENODE SNAPREPLACEVALUEOFNODE


/* XQuery! tokens for imperative features */
%token WHILELPAR LETVARDOLLAR SETDOLLAR

/* XQueryP tokens */
%token DECLAREDOLLAR

/* Names */

%token <Namespace_names.ncname> NCNAME
%token <Namespace_names.uqname> QNAME VARNAME QNAMEPLUS QNAMESTAR QNAMEQUESTION

/* XPath tokens */

%token <Xquery_common_ast.axis> AXIS
%token PROCESSINGINSTRUCTIONLPAR COMMENTLPAR NODELPAR

/* XQuery literals */

%token <Decimal._decimal> DECIMAL
%token <Decimal._integer> INT
%token <float> DOUBLE
%token <string> STRING

/* XQuery symbols */

%token COLON
%token SLASH SLASHSLASH DOT DOTDOT
%token EQ EQUALS
%token NE NOTEQUALS
%token IS
%token LTE GTE LTEQUALS GTEQUALS
%token COLONEQUALS
%token LTOP GTOP LT GT
%token BAR ATSIGN COMMA AMPERSAND SEMICOLON
%token LPAR RPAR IRPAR LBRACK RBRACK LCURLY RCURLY
%token DOLLAR
%token <Namespace_names.uqname> FUNCTIONNAMELPAR

/* Distributed XQuery (DXQ) */

%token LETSERVER IMPLEMENT FROMSERVER ATSERVER FORSERVER BOX EVALCLOSURE DO

/* Error or End of buffer/file */

%token LEXERROR
%token EOF


/**************/
/* precedence */
/**************/

%left COMMA
%left DELETENODE RENAMENODE AS REPLACENODE REPLACEVALUEOFNODE WITH INSERTNODE INTO ASFIRST ASLAST BEFORE AFTER
%left WHILE SETDOLLAR LETVARDOLLAR COLONEQUALS FORDOLLAR LETDOLLAR WHERE RETURN IFLPAR THEN ELSE SOMEDOLLAR EVERYDOLLAR SATISFIES TYPESWITCHLPAR DO BOX EVALCLOSURE
%left OR
%left AND
%left BAR
%left AMPERSAND
%left PRECEDES FOLLOWS EQ EQUALS NE NOTEQUALS IS LTOP GTOP LTE GTE LT GT LTEQUALS GTEQUALS
%left TO
%left PLUS MINUS
%left DIV IDIV MOD MULT
%left UNION EBAR
%left INTERSECT EXCEPT
%left INSTANCEOF
%left TREATAS
%left CASTABLEAS
%left CASTAS
%left SLASH SLASHSLASH
%left LBRACK RBRACK
/* %left LPAR RPAR */
%nonassoc UNARY
%nonassoc QUESTION
%nonassoc STAR
%nonassoc AXIS
%nonassoc ATSIGN

/****************/
/* Entry points */
/****************/


%start statement
%type <Xquery_ast.statement> statement

%start prolog
%type <Xquery_ast.prolog> prolog

%start xquerymodule
%type <Xquery_ast.xmodule> xquerymodule

%start librarymodule
%type <Xquery_ast.library_module> librarymodule

%start mainmodule
%type <Xquery_ast.main_module> mainmodule

%start interface
%type <Xquery_ast.interface> interface

%start extype
%type <(Xquery_type_ast.xtype option * Xquery_type_ast.xtype)> extype

%start stringlit
%type <string> stringlit

%%


/**********/
/* XQuery */
/**********/

xquerymodule:
  | mainmodule
      { EMainModule $1 }
  | librarymodule
      { ELibraryModule $1 }
;

opt_version:
  | /* empty */
      { () }
  | XQUERYVERSION STRING opt_encoding SEMICOLON
      { let version = $2 in
        let encoding = $3 in
        check_version version encoding }
;

opt_encoding:
  | /* empty */
      { None }
  | ENCODING STRING
      { let s = $2 in
        begin
	  ignore(Encoding.encoding_of_string s);
	  Some $2
	end }
;

mainmodule:
  | opt_version prolog statements EOF
      { let prolog = $2
        and statements = $3 in
(* Still allowing main modules without statements for now
        if statements = []
        then
	  raise (Query (Parsing (Finfo.parsing_locinfo (),
				 "Main module does not contain any statement")))
        else
*)
      { pmain_module_prolog     = prolog;
	pmain_module_statements = statements } }
;

librarymodule:
  | opt_version moduledecl prolog EOF
      { let moddecl = $2 in
        let prolog = $3 in
        { plibrary_module_decl = moddecl;
	  plibrary_module_prolog = prolog } }
;

moduledecl:
  | MODULENAMESPACE NCNAME EQUALS STRING opt_interface SEMICOLON
      { ($2,$4,$5) }
;
opt_interface:
  | /* empty */
      { None }
  |  IMPLEMENT STRING
      { Some ($2, None) }
  |  IMPLEMENT STRING AT STRING
      { Some ($2, Some $4) }
;

interface: opt_version interfacedecl prolog EOF
      { (* Woe is me --- hacking a global variable into the parser *)
        parsing_interface := false;
	let infdecl = $2 and
            prolog = $3
	in
	if (prolog.pprolog_indices != []) then
	  raise (Query (Parsing (Finfo.parsing_locinfo (),
				 "Interface may not contain indices")))
	else
	  let interface_prolog =
	    { iprolog_xschemas = prolog.pprolog_xschemas;
	      iprolog_contexts = prolog.pprolog_contexts;
	      iprolog_funcvars = prolog.pprolog_funcvars;
	    }
	  in
	  { pinterface_decl   = infdecl;
	    pinterface_prolog = interface_prolog
	  }
      }
;

interfacedecl:
  | INTERFACENAMESPACE NCNAME EQUALS STRING SEMICOLON
      { parsing_interface := true;
	($2,$4)
      }
;

prolog:
  | setters_and_first_declaration_list second_declaration_list
      { let (nsd1,nsd2,issds) = $1 in
        let (vfd,kd) = $2 in
	  { pprolog_xschemas = issds;
	    pprolog_contexts = nsd1@nsd2;
	    pprolog_funcvars = vfd;
	    pprolog_indices = kd } }
;

setters_and_first_declaration_list:
  |   /* empty */
      { ([],[],[]) }

  /* Boundary-space declaration */

  | DECLAREBOUNDARYSPACE STRIP SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $4 in
        (mkcontext_decl (EXmlSpaceDecl Strip) :: nsd,nsd2,issds) }
  | DECLAREBOUNDARYSPACE PRESERVE SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $4 in
        (mkcontext_decl (EXmlSpaceDecl Preserve) :: nsd,nsd2,issds) }

  /* Default collation declaration */

  | DECLAREDEFAULTCOLLATION STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $4 in
        (mkcontext_decl (EDefaultCollationDecl $2) :: nsd,nsd2,issds) }

  /* Base URI declaration */

  | DECLAREBASEURI STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $4 in
        (mkcontext_decl (EBaseURIDecl $2) :: nsd,nsd2,issds) }

  /* Construction declaration */

  | DECLARECONSTRUCTION STRIP SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $4 in
        (mkcontext_decl (EConstructionDecl Strip) :: nsd,nsd2,issds) }
  | DECLARECONSTRUCTION PRESERVE SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $4 in
        (mkcontext_decl (EConstructionDecl Preserve) :: nsd,nsd2,issds) }

  /* Ordering mode declaration */

  | DECLAREORDERING ORDERED SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $4 in
        (mkcontext_decl (EOrderingDecl Ordered) :: nsd,nsd2,issds) }
  | DECLAREORDERING UNORDERED SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $4 in
        (mkcontext_decl (EOrderingDecl Unordered) :: nsd,nsd2,issds) }

  /* Empty order declaration */

  | DECLAREDEFAULTORDER EMPTY GREATEST SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $5 in
        (mkcontext_decl (EDefaultEmptyOrderDecl EmptyGreatest) :: nsd,nsd2,issds) }

  | DECLAREDEFAULTORDER EMPTY LEAST SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $5 in
        (mkcontext_decl (EDefaultEmptyOrderDecl EmptyLeast) :: nsd,nsd2,issds) }

  /* Copy-namespaces declaration */

  | DECLARECOPYNAMESPACES PRESERVE COMMA INHERIT SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $6 in
        (mkcontext_decl (ECopyNamespacesDecl (NSPreserve,NSInherit)) :: nsd,nsd2,issds) }

  | DECLARECOPYNAMESPACES NOPRESERVE COMMA INHERIT SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $6 in
        (mkcontext_decl (ECopyNamespacesDecl (NSNoPreserve,NSInherit)) :: nsd,nsd2,issds) }

  | DECLARECOPYNAMESPACES PRESERVE COMMA NOINHERIT SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $6 in
        (mkcontext_decl (ECopyNamespacesDecl (NSPreserve,NSNoInherit)) :: nsd,nsd2,issds) }

  | DECLARECOPYNAMESPACES NOPRESERVE COMMA NOINHERIT SEMICOLON setters_and_first_declaration_list
      { let (nsd,nsd2,issds) = $6 in
        (mkcontext_decl (ECopyNamespacesDecl (NSNoPreserve,NSNoInherit)) :: nsd,nsd2,issds) }

  /* Default element namespace declaration */

  | DECLAREDEFAULTELEMENT NAMESPACE STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $5 in
        (nsd1,mkcontext_decl (EDefaultElementNamespaceDecl (NSUri $3)) :: nsd,issds) }

  /* Default function namespace declaration */

  | DECLAREDEFAULTFUNCTION NAMESPACE STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $5 in
        (nsd1,mkcontext_decl (EDefaultFunctionNamespaceDecl (NSUri $3)) :: nsd,issds) }

  /* Schema imports */

  | IMPORTSCHEMA STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $4 in
        (nsd1,mkcontext_decl (ESchemaDecl (None,$2,None)) :: nsd, issds) }

  | IMPORTSCHEMA STRING AT STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $6 in
        (nsd1,mkcontext_decl (ESchemaDecl (None,$2,Some $4)) :: nsd, issds) }

  | IMPORTSCHEMA NAMESPACE NCNAME EQUALS STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $7 in
        (nsd1,mkcontext_decl (ESchemaDecl (Some (NSPrefix $3),$5,None)) :: nsd, issds) }

  | IMPORTSCHEMA NAMESPACE NCNAME EQUALS STRING AT STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $9 in
        (nsd1,mkcontext_decl (ESchemaDecl (Some (NSPrefix $3),$5,Some $7)) :: nsd, issds) }

  | IMPORTSCHEMA DEFAULTELEMENT NAMESPACE STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $6 in
        (nsd1,mkcontext_decl (ESchemaDecl (Some NSDefaultElementPrefix,$4,None)):: nsd, issds) }

  | IMPORTSCHEMA DEFAULTELEMENT NAMESPACE STRING AT STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $8 in
        (nsd1,mkcontext_decl (ESchemaDecl (Some NSDefaultElementPrefix,$4,Some $6)):: nsd, issds) }

  /* Module imports */

  | IMPORTMODULE NAMESPACE NCNAME EQUALS STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $7 in
        (nsd1, mkcontext_decl (EImportModuleDecl ($3,$5, None)) :: nsd, issds) }

  | IMPORTMODULE NAMESPACE NCNAME EQUALS STRING AT STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $9 in
        (nsd1, mkcontext_decl (EImportModuleDecl ($3,$5, Some $7)) :: nsd, issds) }

  /* Interface imports */

  | IMPORTINTERFACE NAMESPACE NCNAME EQUALS STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $7 in
        (nsd1, mkcontext_decl (EImportInterfaceDecl ($3,$5, None)) :: nsd, issds) }

  | IMPORTINTERFACE NAMESPACE NCNAME EQUALS STRING AT STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $9 in
        (nsd1, mkcontext_decl (EImportInterfaceDecl ($3,$5, Some $7)) :: nsd, issds) }

  /* Service import */

  | IMPORTSERVICE NAMESPACE NCNAME EQUALS STRING setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $6 in
        (nsd1, mkcontext_decl (EImportServiceDecl ($3,$5, None)) :: nsd, issds) }

  | IMPORTSERVICE NAMESPACE NCNAME EQUALS STRING AT STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $9 in
        (nsd1, mkcontext_decl (EImportServiceDecl ($3,$5, Some $7)) :: nsd, issds) }

  /* Namespace declaration */

  | DECLARENAMESPACE NCNAME EQUALS STRING SEMICOLON setters_and_first_declaration_list
      { let (nsd1,nsd,issds) = $6 in
        (nsd1,mkcontext_decl (ENamespaceDecl ($2,NSUri $4)) :: nsd, issds) }

  /********************/
  /* Galax Extensions */
  /********************/

  /* Schema declarations */
  | schema_decl setters_and_first_declaration_list
      { let (nsd1,nsd,schemas) = $2 in
        let one_schema = $1 in
        (nsd1,nsd, one_schema :: schemas) }

;

second_declaration_list:
  /* Global variable declarations */
  |   /* empty */
      { ([],[]) }

  | DECLAREVARIABLE qname opt_type_declaration COLONEQUALS expr SEMICOLON second_declaration_list
      { let (vfd,kd) = $7 in
        let varname = $2 in
	let st = $3 in
        let expr = EVarUser (mkexpr (EList $5)) in
        let newvd = mkvar_decl (varname,st,expr) in
        ((VarDef newvd) :: vfd, kd) }

  | DECLAREVARIABLE qname opt_type_declaration EXTERNAL SEMICOLON second_declaration_list
      { let (vfd,kd) = $6 in
        let varname = match $2 with
          | (NSPrefix nc, lcname) -> if (!parsing_interface) then (NSInterfacePrefix nc, lcname) else $2
	  | _ -> if (!parsing_interface) then raise (Query(Parsing(Finfo.parsing_locinfo (),"Invalid QName"))) else $2
	in
	let st = $3 in
        let expr = if (!parsing_interface) then EVarInterface else EVarExternal in
        let newvd = mkvar_decl (varname,st,expr) in
        ((VarDef newvd) :: vfd, kd) }

  /* Function declarations */

  | start_declare_fun FUNCTIONNAMELPAR RPAR LCURLY block RCURLY SEMICOLON second_declaration_list
      {   let (vfd,kd) = $8 in
          let expr = $5 in
          let newfd = mkfunction_def ($2,[],([],None),EFunctionUser expr, $1) in
	    ((FunDef newfd) :: vfd, kd) }

  | start_declare_fun FUNCTIONNAMELPAR paramlist RPAR LCURLY block RCURLY SEMICOLON second_declaration_list
      {   let (vfd,kd) = $9 in
          let (input_types,input_vars) = $3 in
          let expr = $6 in
          let newfd = mkfunction_def ($2,input_vars,(input_types,None),EFunctionUser expr, $1) in
	    ((FunDef newfd) :: vfd, kd) }

  | start_declare_fun FUNCTIONNAMELPAR RPAR AS sequencetype LCURLY block RCURLY SEMICOLON second_declaration_list
      {   let (vfd,kd) = $10 in
          let expr = $7 in
          let newfd = mkfunction_def ($2,[],([],Some $5),EFunctionUser expr, $1) in
	    ((FunDef newfd) :: vfd, kd) }

  | start_declare_fun FUNCTIONNAMELPAR paramlist RPAR AS sequencetype LCURLY block RCURLY SEMICOLON second_declaration_list
      {
	let (vfd,kd) = $11 in
	let (input_types,input_vars) = $3 in
	let expr = $8 in
	let newfd = mkfunction_def ($2,input_vars,(input_types,Some $6),EFunctionUser expr, $1) in
	  ((FunDef newfd :: vfd), kd) }

/* External function declarations */

  | start_declare_fun FUNCTIONNAMELPAR RPAR AS sequencetype EXTERNAL SEMICOLON second_declaration_list
      { let (vfd,kd) = $8 in
        let funkind = if (!parsing_interface) then EFunctionInterface else EFunctionBltIn in
        let fname = match $2 with
          | (NSPrefix nc, lcname) -> if (!parsing_interface) then (NSInterfacePrefix nc, lcname) else $2
	  | _ -> if (!parsing_interface) then raise (Query(Parsing(Finfo.parsing_locinfo (),"Invalid QName"))) else $2
	in
        let newfd = mkfunction_def (fname,[],([],Some $5), funkind, $1) in
	((FunDef newfd :: vfd),kd) }

  | start_declare_fun FUNCTIONNAMELPAR paramlist RPAR AS sequencetype EXTERNAL SEMICOLON second_declaration_list
      { let (vfd,kd) = $9 in
        let (input_types,input_vars) = $3 in
        let funkind = if (!parsing_interface) then EFunctionInterface else EFunctionBltIn in
        let fname = match $2 with
          | (NSPrefix nc, lcname) -> if (!parsing_interface) then (NSInterfacePrefix nc, lcname) else $2
	  | _ -> if (!parsing_interface) then raise (Query(Parsing(Finfo.parsing_locinfo (),"Invalid QName"))) else $2
	in
        let newfd = mkfunction_def (fname,input_vars,(input_types,Some $6), funkind, $1) in
	((FunDef newfd :: vfd), kd) }

  | start_declare_fun FUNCTIONNAMELPAR RPAR EXTERNAL SEMICOLON second_declaration_list
      { let (vfd,kd) = $6 in
        let funkind = if (!parsing_interface) then EFunctionInterface else EFunctionBltIn  in
        let fname = match $2 with
          | (NSPrefix nc, lcname) -> if (!parsing_interface) then (NSInterfacePrefix nc, lcname) else $2
	  | _ -> if (!parsing_interface) then raise (Query(Parsing(Finfo.parsing_locinfo (),"Invalid QName"))) else $2
	in
        let newfd = mkfunction_def (fname,[],([],None), funkind, $1) in
	((FunDef newfd :: vfd), kd) }

  | start_declare_fun FUNCTIONNAMELPAR paramlist RPAR EXTERNAL SEMICOLON second_declaration_list
      { let (vfd,kd) = $7 in
        let (input_types,input_vars) = $3 in
        let funkind = if (!parsing_interface) then EFunctionInterface else EFunctionBltIn in
        let fname = match $2 with
          | (NSPrefix nc, lcname) -> if (!parsing_interface) then (NSInterfacePrefix nc, lcname) else $2
	  | _ -> if (!parsing_interface) then raise (Query(Parsing(Finfo.parsing_locinfo (),"Invalid QName"))) else $2
	in
        let newfd = mkfunction_def (fname,input_vars,(input_types,None), funkind, $1) in
	((FunDef newfd :: vfd), kd) }

  /*****************************************/
  /* DXQ Server implementation declaration */
  /*****************************************/
  | DECLARESERVER NCNAME IMPLEMENT NCNAME AT expr_single SEMICOLON second_declaration_list
      { let (vfd,kd) = $8 in
        let serverdef = mkserver_decl ($2,$4,$6)
	in wrap_dxq (fun () -> ((ServerDef serverdef)::vfd, kd))
      }
  /* Option declarations --- NOOPs right now. */
  | DECLAREOPTION qname STRING SEMICOLON second_declaration_list
      { let (vfd,kd) = $5 in
        ((OptionDecl ($2,$3)) :: vfd,kd) }

  /********************/
  /* Galax Extensions */
  /********************/

  /* Index declarations */
  | DECLAREVALUEINDEX STRING LCURLY expr RCURLY LCURLY expr RCURLY SEMICOLON second_declaration_list
      { let (vfd,kd) = $10 in
        let indexname = $2 in
        let expr1 = mkexpr (EList $4) in
        let expr2 = mkexpr (EList $7) in
        let newindex = mkindex_def (ValueIndex (indexname,expr1,expr2)) in
        (vfd, newindex :: kd) }

  /* Name indices: see physical_name_index.ml */
  | DECLARENAMEINDEX qname SEMICOLON second_declaration_list
      { let (vfd,kd) = $4 in
        let name = $2 in
        let newindex = mkindex_def (NameIndex name) in
        (vfd, newindex :: kd) }

;


/*********************/
/* XQuery statements */
/*********************/

statements:
  | /* no statement */
      { single_statement (); [] }
  | statement
      { $1 :: [] }
  | statement SEMICOLON statements
      { single_statement (); $1 :: $3  }
;

statement:
  | expr
      { mkexpr (EList $1) }
;

paramlist:
  | param
      { let (t,v) = $1 in
        (t :: [], v :: []) }
  | param COMMA paramlist
      { let (t,v) = $1
        and (pt,pv) = $3 in
        (t :: pt, v :: pv) }
;

param:
  | variable AS sequencetype
      { (Some $3,$1) }
  | variable
      { (None,$1) }
;


/**********************/
/* XQuery Expressions */
/**********************/

expr_single:
      /* Or expression */
  | expr_single OR expr_single
      { mkexpr (EBinaryOp($1,BEOr,$3)) }

      /* And expression */
  | expr_single AND expr_single
      { mkexpr (EBinaryOp($1,BEAnd,$3)) }

      /* Precedes Follows expressions */
  | expr_single PRECEDES expr_single
      { mkexpr (EBinaryOp($1,BEPrecedes,$3)) }
  | expr_single FOLLOWS expr_single
      { mkexpr (EBinaryOp($1,BEFollows,$3)) }

      /* FLWR expression */
  | for_let_clause where_order_clause return_clause
      { let fl = $1
        and (where,order) = $2
        and return = $3 in
        mkexpr (EFLWOR (fl,where,order,return)) }

      /* If expression */
  | IFLPAR expr RPAR THEN expr_single ELSE expr_single
      { let expr = mkexpr (EList $2) in
        mkexpr (EIf (expr, $5, $7)) }

      /* Some expression */
  | SOMEDOLLAR bindings SATISFIES expr_single
      { mkexpr (ESome ($2, $4)) }

      /* Every expression */
  | EVERYDOLLAR bindings SATISFIES expr_single
      { mkexpr (EEvery ($2, $4)) }

      /* Typeswitch expression */

  | TYPESWITCHLPAR expr RPAR case_clause_list
      { let expr = mkexpr (EList $2) in
        mkexpr (ETypeswitch (expr, $4)) }

      /* Equality expression */
  | expr_single EQ expr_single
      { mkexpr (EBinaryOp($1,BEEq,$3)) }
  | expr_single EQUALS expr_single
      { mkexpr (EBinaryOp($1,BEEqual,$3)) }
  | expr_single IS expr_single
      { mkexpr (EBinaryOp($1,BEIs,$3)) }
  | expr_single NE expr_single
      { mkexpr (EBinaryOp($1,BENEq,$3)) }

  | expr_single NOTEQUALS expr_single
      { mkexpr (EBinaryOp($1,BENEqual,$3)) }

      /* Relational expression */
  | expr_single LT expr_single
      { mkexpr (EBinaryOp($1,BELt,$3)) }
  | expr_single GT expr_single
      { mkexpr (EBinaryOp($1,BEGt,$3)) }
  | expr_single LTOP expr_single
      { mkexpr (EBinaryOp($1,BELtOp,$3)) }
  | expr_single GTOP expr_single
      { mkexpr (EBinaryOp($1,BEGtOp,$3)) }

  | expr_single LTE expr_single
      { mkexpr (EBinaryOp($1,BELte,$3)) }
  | expr_single GTE expr_single
      { mkexpr (EBinaryOp($1,BEGte,$3)) }

  | expr_single LTEQUALS expr_single
      { mkexpr (EBinaryOp($1,BELteq,$3)) }
  | expr_single GTEQUALS expr_single
      { mkexpr (EBinaryOp($1,BEGteq,$3)) }

      /* Instance Of expression */
  | expr_single INSTANCEOF sequencetype
      { mkexpr (EInstanceOf ($1,$3)) }

      /* Range expression */
  | expr_single TO expr_single
      { mkexpr (ERange ($1,$3)) }

      /* Additive expression */
  | expr_single PLUS expr_single
      { mkexpr (EBinaryOp($1,BEPlus,$3)) }
  | expr_single MINUS expr_single
      { mkexpr (EBinaryOp($1,BEMinus,$3)) }

      /* Multiplicative expression */
  | expr_single MULT expr_single
      { mkexpr (EBinaryOp($1,BEMult,$3)) }
  | expr_single DIV expr_single
      { mkexpr (EBinaryOp($1,BEDiv,$3)) }
  | expr_single IDIV expr_single
      { mkexpr (EBinaryOp($1,BEIDiv,$3)) }
  | expr_single MOD expr_single
      { mkexpr (EBinaryOp($1,BEMod,$3)) }

      /* Unary expression */
  | MINUS expr_single  %prec UNARY
      { mkexpr (EUnaryOp(UEMinus,$2)) }
  | PLUS expr_single   %prec UNARY
      { mkexpr (EUnaryOp(UEPlus,$2)) }

      /* Union expression */
  | expr_single UNION expr_single
      { mkexpr (EBinaryOp($1,BEUnion,$3)) }
  | expr_single BAR expr_single %prec EBAR
      { mkexpr (EBinaryOp($1,BEBar,$3)) }

      /* Intersect expression */
  | expr_single INTERSECT expr_single
      { mkexpr (EBinaryOp($1,BEIntersect,$3)) }
  | expr_single EXCEPT expr_single
      { mkexpr (EBinaryOp($1,BEExcept,$3)) }

      /* Castable expression */
  | expr_single CASTABLEAS singletype
      { mkexpr (ECastable ($1,$3)) }
  | expr_single CASTAS sequencetype
      { mkexpr (ECast ($1,$3)) }
  | expr_single TREATAS sequencetype
      { mkexpr (ETreat ($1,$3)) }

  | pathexpr
      { $1 }

      /* Updates expressions */
  | DELETENODE expr_single
      { mkexpr (EDelete (NoSnap,$2)) }
  | INSERTNODE expr_single INTO expr_single
      { mkexpr (EInsert (NoSnap,$2,EInto $4)) }
  | INSERTNODE expr_single ASLAST INTO expr_single
      { mkexpr (EInsert (NoSnap,$2,EAsLastInto $5)) }
  | INSERTNODE expr_single ASFIRST INTO expr_single
      { mkexpr (EInsert (NoSnap,$2,EAsFirstInto $5)) }
  | INSERTNODE expr_single AFTER expr_single
      { mkexpr (EInsert (NoSnap,$2,EAfter $4)) }
  | INSERTNODE expr_single BEFORE expr_single
      { mkexpr (EInsert (NoSnap,$2,EBefore $4)) }
  | RENAMENODE expr_single AS expr_single
      { mkexpr (ERename (NoSnap,$2,$4)) }
  | REPLACENODE expr_single WITH expr_single
      { mkexpr (EReplace (NoSnap, Normal_Replace, $2, $4)) }
  | REPLACEVALUEOFNODE expr_single WITH expr_single
      { mkexpr (EReplace (NoSnap, Value_Of_Replace, $2, $4)) }
  | transform_copy_clause modify_clause return_clause
      { mkexpr (ETransform ($1, $2, $3)) }

  /* XQueryP */
  | WHILELPAR expr_single RPAR RETURN expr_single
      { wrap_p (fun () -> mkexpr (EWhile($2, $5))) }
  | LETVARDOLLAR qname opt_type_declaration COLONEQUALS expr_single return_clause
      { wrap_p (fun () -> mkexpr(ELetvar($3,$2,$5,$6))) }
  | SETDOLLAR qname COLONEQUALS expr_single
      { wrap_p (fun() -> mkexpr(ESet($2,$4))) }

  /* DXQ */
  | LETSERVER NCNAME IMPLEMENT NCNAME AT expr_single RETURN expr_single
      { wrap_dxq (fun () -> mkexpr (ELetServerImplement($2,$4,$6,$8))) }
  | FROMSERVER NCNAME RETURN expr_single
      { wrap_dxq (fun () -> mkexpr (EExecute(false,$2,$4))) }
  | ATSERVER NCNAME DO expr_single
      { wrap_dxq (fun () -> mkexpr (EExecute(true,$2,$4))) }
  | FORSERVER NCNAME IMPLEMENT NCNAME BOX expr_single
      { wrap_dxq (fun () -> mkexpr (EForServerClose($2,$4,$6))) }
  | EVALCLOSURE expr_single
      { wrap_dxq (fun () -> mkexpr (EEvalClosure($2))) }
;

collation:
  | /* Empty */
      { None }
  | COLLATION STRING
      { Some $2 }
;

sortspeclist:
  | sortspec collation
      { let (a,b,c) = $1 in
        (a,b,c,$2) :: [] }
  | sortspec collation COMMA sortspeclist
      { let (a,b,c) = $1 in
        (a,b,c,$2) :: $4 }
;

sortspec:
  | expr_single
      { ($1,Ascending,None) }
  | expr_single ASCENDING
      { ($1,Ascending,None) }
  | expr_single DESCENDING
      { ($1,Descending,None) }
  | expr_single EMPTY GREATEST
      { ($1,Ascending,Some EmptyGreatest) }
  | expr_single ASCENDING EMPTY GREATEST
      { ($1,Ascending,Some EmptyGreatest) }
  | expr_single DESCENDING EMPTY GREATEST
      { ($1,Descending,Some EmptyGreatest) }
  | expr_single EMPTY LEAST
      { ($1,Ascending,Some EmptyLeast) }
  | expr_single ASCENDING EMPTY LEAST
      { ($1,Ascending,Some EmptyLeast) }
  | expr_single DESCENDING EMPTY LEAST
      { ($1,Descending,Some EmptyLeast) }
;

bindings:
  | qname opt_type_declaration IN expr_single
      {  [($2, $1, $4)] }
  | qname opt_type_declaration IN expr_single COMMA DOLLAR bindings
      {  ($2, $1, $4) :: $7 }
;

opt_positional_var:
  | /* No at variable */
      { None }
  | AT variable
      { Some $2 }
;

for_let_clause:
  | FORDOLLAR qname opt_type_declaration opt_positional_var IN expr_single for_clause
      { (mkfl_expr (EFor ($3, $2, $4, $6))) :: $7 }
  | LETDOLLAR qname opt_type_declaration COLONEQUALS expr_single let_clause
      { (mkfl_expr (ELet ($3, $2, $5))) :: $6 }
;

for_clause:
  | /* empty */
      { [] }
  | COMMA variable opt_type_declaration opt_positional_var IN expr_single for_clause
      { (mkfl_expr (EFor ($3, $2, $4, $6))) :: $7 }
  | for_let_clause
      { $1 }
;

let_clause:
  | /* empty */
      { [] }
  | COMMA variable opt_type_declaration COLONEQUALS expr_single let_clause
      { (mkfl_expr (ELet ($3, $2, $5))) :: $6 }
  | for_let_clause
      { $1 }
;

where_order_clause:
  | /* empty */
      { (None,None) }
  | where_clause
      { (Some $1,None) }
  | order_clause
      { (None, Some $1) }
  | where_clause order_clause
      { (Some $1, Some $2) }
;

where_clause:
  | WHERE expr_single
      { $2 }
;

order_clause:
  | ORDERBY sortspeclist
      { (NonStable, $2) }
  | STABLEORDERBY sortspeclist
      { (Stable, $2) }
;

return_clause:
  | RETURN expr_single
      { $2 }
;

opt_variable:
  | /* empty */
      { None }
  | variable
      { Some $1 }
;

opt_variable_as:
  | /* empty */
      { None }
  | variable AS
      { Some $1 }
;

case_clause_list:
  | CASE opt_variable_as sequencetype RETURN expr_single DEFAULT opt_variable RETURN expr_single
      { (mkpattern (Case ($3)), $2, $5) :: (mkpattern (Default), $7, $9) :: [] }
  | CASE opt_variable_as sequencetype RETURN expr_single case_clause_list
      { (mkpattern (Case ($3)), $2, $5) :: $6 }
;

pathexpr:
  | relativepathexpr
      { $1 }
  | SLASH
      { mkexpr ERoot }
  | SLASH relativepathexpr
      { match $2.pexpr_desc with
        | EPath(PSlash(e1, e2)) ->
            mkexpr (EPath(PSlash(mkexpr (EPath(PSlash(mkexpr ERoot, e1))), e2)))
	| EPath(PSlashSlash(e1, e2)) ->
            mkexpr (EPath(PSlashSlash(mkexpr (EPath(PSlash(mkexpr ERoot, e1))), e2)))
	| _ -> mkexpr (EPath (PSlash(mkexpr ERoot, $2))) }
  | SLASHSLASH relativepathexpr
      { match $2.pexpr_desc with
        | EPath(PSlash(e1, e2)) ->
            mkexpr(EPath(PSlash(mkexpr (EPath(PSlashSlash(mkexpr ERoot,e1))),e2)))
	| EPath(PSlashSlash(e1, e2)) ->
            mkexpr(EPath(PSlashSlash(mkexpr (EPath(PSlashSlash(mkexpr ERoot,e1))),e2)))
	| _ -> mkexpr (EPath (PSlashSlash(mkexpr ERoot, $2))) }
;

relativepathexpr:
  | stepexpr
      { $1 }
  | relativepathexpr SLASH stepexpr
      { mkexpr (EPath (PSlash($1, $3))) }
  | relativepathexpr SLASHSLASH stepexpr
      { mkexpr (EPath (PSlashSlash($1, $3))) }
;

stepexpr:
  | axisstepexpr
      { $1 }
  | otherstepexpr
      { $1 }
;

axisstepexpr:
  | AXIS nodetest
      { let axis = $1 in
        mkexpr (EPath(PAxis (axis,$2))) }
  | AXIS nodetest stepqualifiers
      { let axis = $1 in
        let axis_expr = mkexpr (EPath(PAxis (axis,$2))) in
        mkexpr (EPath(PStepQualifiers (false,axis_expr,$3))) }
  | ATSIGN nodetest
      { mkexpr (EPath(PAxis (Attribute,$2))) }
  | ATSIGN nodetest stepqualifiers
      { let axis_expr = mkexpr (EPath(PAxis (Attribute,$2))) in
        mkexpr (EPath(PStepQualifiers (false,axis_expr,$3))) }
;

otherstepexpr:
  | primaryexpr
      { $1 }
  | primaryexpr stepqualifiers
      { mkexpr (EPath (PStepQualifiers (true,$1,$2))) }
;

stepqualifiers:
  | LBRACK expr RBRACK
      { let expr = mkexpr (EList $2) in
        (mkstep_qualifier (PredicateQualifier expr)) :: [] }
  | LBRACK expr RBRACK stepqualifiers
      { let expr = mkexpr (EList $2) in
        (mkstep_qualifier (PredicateQualifier expr)) :: $4 }
;

transform_copy_clause:
  | COPYDOLLAR qname COLONEQUALS expr_single copyvar_clause
      { (mkcopyvar_expr ($2, $4)) :: $5 }

copyvar_clause:
  | /* empty */
      { [] }
  | COMMA variable COLONEQUALS expr_single copyvar_clause
      { (mkcopyvar_expr ($2, $4)) :: $5 }
;

modify_clause:
  | MODIFY expr_single
      { $2 }
;

primaryexpr:
      /* Self */
  | DOT
      { mkexpr ESelf }
      /* Parent */
  | DOTDOT
      { mkexpr EParent }
      /* Node test */
  | nodetest
      { let nt = $1 in
        let axis = make_axis_from_nt nt in
        mkexpr (EPath(PAxis (axis,$1))) }
      /* Variable */
  | variable
      { mkexpr (EVar $1) }
      /* Literal */
  | literal
      { $1 }
      /* Function call */
  | FUNCTIONNAMELPAR RPAR
      { mkexpr (EApp ($1,[])) }
  | FUNCTIONNAMELPAR expr RPAR
      { mkexpr (EApp ($1,$2)) }

      /* Parenthesized expression */
  | LPAR RPAR
      { mkexpr (EList []) }
  | LPAR expr RPAR
      { mkexpr (EList $2) }

      /* Validate expression */
  | VALIDATELCURLY expr RCURLY
      { let expr = mkexpr (EList $2) in
        mkexpr (EValidate (None,expr)) }
  | VALIDATESTRICTLCURLY expr RCURLY
      { let expr = mkexpr (EList $2) in
        mkexpr (EValidate (Some Strict,expr)) }
  | VALIDATELAXLCURLY expr RCURLY
      { let expr = mkexpr (EList $2) in
        mkexpr (EValidate (Some Lax,expr)) }

      /* Document node constructors */
  | DOCUMENTCURLY expr RCURLY
      { mkexpr (EDocument $2) }

      /* Text node constructors */
  | TEXTCURLY expr RCURLY
      { mkexpr (ETextComputed [mkexpr (EEnclosed (mkexpr (EList $2)))]) }

      /* Element Constructors */
  | ELEMENTQNAMECURLY expr RCURLY
      { mkexpr (EElemFixed ($1,[],$2)) }

  | ELEMENTQNAMECURLY RCURLY
      { mkexpr (EElemFixed ($1,[],[])) }

  | ATTRIBUTEQNAMECURLY expr RCURLY
      { mkexpr (EAttrFixed ($1,[mkexpr (EEnclosed (mkexpr (EList $2)))])) }

  | ATTRIBUTEQNAMECURLY RCURLY
      { mkexpr (EAttrFixed ($1,[])) }

      /* Computed element constructors */
  | ELEMENTCURLY expr RCURLY LCURLY expr RCURLY
      { mkexpr (EElemComputed ($2, $5)) }

  | ELEMENTCURLY expr RCURLY LCURLY RCURLY
      { mkexpr (EElemComputed ($2, [])) }

  | ATTRIBUTECURLY expr RCURLY LCURLY expr RCURLY
      { mkexpr (EAttrComputed ($2, $5)) }

  | ATTRIBUTECURLY expr RCURLY LCURLY RCURLY
      { mkexpr (EAttrComputed ($2, [])) }

      /* Escapes into XML */
    /* Empty element */
  | LOPENINGTAG qname attrlist REMPTYELEMENT
      { mkexpr (EElemFixed ($2, $3, [])) }

    /* Element having a content inside */
  | LOPENINGTAG qname attrlist ROPENINGTAG elementcontent qname opt_whitespace RCLOSINGTAG
      { let os = $2
	and cs = $6 in
        same_tag os cs;
        mkexpr (EElemFixed (os, $3, $5)) }

    /* Processing instruction */
  | OPENINGPI TEXTCLOSINGPI
      { mkexpr (EPI ($1,$2)) }

    /* Comment */
  | LOPENINGCOMMENT TEXTRCLOSINGCOMMENT
      { mkexpr (EComment $2) }

  | COMMENTCURLY expr RCURLY
      { mkexpr (ECommentComputed $2) }

  | PICURLY expr RCURLY LCURLY expr RCURLY
      { mkexpr (EPIComputed ($2, $5)) }

  | PINCNAMECURLY expr RCURLY
      { mkexpr (EPIComputed([mkexpr (EScalar (StringLiteral $1))], $2)) }

  | PINCNAMECURLY RCURLY
      { mkexpr (EPIComputed([mkexpr (EScalar (StringLiteral $1))], [])) }

    /* Ordered and Unordered expressions */
  | ORDEREDCURLY expr RCURLY
      { mkexpr (EOrdered $2) }
  | UNORDEREDCURLY expr RCURLY
      { mkexpr (EUnordered $2) }

    /* Extension expression */
  | extensionexpr
      { mkexpr (EList $1) }

    /* Kind of useful for updates expressions */
  | COPYLCURLY expr_single RCURLY
      { mkexpr (ECopy $2) }
  | SNAPLCURLY expr RCURLY
      { let expr = mkexpr (EList $2) in
	  mkexpr (ESnap (Snap_Unordered_Deterministic, expr)) }
  | SNAPLCURLYORDERED expr RCURLY
      { let expr = mkexpr (EList $2) in
	  mkexpr (ESnap (Snap_Ordered_Deterministic, expr)) }

      /* XQueryP */
  | LCURLY block RCURLY
      { wrap_p (fun () -> $2) }

;

/* Extension expression */

extensionexpr:
  | PRAGMA LCURLY expr RCURLY
      { let (name,content) = $1 in
        check_pragma_content content;
        mkexpr (EPragma(name,content,$3)) :: [] }
  | PRAGMA extensionexpr
      { let (name,content) = $1 in
        check_pragma_content content;
        mkexpr (EPragma(name,content,$2)) :: [] }
;


/* Expression sequence */

expr:
  | expr_single
      { $1 :: [] }
  | expr_single COMMA expr
      { $1 :: $3 }
;

/* XQueryP blocks */

block:
  | block_decl_list block_exprs
      { mkexpr(EBlock ($1,$2)) }
;

block_exprs:
  | expr SEMICOLON block_exprs
      { wrap_p (fun () -> (mkexpr (EList $1)) :: $3) }
  | expr
      { mkexpr (EList $1) :: [] }
;

block_sub_decl:
  | /* Empty */
      {[]}
  | COMMA variable opt_type_declaration COLONEQUALS expr_single block_sub_decl
      { (mkblock_decl_expr ($3,$2,$5)) :: $6 }
;

block_decl:
  | DECLAREDOLLAR qname opt_type_declaration COLONEQUALS expr_single block_sub_decl
      { (mkblock_decl_expr ($3,$2,$5)) :: $6 }
;

block_decl_list:
  | block_decl SEMICOLON block_decl_list
      { wrap_p (fun () -> $1 @ $3 )}
  |  /* no declaration */
      { [] }
;


/* Variable subrule */

variable:
  | VARNAME
      { $1 }
  | DOLLAR qname
      { $2 }
;

/* Literal subrules */

literal:
  | numericliteral
      { $1 }
  | stringliteral
      { $1 }
;

numericliteral:
  | integer
      { mkexpr (EScalar (IntegerLiteral $1)) }
  | decimal
      { mkexpr (EScalar (DecimalLiteral $1)) }
  | DOUBLE
      { mkexpr (EScalar (DoubleLiteral $1)) }
;

stringliteral:
  | STRING
      { mkexpr (EScalar (StringLiteral $1)) }
;

/* Subrules for node tests */

nodetest:
  | nametest
      { (PNameTest $1) }
  | kindtest RPAR
      { (PNodeKindTest ($1)) }
;

nametest:
  | qname
      { $1 }
  | wildcard
      { $1 }
;

qname:
  | NCNAME
      { (NSDefaultElementPrefix,$1) }
  | QNAME
      { $1 }
;

wildcard:
  | STAR
      { (NSWildcardPrefix,"*") }
  | NCNAMESTAR
      { (NSPrefix $1,"*") }
  | STARNCNAME
      { (NSWildcardPrefix, $1) }
;

/* Attributes inside element tags subrules */

attrlist:
  | /* empty */
      { [] }
  | S
      { [] }
  | S qname eq attributecontent attrlist
      { let previous_attributes = $5 in
        let qname = $2 in
        let real_att_content =
          let att_content = $4 in
	  if att_content = []
          then [(mkexpr (EText ""))]
	  else att_content
        in
        (mkexpr (EAttrFixed (qname,real_att_content))) :: previous_attributes }
;

attributecontent:
  | ATTRIBUTETEXT
      { let text = $1 in
        if text = "" then
	  []
	else
	  (mkexpr (EText $1)) :: [] }
  | ATTRIBUTETEXTLCURLY expr RCURLY attributecontent
      {
        (mkexpr (EText $1)) :: (mkexpr (EEnclosed (mkexpr (EList $2)))) :: $4
      }

    /* Character references */
  | TEXTCHARREF attributecontent
      { let (text,charref) = $1 in
        (mkexpr (EText text)) :: (mkexpr (ECharRef charref)) :: $2 }

    /* Entity references */
  | TEXTENTITYREF attributecontent
      { let (text,entityref) = $1 in
        let entity_ref_text = Parse_context.get_general_entity xquery_parse_context (Finfo.parsing_locinfo ()) entityref in
	(mkexpr (EText text)) :: (mkexpr (EText entity_ref_text)) :: $2 }
;

elementcontent:
  | TEXTLCLOSINGTAG
      { (mkexpr (EText $1)) :: [] }
  | LCLOSINGTAG
      { [] }

  | TEXTLCURLY expr RCURLY elementcontent
      { (mkexpr (EText $1)) :: (mkexpr (EEnclosed(mkexpr (EList $2)))) :: $4 }

  | TEXTLOPENINGTAG qname attrlist REMPTYELEMENT elementcontent
      { (mkexpr (EText $1)) :: (mkexpr (EElemFixed ($2, $3, []))) :: $5 }

  | TEXTLOPENINGTAG qname attrlist ROPENINGTAG elementcontent qname opt_whitespace RCLOSINGTAG elementcontent
      { let os = $2
	and cs = $6 in
        same_tag os cs;
        (mkexpr (EText $1)) :: (mkexpr (EElemFixed ($2, $3, $5))) :: $9 }

    /* Processing instruction */
  | TEXTOPENINGPI TEXTCLOSINGPI elementcontent
      { let (text,target_pi) = $1
	in (mkexpr (EText text)) :: (mkexpr (EPI (target_pi, $2))) :: $3 }

    /* Comment */
  | TEXTLOPENINGCOMMENT TEXTRCLOSINGCOMMENT elementcontent
      { (mkexpr (EText $1)) :: (mkexpr (EComment ($2))) :: $3 }

    /* Character references */
  | TEXTCHARREF elementcontent
      { let (text,charref) = $1 in
        (mkexpr (EText text)) :: (mkexpr (ECharRef charref)) :: $2 }

    /* Entity references */
  | TEXTENTITYREF elementcontent
      { let (text,entityref) = $1 in
        let entity_ref_text = Parse_context.get_general_entity xquery_parse_context (Finfo.parsing_locinfo ()) entityref in
        (mkexpr (EText text)) :: (mkexpr (EText entity_ref_text)) :: $2 }
;

/* Maybe some whitespace */

opt_whitespace:
  |
      { () }
  | S
      { () }
;

/* Equal sign in an attribute */

eq:
  | EQUALS { () }
  | S EQUALS { () }
  | EQUALS S { () }
  | S EQUALS S { () }
;


/**********************/
/* Update expressions */
/**********************/

start_declare_fun:
  | DECLAREFUNCTION
      { NonUpdating }
  | DECLAREUPDATINGFUNCTION
      { Updating }
  /* Events */
/*  | DECLAREEVENT NCNAME
      { raise (Failure "found event decl") } */
;


/*********/
/* Types */
/*********/

/* Syntax for types */
    /* Note: This is compliant with the latest consensus grammar - Jerome */


nillable:
  | /* Empty */
      { NonNillable }
  | NILLABLE
      { Nillable }
;

nested_stype_spec:
  | qname
      { mkstype_specifier (STypeRef $1) }
  | stype_derivation
      { mkstype_specifier (SAnonymous $1) }
;

stype_specifier_union:
  | NONE
      { [] }
  | nested_stype_spec
      { $1 :: [] }
  | nested_stype_spec BAR stype_specifier_union
      { $1 :: $3 }
;

stype_derivation:
  | RESTRICTS nested_stype_spec
      { SRestriction $2 }
  | LISTOF nested_stype_spec
      { SList $2 }
  | UNIONOF LCURLY stype_specifier_union RCURLY
      { SUnion $3 }
;

stype_specifier:
  | OFSIMPLETYPE qname
      { let ssd = STypeRef $2 in
        mkstype_specifier ssd }
  | stype_derivation
      { let ssd = SAnonymous $1 in
        mkstype_specifier ssd }
;

deriv_mixed:
  | /* Empty */
      { (None,NonMixed) }
  | MIXED
      { (None,Mixed) }
  | RESTRICTS qname
      { (Some (TRestriction $2), NonMixed) }
  | EXTENDS qname
      { (Some (TExtension $2), NonMixed) }
  | RESTRICTS qname MIXED
      { (Some (TRestriction $2), Mixed) }
  | EXTENDS qname MIXED
      { (Some (TExtension $2), Mixed) }
;

extype:
  | xtype
      { (None,$1) }
  | xtype SEMICOLON axtype
      { (Some $1,$3) }
;

ctype_derivation:
  | deriv_mixed LCURLY extype RCURLY
      { let (deriv,mixed) = $1 in
        let (attr_xtype,elem_xtype) = $3 in
        (deriv,attr_xtype,mixed,elem_xtype) }
;

ctype_specifier:
  | OFTYPE qname
      { let csd = TTypeRef $2 in
        mkctype_specifier csd }
  | ctype_derivation
      { let csd = TAnonymous $1 in
        mkctype_specifier csd }
;

xtype_specifier:
  | stype_specifier
      { TSpecSimple $1 }
  | ctype_specifier
      { TSpecComplex $1 }
;

axtype:
  | ATTRIBUTE qname
      { mkxtype (TAttributeRef ($2)) }
  | ATTRIBUTE nameclass stype_specifier
      { mkxtype (TAttributeLocal ($2,$3)) }
  | ATTRGROUP qname
      { mkxtype (TAttrGroupRef $2) }
  | axtype QUESTION
      { mkxtype (TBound ($1,Occurrence.occurs 0, Occurrence.occurs 1)) }
  | axtype COMMA axtype
      { mkxtype (TSequence ($1,$3)) }
  | LPAR RPAR
      { mkxtype TEmpty }
  | axtype AMPERSAND axtype
      { mkxtype (TInterleave ($1,$3)) }
  | LPAR axtype RPAR
      { $2 }
;

xtype:
  | qname
      { mkxtype (TAtomicRef $1) }
  | ELEMENT qname
      { mkxtype (TElementRef $2) }
  | ELEMENT nameclass nillable xtype_specifier
      { mkxtype (TElementLocal ($2, $3, $4)) }
  | DOCUMENT LCURLY xtype RCURLY
      { mkxtype (TDocument $3) }
  | TEXT
      { mkxtype (TText) }
  | PROCESSINGINSTRUCTION
      { mkxtype (TProcessingInstruction) }
  | COMMENT
      { mkxtype (TComment) }
  | GROUP qname
      { mkxtype (TGroupRef $2) }
  | ATTRGROUP qname
      { mkxtype (TAttrGroupRef $2) }
  | xtype STAR
      { mkxtype (TBound ($1,Occurrence.occurs 0, Occurrence.unbounded)) }
  | xtype PLUS
      { mkxtype (TBound ($1,Occurrence.occurs 1, Occurrence.unbounded)) }
  | xtype QUESTION
      { mkxtype (TBound ($1,Occurrence.occurs 0, Occurrence.occurs 1)) }
  | xtype COMMA xtype
      { mkxtype (TSequence ($1,$3)) }
  | LPAR RPAR
      { mkxtype TEmpty }
  | xtype BAR xtype
      { mkxtype (TChoice ($1,$3)) }
  | NONE
      { mkxtype TNone }
  | xtype AMPERSAND xtype
      { mkxtype (TInterleave ($1,$3)) }
  | LPAR xtype RPAR
      { $2 }
;

sequencetype:
  | EMPTYSEQUENCELPAR RPAR
      { mksequencetype(ITEmpty, None) }
  | qname_occurrence
      { $1 }
  | itemtype RPAR item_occurrence
      { mksequencetype($1, $3) }
;

item_occurrence:
  |
    { None }
  | ISTAR
      { Some(Occurrence.occurs 0, Occurrence.unbounded) }
  | IPLUS
      { Some(Occurrence.occurs 1, Occurrence.unbounded) }
  | QUESTION
      { Some(Occurrence.occurs 0, Occurrence.occurs 1) }
;

singletype:
  | QNAME
      { mksequencetype(ITAtomic $1,None) }
  | QNAMEQUESTION
      { mksequencetype(ITAtomic $1,Some(Occurrence.occurs 0, Occurrence.occurs 1)) }
;

opt_type_declaration:
  | /* Empty */
      { None }
  | AS sequencetype
      { Some $2 }
;

optelementref:
  | /* empty */
      { None }
    /* Global element reference */
  | STAR
      { None }
  | qname
      { (Some ($1,None)) }
  | STAR COMMA qname
      { (Some ((NSWildcardPrefix,"*"), Some $3)) }
  | qname COMMA qname
      { (Some ($1,Some $3)) }
;

optattributeref:
  | /* empty */
      { None }
    /* Global element reference */
  | STAR
      { None }
  | qname
      { (Some ($1,None)) }
  | STAR COMMA qname
      { (Some ((NSWildcardPrefix,"*"), Some $3)) }
  | qname COMMA qname
      { (Some ($1,Some $3)) }
;

qname_occurrence:
  | QNAME
      { mksequencetype(ITAtomic $1,None) }
  | QNAMESTAR
      { mksequencetype(ITAtomic $1,Some(Occurrence.occurs 0, Occurrence.unbounded)) }
  | QNAMEPLUS
      { mksequencetype(ITAtomic $1,Some(Occurrence.occurs 1, Occurrence.unbounded)) }
  | QNAMEQUESTION
      { mksequencetype(ITAtomic $1,Some(Occurrence.occurs 0, Occurrence.occurs 1)) }
;

documentnode_kindtest:
  | ELEMENTLPAR optelementref RPAR
      { (ElementTest $2) }
  | SCHEMAELEMENTLPAR qname RPAR
      { (SchemaElementTest $2) }

kindtest:
  | ELEMENTLPAR optelementref
      { ElementKind (ElementTest $2) }
  | SCHEMAELEMENTLPAR qname
      { ElementKind (SchemaElementTest $2) }
  | ATTRIBUTELPAR optattributeref
      { AttributeKind (AttributeTest $2) }
  | SCHEMAATTRIBUTELPAR qname
      { AttributeKind (SchemaAttributeTest $2) }
  | NODELPAR
      { AnyKind }
  | TEXTLPAR
      { TextKind }
  | COMMENTLPAR
      { CommentKind }
  | PROCESSINGINSTRUCTIONLPAR
      { PIKind None }
  | PROCESSINGINSTRUCTIONLPAR NCNAME
      { PIKind (Some $2) }
  | PROCESSINGINSTRUCTIONLPAR STRING
      { let ncname = Datatypes_util.normalize_pi_test $2 in PIKind (Some ncname) }
  | DOCUMENTNODELPAR
      { DocumentKind (None) }
  | DOCUMENTNODELPAR documentnode_kindtest
      { DocumentKind (Some ($2)) }
;

itemtype:
  | kindtest
      { ITKindTest $1 }
  | TYPELPAR qname
      { ITTypeRef $2 }
  | ITEMLPAR
      { ITItem }
  | NUMERICLPAR
      { ITNumeric }
  | ANYSTRINGLPAR
      { ITAnyString }
;

substitutes_for_nillable:
  | /* Empty */
      { (TNonSubstitutesFor,NonNillable) }
  | SUBSTITUTESFOR qname
      { (TSubstitutesFor $2,NonNillable) }
  | NILLABLE
      { (TNonSubstitutesFor,Nillable) }
  | SUBSTITUTESFOR qname NILLABLE
      { (TSubstitutesFor $2,Nillable) }
;

xelem_derivation:
  | substitutes_for_nillable xtype_specifier
      { let (sf,nil) = $1 in
        (sf,nil,$2) }
;

namespace_decls:
  | /* Empty */
      { [] }
  | DECLARENAMESPACE NCNAME EQUALS STRING SEMICOLON namespace_decls
      { ($2,NSUri $4) :: $6 }

;

type_decls:
  | /* Empty */
      { [] }
  | typedefinition type_decls
      { $1 :: $2 }
;

typedefinition:
  | DECLAREATTRIBUTE stype_specifier SEMICOLON
      { mkissd (TAttributeDecl ($1,$2)) }
  | DECLAREELEMENT xelem_derivation SEMICOLON
      { mkissd (TElementDecl ($1,$2)) }
  | DECLARESIMPLETYPE stype_derivation SEMICOLON
      { mkissd (TTypeDecl ($1,TSimpleDerivation $2)) }
  | DECLARECOMPLEXTYPE ctype_derivation SEMICOLON
      { mkissd (TTypeDecl ($1,TComplexDerivation $2)) }
  | DECLAREGROUP LCURLY xtype RCURLY SEMICOLON
      { mkissd (TGroupDecl ($1,$3)) }
  | DECLAREATTRGROUP LCURLY axtype RCURLY SEMICOLON
      { mkissd (TAttrGroupDecl ($1,$3)) }
;

schema_decl:
  | DECLARESCHEMALCURLY namespace_decls schema_decls type_decls RCURLY SEMICOLON
      { let nss = $2
        and schemas = $3
        and issds = $4 in
        fmkschema schemas nss issds }
;

schema_decls:
  | /* Empty */
      { [] }
  | schema_decl schema_decls
      { $1 :: $2 }
;

nameclass:
  | qname
      { $1 }
  | wildcard
      { $1 }
;

integer:
  | INT { $1 }
;

decimal:
  | DECIMAL { $1 }
;

stringlit:
  | STRING { $1 }
;
