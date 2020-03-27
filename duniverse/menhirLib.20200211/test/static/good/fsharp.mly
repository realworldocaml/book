%{
(* (c) Microsoft Corporation. All rights reserved *)
(*F# open Microsoft.Research.AbstractIL F#*)
(*F# open Microsoft.Research.AbstractIL.Internal F#*)
(*F# open Microsoft.FSharp.Compiler F#*)

open Range
open Ast
open Lib

let mk_optional m xopt =
    match xopt with
    | None -> mksyn_lid_get m Env.lib_MFCore_path "None"
    | Some x  -> Expr_app(mksyn_lid_get m Env.lib_MFCore_path "Some",x,m)

let mk_Do (strict,expr,m) =
    Binding (None,
             (if strict then DoBinding else StandaloneExpression),
             false,false,[],emptyXMLDoc,None,
             (if strict then Pat_const(Const_unit,m) else Pat_wild m),
             BindingExpr([],None,expr),m)

let mksyn_lazy_ty m ty =
  Type_app((path_to_lid m Env.lazy_path @ [mksyn_id m "Lazy"]),[ty],m)

let addAttribs attrs p =  Pat_attrib(p,attrs,range_of_synpat p)

let computeOverloadQualifier attrs =
  let attrs =
      attrs |> chooseList (fun attr ->
          match attr with
          | (Attr(lid,(Expr_const(Const_string (s,_),_) | Expr_paren(Expr_const(Const_string (s,_),_),_)),_,_)) ->
              begin match frontAndBack lid with
              | (_,{idText="OverloadID" | "OverloadIDAttribute"}) -> Some(s)
              | _ -> None
              end
          | _ -> None) in
  match attrs with
  | [x] -> Some x
  | [] -> None
  | _ -> failwith "Multiple OverloadID attributes"

(* error recovery*)
let arbExpr() = Expr_const(Const_unit,lhs())

let mksyn_lazy_status m e = Expr_recd(None,None, [ ((path_to_lid m Env.lazy_path, mksyn_id m "status"),e) ], m)

let mksyn_lazy (e,m) =
  Expr_typed(mksyn_lazy_status m (Expr_app(mksyn_lid_get m Env.lazystatus_path "Delayed",mksyn_delay m e,m)),
             mksyn_lazy_ty m (Type_anon m),m)

let mksyn_anon_constraint ty m = Type_anon_constraint(ty,m)

let parse_error s = errorR(Error(s,curr_lex_range())) (* returning initiates error recovery *)
let report_parse_warning s = warning(Error(s,curr_lex_range()))
let report_parse_error_at m s = errorR(Error(s,m))

let report_parse_warning_at m s = warning(Error(s,m))

(* OCaml generated parsers seem to create corrupted terms if parse error are raised when an "error" symbol is being shifted *)
let raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m s = report_parse_error_at m s; raise Parsing.Parse_error

let check_eof_error_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY res t =
  match t with
  | AT_ifdef_skip(_,m) -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m "end of file in #if section begun at or after here"
  | AT_string m ->  raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m "end of file in string begun at or before here"
  | AT_vstring m ->  raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m "end of file in verbatim string begun at or before here"
  | AT_comment (_,m) ->  raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m "end of file in comment begun at or before here"
  | AT_comment_string (_,m) -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m "end of file in string embedded in comment begun at or before here"
  | AT_camlonly m -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m "end of file in IF-OCAML section begun at or before here"
  | AT_token -> res

(* note: defns moved to ast.ml *)
let grabXML ()      = Ast.grabXML ()

let mkClassMemberLocalBindings isStatic wholem attrs vis (_,isRec,isUse,declsPreAttrs) =
   let ignoredFreeAttrs,decls = declsPreAttrs attrs vis in
   if nonNil ignoredFreeAttrs then warning(Error("attributes have been ignored in this construct",wholem));
   if isUse then warning(Error("'use' bindings are not permitted in implicit class constructors",wholem));
   ClassMemberDefn_let_bindings (decls,isStatic,isRec,wholem)

let mkLocalBindings wholem (_,isRec,isUse,declsPreAttrs) body =
   let ignoredFreeAttrs,decls = declsPreAttrs [] None in
   if nonNil ignoredFreeAttrs then warning(Error("attributes have been ignored in this construct",wholem));
   Expr_let (isRec,isUse,decls,body,wholem)

let mkDefnBindings wholem (_,isRec,isUse,declsPreAttrs) attrs vis attrsm =
   if isUse then warning(Error("'use' bindings are treated as 'let' bindings in modules",wholem));
   let freeAttrs,decls = declsPreAttrs attrs vis in
   let letDecls = [ Def_let (isRec,decls,wholem) ] in
   let attrDecls = if nonNil freeAttrs then [ Def_attributes (freeAttrs,attrsm) ] else [] in
   attrDecls @ letDecls

let mkComprehensionBindings wholem (_,isRec,isUse,declsPreAttrs) rest =
   let ignoredFreeAttrs,decls = declsPreAttrs [] None in
   if nonNil ignoredFreeAttrs then warning(Error("attributes have been ignored in this construct",wholem));
   if isRec then errorR(Error("recursive bindings are not permitted in comprehensions",wholem));
   match decls with
   | [] -> error(Error("unexpected empty declarations",wholem))
   | [Binding (vis,bindingKind,pseudo,mut,attrs,doc,memberInfo,pat,(BindingExpr(spatsL,rtyOpt,expr)),bindm)] ->
        if nonNil(attrs) && bindingKind = StandaloneExpression then error(Error("expressions may not have attributes",bindm));
        if isSome rtyOpt then warning(Error("this type attribute is currently ignored",bindm));
        if nonNil spatsL then error(Error("functions may not be defined in comprehensions",bindm));
        if isSome memberInfo then error(InternalError("unexpected member comprehension",bindm));
        if pseudo then error(Error("inline values may not be defined in comprehensions",bindm));
        if isSome vis then warning(Error("visibility attributes are ignored on declarations in comprehensions",bindm));
        if mut then warning(Error("declarations in comprehensions may not be declared mutable",bindm));
        if nonNil attrs then warning(Error("attributes have been ignored in this construct",bindm));
        Comp_bind(isUse,None,Some pat,expr,rest)
   | _ -> error(Error("multiple binding declarations may not be used in comprehensions",wholem))

let id_of_pat m p = match p with Pat_as (Pat_wild _,id,false,_,_) -> id | _ -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m "A integer for loop must use a simple identifier"

let checkForMultipleAugmentations m a1 a2 =
        if nonNil a1 && nonNil a2 then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m "at most one 'with' augmentation is permitted";
        a1 @ a2

%}

%token <Bytes.bytes> BYTEARRAY
%token <Bytes.bytes> STRING
%token <string> IDENT
%token <string> INFIX_STAR_STAR_OP
%token <string> INFIX_COMPARE_OP
%token <string> INFIX_AT_HAT_OP
%token <string> INFIX_BAR_OP
%token <string> PREFIX_OP
%token <string> INFIX_STAR_DIV_MOD_OP
%token <string> INFIX_AMP_OP
%token <string> PLUS_MINUS_OP
%token <string> ADJACENT_PREFIX_PLUS_MINUS_OP
%token <string> FUNKY_OPERATOR_NAME

%token <Nums.i8> INT8
%token <Nums.u8> UINT8
%token <Nums.i16> INT16
%token <Nums.u16> UINT16
%token <int32> INT32 INT32_DOT_DOT
%token <Nums.u32> UINT32
%token <int64> INT64
%token <Nums.u64> UINT64
%token <Nums.u64> UNATIVEINT
%token <int64> NATIVEINT
%token <Nums.ieee32> IEEE32
%token <Nums.ieee64> IEEE64
%token <Bytes.bytes> DECIMAL
%token <Nums.unichar> CHAR
%token <Bytes.bytes> BIGINT BIGNUM
%token <bool> LET YIELD YIELD_BANG
%token <string> SPLICE_SYMBOL PERCENT_OP BINDER HASH_KEYWORD
%token <string * bool> LQUOTE RQUOTE
%token BAR_BAR LESS GREATER UPCAST DOWNCAST NULL RESERVED MODULE NAMESPACE DELEGATE CONSTRAINT
%token AND AS ASSERT ASR BEGIN DO DONE DOWNTO ELSE ELIF END DOT_DOT
%token EXCEPTION FALSE FOR FUN FUNCTION IF IN FINALLY DO_BANG
%token LAZY  MATCH METHOD MUTABLE NEW OF
%token OPEN OR REC THEN TO TRUE TRY TYPE VAL INLINE INTERFACE INSTANCE
%token WHEN WHILE WITH HASH AMP AMP_AMP QUOTE LPAREN RPAREN STAR COMMA RARROW RARROW2 GREATER_DOT
%token QMARK QMARK_QMARK DOT COLON COLON_COLON COLON_GREATER  COLON_QMARK_GREATER COLON_QMARK COLON_EQUALS SEMICOLON
%token SEMICOLON_SEMICOLON LARROW EQUALS  LBRACK  LBRACK_BAR  LBRACK_LESS LBRACE
%token LBRACE_LESS BAR_RBRACK GREATER_RBRACE UNDERSCORE
%token BAR RBRACK RBRACE MINUS DOLLAR
%token GREATER_RBRACK STRUCT SIG
%token STATIC MEMBER CLASS VIRTUAL ABSTRACT OVERRIDE DEFAULT CONSTRUCTOR INHERIT
%token EXTERN VOID PUBLIC PRIVATE INTERNAL

/* for high-precedence tyapps and apps */
%token HIGH_PRECEDENCE_APP   /* inserted for f(x), but not f (x) */
%token HIGH_PRECEDENCE_TYAPP /* inserted for x<y>, but not x<y */

/* for offside rule */
%token <bool> OLET      /* LexFilter #light converts 'LET' tokens to 'OLET' when starting (CtxtLetDecl(blockLet=true)) */
%token <string> OBINDER /* LexFilter #light converts 'BINDER' tokens to 'OBINDER' when starting (CtxtLetDecl(blockLet=true)) */
%token ODO              /* LexFilter #light converts 'DO' tokens to 'ODO' */
%token ODO_BANG         /* LexFilter #light converts 'DO_BANG' tokens to 'ODO_BANG' */
%token OTHEN            /* LexFilter #light converts 'THEN' tokens to 'OTHEN' */
%token OELSE            /* LexFilter #light converts 'ELSE' tokens to 'OELSE' except if immeditely followed by 'if', when they become 'ELIF' */
%token OWITH            /* LexFilter #light converts SOME (but not all) 'WITH' tokens to 'OWITH' */
%token OFUNCTION        /* LexFilter #light converts 'FUNCTION' tokens to 'OFUNCTION' */
%token OFUN             /* LexFilter #light converts 'FUN' tokens to 'OFUN' */


%token ORESET           /* LexFilter uses internally to force a complete reset on a ';;' */

%token OBLOCKBEGIN      /* LexFilter #light inserts for:
                                  - just after first '=' or ':' when in 'CtxtModuleHead', i.e. after 'module' and sequence of dot/identifier/access tokens
                                  - just after first '=' when in 'CtxtMemberHead'
                                  - just after first '=' when in 'CtxtType'
                                  - just after 'do' in any context (when opening CtxtDo)
                                  - just after 'finally' in any context
                                  - just after 'with' (when opening CtxtWithAsAugment)
                                  - just after 'else' (when opening CtxtElse)
                                  - just after 'then' (when opening CtxtThen)
                                  - just after 'interface' (when pushing CtxtParen(INTERFACE), i.e. next token is DEFAULT | OVERRIDE | INTERFACE | NEW | TYPE | STATIC | END | MEMBER | ABSTRACT  | INHERIT | LBRACK_LESS)
                                  - just after 'class' (when pushing CtxtParen(CLASS)
                                  - just after 'class'
                           But not when opening these CtxtSeqBlocks:
                                  - just after first non-dot/identifier token past 'namespace'
                                  - just after first '=' when in 'CtxtLetDecl' or 'CtxtWithAsLet'
                                  - just after 'lazy' in any context
                                  - just after '->' in any context
                                  - when opening CtxtNamespaceHead, CtxtModuleHead
                        */
%token OBLOCKSEP        /* LexFilter #light inserts when transforming CtxtSeqBlock(NotFirstInSeqBlock,_,AddBlockEnd) to CtxtSeqBlock(FirstInSeqBlock,_,AddBlockEnd) on exact alignment */

/*    TODO: merge OEND, OBLOCKEND and ORIGHT_BLOCK_END into one token, and possibly ODECLEND too */
%token OEND             /* LexFilter #light inserts when closing CtxtFun, CtxtMatchClauses, CtxtWithAsLet _        */
%token ODECLEND         /* LexFilter #light inserts when closing CtxtDo and CtxtLetDecl(block) */
%token ORIGHT_BLOCK_END /* LexFilter #light inserts when closing CtxtSeqBlock(_,_,AddOneSidedBlockEnd) */
%token OBLOCKEND        /* LexFilter #light inserts when closing CtxtSeqBlock(_,_,AddBlockEnd) */

%token OINTERFACE_MEMBER /* inserted for non-paranthetical use of 'INTERFACE', i.e. not INTERFACE/END */
%token <token> ODUMMY

/* These are artificial */
%token <string> LEX_FAILURE
%token <Ast.lexcont> COMMENT WHITESPACE LINE_COMMENT STRING_TEXT EOF

%start signatureFile implementationFile interaction
%type <Ast.impl list> implementationFile
%type <Ast.intf list> signatureFile
%type <Ast.interaction> interaction
%type <Ast.ident> ident
%type <Ast.typ> typ
%type <Ast.tyconSpfn list> tyconSpfns
%type <Ast.synexpr> declExpr
%type <Ast.synpat> headBindingPattern


/* About precedence rules:
 *
 * Tokens and dummy-terminals are given precedence below (lowest first).
 * A rule has precedence of the first token or the dummy terminal given after %prec.
 * The precedence resolve shift/reduce conflicts:
 *   (a) If either rule has no precedence:
 *       S/R: shift over reduce, and
 *       R/R: reduce earlier rule over later rule.
 *   (b) If both rules have precedence:
 *       S/R: choose highest precedence action (precedence of reduce rule vs shift token)
 *            if same precedence: leftassoc gives reduce, rightassoc gives shift, nonassoc error.
 *       R/R: reduce the rule that comes first (textually first in the yacc file)
 *
 * Advice from: http://dinosaur.compilertools.net/yacc/
 *
 *   'Conflicts resolved by precedence are not counted in the number of S/R and R/R
 *    conflicts reported by Yacc. This means that mistakes in the moduleOrNamespaceSpfn of
 *    precedences may disguise errors in the input grammar; it is a good idea to be
 *    sparing with precedences, and use them in an essentially ``cookbook'' fashion,
 *    until some experience has been gained'
 *
 * Observation:
 *   It is possible to eliminate conflicts by giving precedence to rules and tokens.
 *   Dummy tokens can be used for the rule and the tokens also need precedence.
 *   The danger is that giving precedence to the tokens may twist the grammar elsewhere.
 *   Maybe it would be good to assign precedence at given locations, e.g.
 *
 *   order: precShort precLong
 *
 *   rule: TokA TokB %@precShort        {action1}     -- assign prec to rule.
 *       | TokA TokB TokC@precLong TokD {action2}     -- assign prec to TokC at this point.
 *
 * Observation: reduce/reduce
 *   If there is a common prefix with a reduce/reduce conflict,
 *   e.g "OPEN path" for topopens and moduleOrNamespaceDefns then can factor
 *   opendef = "OPEN path" which can be on both paths.
 *
 * Debugging and checking precedence rules.
 *   - comment out a rule's %prec and see what conflicts are introduced.
 *
 * Dummy terminals (like prec_type_prefix) can assign precedence to a rule.
 * Doc says rule and (shift) token precedence resolves shift/reduce conflict.
 * It seems like dummy terminals can not assign precedence to the shift,
 * but including the tokens in the precedences below will order them.
 * e.g. prec_type_prefix lower precedence than RARROW, LBRACK, IDENT, LAZY, STAR (all extend types).
 */

/* start with lowest */

%nonassoc prec_args_error             /* less than RPAREN */
%nonassoc prec_atomexpr_lparen_error  /* less than RPAREN */

%right AS

/* prec_wheretyp_prefix = "where typ" lower than extensions, i.e. "WHEN" */
%nonassoc prec_wheretyp_prefix        /* lower than WHEN and RPAREN */
%nonassoc RPAREN

%right WHEN

/* prec_pat_pat_action = "pattern when expr -> expr"
 * Lower than match extensions - i.e. BAR.
 */
%nonassoc prec_pat_pat_action          /* lower than BAR */

/* "a then b" as an object constructor is very low precedence */
/* Lower than "if a then b" */
%left prec_then_before
%nonassoc prec_then_if
%left  BAR

%right SEMICOLON  prec_semiexpr_sep OBLOCKSEP
%right prec_defn_sep

/* prec_atompat_pathop = precedence of at atomic pattern, e.g "Constructor".
 * Lower than possible pattern extensions, so "pathop . extension" does shift not reduce.
 * possible extensions are:
 *  - constant terminals.
 *  - null
 *  - LBRACK = [
 *  - TRUE,FALSE
 */
%nonassoc prec_atompat_pathop
%nonassoc INT8 UINT8 INT16 UINT16 INT32 UINT32 INT64 UINT64 NATIVEINT UNATIVEINT IEEE32 IEEE64 CHAR STRING BYTEARRAY BIGINT BIGNUM DECIMAL
%nonassoc LPAREN LBRACE LBRACK_BAR
%nonassoc TRUE FALSE UNDERSCORE NULL


/* prec_typ_prefix        lower than "T  -> T  -> T" extensions.
 * prec_tuptyp_prefix     lower than "T * T * T * T" extensions.
 * prec_tuptyptail_prefix lower than "T * T * T * T" extensions.
 * Lower than possible extensions:
 *  - STAR, LAZY, IDENT, RARROW
 *  - LBRACK = [ - for "base[]" types
 * Shifts not reduces.
 */
%nonassoc prec_typ_prefix             /* lower than STAR, LAZY, IDENT, RARROW etc */
%nonassoc prec_tuptyp_prefix          /* ditto */
%nonassoc prec_tuptyptail_prefix      /* ditto */
%nonassoc prec_toptuptyptail_prefix      /* ditto */

%right    RARROW
%nonassoc IDENT LAZY LBRACK

/* prec_opt_attributes_none = precedence of no attributes
 * These can prefix LET-moduleOrNamespaceDefns.
 * Committing to an opt_attribute (reduce) forces the decision that a following LET is a definition.
 * At the top-level, it could turn out to be an expr, so prefer to shift and find out...
 */
%nonassoc prec_opt_attributes_none    /* lower than LET,NEW */

/* LET,NEW higher than SEMICOLON so shift
 *   "seqExpr = seqExpr; . let x = y in z"
 *   "seqExpr = seqExpr; . new...."
 */
%nonassoc LET NEW

/* Redundant dummies: expr_let, expr_function, expr_fun, expr_match */
/* Resolves conflict: expr_try, expr_if */
%nonassoc expr_let
%nonassoc decl_let
%nonassoc expr_function expr_fun expr_match expr_try expr_do
%nonassoc decl_match decl_do
%nonassoc expr_if                     /* lower than ELSE to disambiguate "if _ then if _ then _ else _" */
%nonassoc ELSE

/* prec_atomtyp_path = precedence of atomType "path"
 * Lower than possible extension "path<T1,T2>" to allow "path . <" shift.
 * Extensions: LESS
 */
%nonassoc prec_atomtyp_path           /* lower than LESS */
%nonassoc prec_atomtyp_get_path       /* lower than LESS */

/* prec_no_more_attr_bindings = precedence of "more_localBindings = ."
 * Lower precedence than AND so further bindings are shifted.
 */
%nonassoc prec_no_more_attr_bindings  /* lower than AND */
%nonassoc OPEN

/* prec_interfaces_prefix - lower than extensions, i.e. INTERFACE */
%nonassoc prec_interfaces_prefix      /* lower than INTERFACE */
%nonassoc INTERFACE

%right LARROW
%right COLON_EQUALS
%nonassoc pat_tuple expr_tuple
%left COMMA
%nonassoc slice_comma  /* for matrix.[1..2,3..4] the ".." has higher precedence than "2,3" */
%nonassoc DOT_DOT /* for matrix.[1..2,3..4] the ".." has higher precedence than "2,3" */
%nonassoc paren_pat_colon
%nonassoc paren_pat_attribs
%left OR BAR_BAR
%left AND   /* check */
%left  AMP AMP_AMP
%nonassoc pat_conj
%nonassoc expr_not
%left INFIX_COMPARE_OP DOLLAR LESS GREATER EQUALS  INFIX_BAR_OP INFIX_AMP_OP
%right INFIX_AT_HAT_OP
%right COLON_COLON
%nonassoc pat_isinst expr_isinst COLON_GREATER
%left PLUS_MINUS_OP MINUS ADJACENT_PREFIX_PLUS_MINUS_OP
%left  INFIX_STAR_DIV_MOD_OP STAR PERCENT_OP
%right INFIX_STAR_STAR_OP
%left  QMARK_QMARK
%nonassoc expr_prefix_plus_minus
%left expr_app expr_assert expr_lazy
%left expr_args
%right matching_bar
%left pat_app
%left pat_args
%left PREFIX_OP
%left DOT
%left HIGH_PRECEDENCE_APP
%left HIGH_PRECEDENCE_TYAPP


%nonassoc prec_interaction_empty

%%

/* F# TopLevel */
/* NOTE: interactions */
/* A SEMICOLON_SEMICOLON (or EOF) will mark the end of all interaction blocks. */
/* The end of interaction blocks must be determined without needing to lookahead one more token. */
/* A lookahead token would be dropped between parser calls. See bug 1027. */

interaction:
  | opt_itop_seps interactiveExprOrDefinitions
     { IDefns ($2,lhs()) }
  /* Hash directives are processed separately. Note they may be */
  /* separated by a OBLOCKSEP */
  | opt_itop_seps hashDirective interactiveTerminator
     { $2 }
  | opt_itop_seps hashDirective OBLOCKSEP
     { $2 }

hashDirective:
  | HASH IDENT hashDirectiveArgs
     { IHash ($2,$3,lhs()) }
  | HASH_KEYWORD hashDirectiveArgs
     { IHash ($1,$2,lhs()) }

hashDirectiveArg:
  | STRING
     { Bytes.unicode_bytes_as_string $1 }

hashDirectiveArgs:
  |
     { [] }
  | hashDirectiveArgs hashDirectiveArg
     { $1 @ [$2] }

interactiveTerminator:
  | SEMICOLON_SEMICOLON {}
  | EOF     {}

/* Represents the sequence of items swallowed in one gulp by F# Interactive */
/* It is important to make this as large as possible given the chunk of input */
/* text. More or less identical to 'moduleOrNamespaceDefns' but where SEMICOLON_SEMICOLON is */
/* not part of the grammar of topSeps and HASH interactions are not part of */
/* the swalloed blob, since things like #use must be processed separately. */
/* REVIEW: limiting the input chunks until the next # directive can lead to */
/* discrepencies between whole-file type checking in FSI and FSC. */

interactiveDefinitions:
  /* Always ends on interactiveTerminator */
  | moduleOrNamespaceDefn interactiveDefinitions { $1 @ $2 }
  | moduleOrNamespaceDefn itop_seps interactiveExprOrDefinitions { $1 @ $3 }
  | interactiveTerminator { [] }

/* this can come as the start of an interaction or after a interactiveTopSep */
interactiveExprOrDefinitions:
  /* Always ends on interactiveTerminator */
  | declExpr itop_seps interactiveExprOrDefinitions
      { Def_let(false,[mk_Do  (false,$1,rhs 1)], rhs 1) :: $3 }
  | declExpr interactiveTerminator
      { [ Def_let(false,[mk_Do  (false,$1,rhs 1)], rhs 1) ] }
  | interactiveDefinitions { $1 }

/* F# Language Proper */

signatureFile:
  | namespaceSpecs EOF
     { check_eof_error_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY $1 $2 }

implementationFile:
  | namespaceImpls EOF
     { check_eof_error_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY $1 $2 }

moduleIntro:
  | MODULE grab_doc opt_access path { $4,true,$2,$3 }

namespaceIntro:
  | NAMESPACE grab_doc path { $3,false,$2  }

namespaceSpecs:
  |  moduleSpec  { [ ($1 ([],emptyXMLDoc)) ] }
  | namespaceSpecList { $1 }

namespaceSpecList:
  | namespaceSpec namespaceSpecList { $1 :: $2 }
  | namespaceSpec { [$1] }
namespaceSpec:
  | opt_attributes namespaceIntro deprecated_opt_equals moduleSpec
     { let path,_,xml = $2 in ($4 (path,xml)) }


namespaceImpls:
  |  moduleImpl  { [ ($1 ([],emptyXMLDoc)) ] }
  | namespaceImplList { $1 }

namespaceImplList:
  | namespaceImpl namespaceImplList { $1 :: $2 }
  | namespaceImpl { [$1] }

namespaceImpl:
  | opt_attributes namespaceIntro deprecated_opt_equals moduleImpl
     { let path,_,xml = $2 in ($4 (path,xml)) }

moduleSpec:
  | opt_attributes opt_decl_visibility  moduleIntro specifications
    { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
      let m = (rhs2 3 4) in
      (fun (path,_) ->
        let path2,_,xml,vis = $3 in
        let lid = path@path2 in
        NamedTopModuleSpec(ModuleSpec(lid,true, $4, xml,$1,vis,m)))  }
  | specifications
    { let m = (rhs 1) in
      (fun (path,xml) ->
        match path with
        | [] -> AnonTopModuleSpec($1, m)
        | _ -> AnonNamespaceFragmentSpec(path,false, $1, xml,[],m))  }

moduleImpl:
  | opt_attributes opt_decl_visibility moduleIntro exprOrDefinitions
    { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
      let m = (rhs2 3 4) in
      (fun (path,_) ->
        let path2,isModule,xml,vis = $3 in
        let lid = path@path2 in
        NamedTopModuleImpl(ModuleImpl(lid,isModule, $4, xml,$1,vis,m))) }
  | exprOrDefinitions
    { let m = (rhs 1) in
      (fun (path,xml) ->
        match path with
        | [] -> AnonTopModuleImpl($1,m)
        | _ -> AnonNamespaceFragmentImpl(path,false, $1, xml,[],m)) }

specifications:
  | moduleOrNamespaceSpfn  opt_top_seps specifications
      { $1 :: $3 }
  | hashDirective top_sep specifications
      { Spec_hash ($1,rhs2 1 1) :: $3 }
  | error opt_top_seps specifications
      { (* silent recovery *) $3 }
  |
      { [] }


/* this can come as the start of a file or after a SEMICOLON_SEMICOLON */
exprOrDefinitions:
  | declExpr top_seps exprOrDefinitions
      { Def_let(false,[mk_Do  (false,$1,rhs 1)], rhs 1) :: $3 }
  | declExpr
      { [ Def_let(false,[mk_Do  (false,$1,rhs 1)], rhs 1) ] }
  | moduleOrNamespaceDefns { $1 }

moduleOrNamespaceDefns:
  | moduleOrNamespaceDefn moduleOrNamespaceDefns
      {  $1 @ $2 }
  | moduleOrNamespaceDefn top_seps exprOrDefinitions
      {  $1 @ $3 }
  | hashDirective top_seps exprOrDefinitions
      { Def_hash ($1,rhs2 1 1) :: $3 }
  | error moduleOrNamespaceDefns
      { (* silent recovery *) $2 }
  | error top_seps exprOrDefinitions
      { (* silent recovery *) $3 }
  |
      { [] }

moduleOrNamespaceDefn:
  | defnBindings                                  %prec decl_let
      { new_arg_uniq_ref := 0;
        mkDefnBindings (lhs()) $1 [] None (lhs())  }

  | hardwhiteDefnBindings                         %prec decl_let
      { new_arg_uniq_ref := 0;
        mkDefnBindings (lhs()) $1 [] None (lhs()) }

  | opt_attributes opt_decl_visibility defnBindings                   %prec decl_let
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        new_arg_uniq_ref := 0;
        mkDefnBindings (rhs 3) $3 $1 $2 (rhs 3)  }

  | opt_attributes opt_decl_visibility hardwhiteDefnBindings          %prec decl_let
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        new_arg_uniq_ref := 0;
        mkDefnBindings (rhs 3) $3 $1 $2 (rhs 3)  }

  | opt_attributes opt_decl_visibility doBinding %prec decl_let
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        let letm = rhs 3 in
        mkDefnBindings letm $3 $1 $2 (rhs 3) }
/*
But where do signatures for recursive elements go?
Also need signatures for members

  | valSpfn
      { [ Def_partial_inline_compsig($1,lhs()) ] }
*/

  | opt_attributes opt_decl_visibility TYPE tyconDefn tyconDefnList
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        let      (TyconDefn(ComponentInfo(cas   ,kind     ,a,cs,b,c,d,d2,d3),e,f,g)) = $4 in
        let tc = (TyconDefn(ComponentInfo($1@cas,TMK_Tycon,a,cs,b,c,d,d2,d3),e,f,g)) in
        [ Def_tycon(tc :: $5,rhs2 3 5) ] }

/*
  | opt_attributes opt_decl_visibility MODULE TYPE componentInfo EQUALS moduleSpecBlock
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        let (ComponentInfo(cas,_,a,cs,b,c,d,e,f)) = $4 in
        if nonNil cs then  raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 4) "constraint modules may not yet include constraints on the parameters";
        let info = ComponentInfo($1@cas,TMK_Constraint,a,cs,b,c,d,e,f) in
        [ Def_named_compsig(info,$6,rhs2 3 6) ] }
*/

  | opt_attributes opt_decl_visibility TYPE componentInfo tyconDefnAugmentation
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        if $1 <> [] then warning(Error("Attributes on augmentations are ignored, they must be placed on the original declaration",rhs 1));
        [ Def_partial_tycon($4,$5,rhs2 3 5) ] }

  | opt_attributes opt_decl_visibility exconDefn
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        let (ExconDefn(ExconCore(cas,a,b,c,d,d2),e,f)) = $3 in
        let ec = (ExconDefn(ExconCore($1@cas,a,b,c,d,d2),e,f)) in
        [ Def_exn(ec, rhs2 3 3) ] }

  | opt_attributes opt_decl_visibility moduleIntro opt_signature EQUALS namedModuleAbbrevBlock
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        let (path,isModule,xml,vis),mty,eqn = $3,$4,$6 in
        if not isModule          then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) "namespaces must be declared at the head of a file";
        if isSome mty            then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) "a module abbreviation may not be given a constraint";
        if List.length path <> 1 then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) "a module abbreviation must be a simple name, not a path";
        if List.length $1 <> 0   then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 1) "ignorning attributes on module abbreviation";
        if isSome vis            then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 1) "ignorning accessibility attribute on module abbreviation. Module abbreviations are always private";
        [ Def_module_abbrev(List.hd path,eqn,rhs2 3 6) ] }

  | opt_attributes opt_decl_visibility moduleIntro opt_signature EQUALS  namedModuleDefnBlock
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        let attribs,(path,isModule,xml,vis),mty,def = $1,$3,$4,$6 in
        if not isModule          then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) "namespaces must be declared at the head of a file";
        if List.length path <> 1 then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) "a module definition must be a simple name, not a path";
        let info = ComponentInfo(attribs,TMK_Module,[],[],path,xml,false,vis,rhs 3) in
        [ Def_module(info,def,mty,rhs2 3 6) ] }

  | openDecl
      { [Def_open($1,rhs 1)] }

namedModuleAbbrevBlock:
  | OBLOCKBEGIN path OBLOCKEND
       { $2 }
  | path
       { $1 }

namedModuleDefnBlock:
  | OBLOCKBEGIN wrappedNamedModuleDefn OBLOCKEND
       { $2 }
  | OBLOCKBEGIN moduleOrNamespaceDefns OBLOCKEND
       { $2 }
  | OBLOCKBEGIN moduleOrNamespaceDefns recover
       { report_parse_error_at (rhs 1) "unclosed block in #light syntax";
         $2 }
  | OBLOCKBEGIN error OBLOCKEND
       { [] }
  | wrappedNamedModuleDefn
       { $1 }

wrappedNamedModuleDefn:
  | structOrBegin exprOrDefinitions END
       { $2 }
  | structOrBegin exprOrDefinitions recover
       { report_parse_error_at (rhs 1) "unmatched 'begin' or 'struct'";
         $2 }
  | structOrBegin error END
       { [] }

opt_signature :
  |
       { None }
  | COLON moduleSpecBlock
       { deprecated "Signature types must be given in a .fsi or .mli file" (lhs());
         Some(Sign_explicit($2)) }
  | COLON path
       { deprecated "Signature types must be given in a .fsi or .mli file" (lhs());
         Some(Sign_named($2)) }

tyconDefnAugmentation:
  | WITH classDefnBlock decl_end
     { $2 }
/* opt_sig: { None } | COLON sigOrBegin specifications END { $3 } */

moduleOrNamespaceSpfn:
  | valSpfn
      { $1 }

  | opt_attributes opt_decl_visibility moduleIntro colonOrEquals namedModuleAbbrevBlock
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        let path,isModule,xml,vis = $3 in
        if not isModule          then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) "namespaces must be declared at the head of a file";
        if List.length path <> 1 then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) "a module abbreviation must be a simple name, not a path";
        if List.length $1 <> 0   then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 1) "ignorning attributes on module abbreviation";
        if isSome(vis)           then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 1) "ignorning visibility attribute on module abbreviation. Module abbreviations are always private";
        Spec_module_abbrev(List.hd path,$5,rhs2 3 5) }

  | opt_attributes opt_decl_visibility  moduleIntro colonOrEquals moduleSpecBlock
      { let path,isModule,xml,vis = $3 in
        if not isModule          then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) "namespaces must be declared at the head of a file";
        if List.length path <> 1 then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) "a module definition must be a simple name, not a path";
        let info = ComponentInfo($1,TMK_Module,[],[],path,xml,false,vis,rhs 3) in
        if isSome($2) then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        Spec_module(info,$5,rhs2 3 5) }

  | opt_attributes opt_decl_visibility  tyconSpfns
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        let (TyconSpfn(ComponentInfo(cas,k,a,cs,b,c,d,d2,d3),e,f,g)),rest =
           match $3 with
           | [] -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) "unexpected empty type definition list"
           | h::t -> h,t in
        let tc = (TyconSpfn(ComponentInfo($1@cas,k,a,cs,b,c,d,d2,d3),e,f,g))in
        Spec_tycon (tc::rest,rhs 3) }

  | opt_attributes opt_decl_visibility exconSpfn
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        let (ExconSpfn(ExconCore(cas,a,b,c,d,d2),e,f)) = $3 in
        let ec = (ExconSpfn(ExconCore($1@cas,a,b,c,d,d2),e,f)) in
        Spec_exn(ec, rhs 3) }

  | OPEN path { Spec_open ($2, rhs2 1 2) }

valSpfn:
  | opt_attributes opt_decl_visibility VAL opt_attributes opt_inline opt_mutable opt_access nameop grab_doc opt_explicitValTyparDecls COLON topTypeWithTypeConstraints opt_literalValue
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        (let attr1,attr2,inlineFlag,mutableFlag,vis2,id,doc,explicitValTyparDecls,(ty,arity),konst = ($1),($4),($5),($6),($7),($8),($9),($10),($12),($13) in
        if attr2 <> [] then warning(Error("This syntactic location for attributes is deprecated. Attributes should be placed before 'val'",rhs 4));
        let m = rhs2 3 12 in
        let valSpfn = ValSpfn((attr1@attr2),id,explicitValTyparDecls,ty,arity,inlineFlag,mutableFlag,doc, vis2,konst,m) in
        Spec_val(valSpfn,m))
      }

opt_literalValue:
  | { None }
  | EQUALS declExpr { Some($2) }


moduleSpecBlock:
  | OBLOCKBEGIN            specifications     OBLOCKEND { $2 }
  | OBLOCKBEGIN sigOrBegin specifications END OBLOCKEND { $3 }
  |             sigOrBegin specifications END { $2 }

opt_attributes:
  | attributes                                { $1 }
  |            %prec prec_opt_attributes_none { [] }

attributes:
  | attributeList
     { $1 }
  | attributeList attributes
     { $1 @ $2 }

attributeList:
  | LBRACK_LESS  attributeListElements opt_seps GREATER_RBRACK opt_OBLOCKSEP {  matchPair 1 4; $2 }

attributeListElements:
  | attribute
     { [$1] }
  | attributeListElements seps attribute
     { $1 @ [$3] }

attribute:
  | path opt_HIGH_PRECEDENCE_APP opt_argExprAfterType
     { let arg = match $3 with None -> mksyn_unit (lhs()) | Some e -> e in
       Attr($1,arg,None,lhs()) }
  | attributeTarget COLON path opt_HIGH_PRECEDENCE_APP opt_argExprAfterType
     { let arg = match $5 with None -> mksyn_unit (lhs()) | Some e -> e in
       Attr($3,arg,Some $1,lhs()) }

attributeTarget:
  | MODULE { ident("module",lhs()) }
  | TYPE { ident("type",lhs()) }
  | ident { $1 }
  | YIELD /* return */ { if $1 then report_parse_error_at (rhs 1) "syntax error";
                         ident("return",lhs()) }

/* note: grabXML() gets XML doc lines to point (and maybe one lookahead token) */
grab_doc:
  |
     { grabXML() }

tyconSpfns:
  | TYPE tyconSpfn_list
     { $2 }

tyconSpfn_list:
  | tyconSpfn AND tyconSpfn_list
     { $1 :: $3 }
  | tyconSpfn
     { [$1] }

tyconSpfn:
  | componentInfo  EQUALS tyconSpfnRhsBlock
      { $3 $1 }
  | componentInfo  opt_classSpfn
      { TyconSpfn($1,TyconSpfnRepr_simple (TyconCore_repr_hidden (lhs()),lhs()),$2,lhs()) }

tyconSpfnRhsBlock:
  /* This rule allows members to be given for record and union types in the #light syntax */
  /* without the use of 'with' ... 'end'. For example: */
  /*     type R = */
  /*         { a : int } */
  /*         member r.A = a */
  /* It also takes into account that any existing 'with' */
  /* block still needs to be considered and may occur indented or undented from the core type */
  /* representation. */
  | OBLOCKBEGIN  tyconSpfnRhs opt_OBLOCKSEP classSpfnMembers opt_classSpfn OBLOCKEND opt_classSpfn
     { let m = lhs() in
       (fun nameInfo ->
           $2 nameInfo (checkForMultipleAugmentations m ($4 @ $5) $7)) }
  | tyconSpfnRhs opt_classSpfn
     { let m = lhs() in
       (fun nameInfo ->
           $1 nameInfo $2) }

tyconSpfnRhs:
  | tyconDefnOrSpfnSimpleRepr
     { let m = lhs() in
       (fun nameInfo augmentation ->
           TyconSpfn(nameInfo,TyconSpfnRepr_simple ($1,m),augmentation,m)) }
  | tyconClassSpfn
     { let m = lhs() in
       (fun nameInfo augmentation ->
           TyconSpfn(nameInfo,TyconSpfnRepr_class (fst $1,snd $1,m),augmentation,m)) }
  | DELEGATE OF topType
     { let m = lhs() in
       let ty,arity = $3 in
       let invoke = ClassMemberSpfn_binding(ValSpfn([],mksyn_id m "Invoke",inferredTyparDecls,ty,arity,false,false,emptyXMLDoc,None,None,m),abstractFlags None MemberKindMember,m) in
       (fun nameInfo augmentation ->
           if nonNil augmentation then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m "augmentations are not permitted on delegate type definitions";
           TyconSpfn(nameInfo,TyconSpfnRepr_class (TyconDelegate (ty,arity),[invoke],m),[],m)) }

tyconClassSpfn:
  | classSpfnBlockKindUnspecified
     { (TyconUnspecified, $1) }
  | classOrInterfaceOrStruct classSpfnBlock END
     { ($1,$2) }
  | classOrInterfaceOrStruct classSpfnBlock recover
     { report_parse_error_at (rhs 1) "unmatched 'class', 'interface' or 'struct'";
       ($1,$2) }
  | classOrInterfaceOrStruct error END
     { (* silent recovery *) ($1,[]) }

classSpfnBlockKindUnspecified:
  | OBLOCKBEGIN  classSpfnMembers OBLOCKEND
     { $2 }
/* NOTE: these rules enable a 'heavy' syntax to omit the kind of a type. However this doesn't seem necessary to support.
  NOTE: that is 'type kind inference' is only supported for #light
  | BEGIN  classSpfnBlock END
     { $2 }
  | BEGIN  classSpfnBlock recover
     { report_parse_error_at (rhs 1) "unmatched 'begin'";
       $2 }
  | BEGIN  error END
     { (* silent recovery *) [] }
*/


classSpfnBlock:
  | OBLOCKBEGIN  classSpfnMembers OBLOCKEND { $2 }
  | classSpfnMembers { $1 }

classSpfnMembers:
  | classMemberSpfn opt_seps classSpfnMembers
     { $1 :: $3 }
  |
     { []  }

memberFlags:
  /* | STATIC          { (fun q k -> staticFlags q k) } */
  | STATIC MEMBER   { (fun q k -> staticFlags q k) }
  | MEMBER          { (fun q k -> nonVirtualFlags q k) }
  | METHOD         { raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 1) "use 'member x.MyMethod(arg) = ...' to declare a new method" }
  | VIRTUAL         { raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 1) "use 'abstract' to declare a new virtual method slot, and 'default' or 'override' to specify the default implemenation for that slot" }
  | OVERRIDE        { (fun q k -> overrideFlags q k) }
  | DEFAULT        { (fun q k -> overrideFlags q k) }

memberSpecFlags:
  | memberFlags { $1 }
  | ABSTRACT        { (fun q k -> abstractFlags q k) }
  | ABSTRACT MEMBER { (fun q k -> abstractFlags q k) }

classMemberSpfnGetSet:
  | /* EMPTY */
    { (fun arity -> (match arity with TopValSynData([],_) -> MemberKindPropertyGet | _ -> MemberKindMember)) }
  | WITH classMemberSpfnGetSetElements
    { (fun arity -> $2) }
  | OWITH classMemberSpfnGetSetElements OEND
    { (fun arity -> $2) }
  | OWITH classMemberSpfnGetSetElements error
    {  report_parse_error_at (rhs 1) "unmatched 'with' or badly formatted 'with' block";
       (fun arity -> $2) }


classMemberSpfnGetSetElements:
  | identop
    { (let (id:ident) = $1 in
       if id.idText = "get" then MemberKindPropertyGet
       else if id.idText = "set" then MemberKindPropertySet
       else raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 1) "'get', 'set' or 'get,set' required") }
  | identop COMMA identop
    { let (id:ident) = $1 in
      if not ((id.idText = "get" && $3.idText = "set") or
              (id.idText = "set" && $3.idText = "get")) then
         raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs2 1 3) "'get', 'set' or 'get,set' required";
      MemberKindPropertyGetSet }

classMemberSpfn:
  | opt_attributes opt_decl_visibility memberSpecFlags opt_inline grab_doc opt_access nameop opt_explicitValTyparDecls COLON topTypeWithTypeConstraints classMemberSpfnGetSet opt_literalValue
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       let inlineFlag,doc,vis2,id,explicitValTyparDecls,(ty,arity),optLiteralValue = $4,$5,$6,$7,$8,$10,$12 in
       let m = rhs2 3 11 in
       let valSpfn = ValSpfn($1,id,explicitValTyparDecls,ty,arity, inlineFlag,false,doc, vis2,optLiteralValue,m) in
       ClassMemberSpfn_binding(valSpfn, $3 (computeOverloadQualifier $1) ($11 arity),m) }
  | opt_attributes opt_decl_visibility interfaceMember appType
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       ClassMemberSpfn_interface ($4,rhs2 3 4) }
  | opt_attributes opt_decl_visibility INHERIT appType
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       ClassMemberSpfn_inherit ($4,rhs2 3 4) }
  | opt_attributes opt_decl_visibility VAL fieldDecl
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       ClassMemberSpfn_field($4 $1 false,rhs2 3 4) }
  | opt_attributes opt_decl_visibility STATIC VAL fieldDecl
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       ClassMemberSpfn_field($5 $1 true,rhs2 3 5) }
  | opt_attributes  opt_decl_visibility STATIC TYPE tyconSpfn
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       ClassMemberSpfn_tycon($5,rhs2 3 5) }
  | opt_attributes opt_decl_visibility NEW grab_doc COLON topTypeWithTypeConstraints
     { let vis = $2 in
       let doc = $4 in
       let ty,arity = $6 in
       let m = rhs2 3 6 in
       let inlineFlag = false in
       let valSpfn = ValSpfn($1,mksyn_id (rhs 3) "new",noInferredTypars,ty,arity,inlineFlag,false, doc, vis,None,m) in
       ClassMemberSpfn_binding(valSpfn, ctorMemFlags  (computeOverloadQualifier $1),m) }

componentInfo:
  | opt_attributes grab_doc tyconNameAndTyparDecls opt_typeConstraints
     { startName(rhs 2);
       let a,b,c,tpcs1,vis = $3 in
       let tpcs2 = $4 in
       ComponentInfo($1,TMK_Tycon,a,(tpcs1 @ tpcs2),b,$2,c,vis,rhs2 3 3)  }

tyconDefnList:
  | AND tyconDefn tyconDefnList
     { $2 :: $3 }
  |
     { [] }

tyconDefn:
  | componentInfo EQUALS tyconDefnRhsBlock
     { let tcDefRepr,members = $3 in
       TyconDefn($1,tcDefRepr,members,lhs()) (* <-- mark changed *) }
  | componentInfo opt_HIGH_PRECEDENCE_APP  simplePatterns opt_as EQUALS tyconDefnRhsBlock
     { let spats, az,(tcDefRepr,members) = $3,$4,$6 in
       let memberCtorPattern = ClassMemberDefn_implicit_ctor (spats,az,rhs 1) in
       let tcDefRepr =
         match tcDefRepr with
         | TyconDefnRepr_class (k,cspec,m) -> TyconDefnRepr_class (k,memberCtorPattern::cspec,m)
         | _ -> report_parse_error_at (rhs 1) "Only class types may take value arguments"; tcDefRepr
       in
       TyconDefn($1,tcDefRepr,members,lhs()) (* <-- mark changed *) }

tyconDefnRhsBlock:
  /* This rule allows members to be given for record and union types in the #light syntax */
  /* without the use of 'with' ... 'end'. For example: */
  /*     type R = */
  /*         { a : int } */
  /*         member r.A = a */
  /* It also takes into account that any existing 'with' */
  /* block still needs to be considered and may occur indented or undented from the core type */
  /* representation. */
  | OBLOCKBEGIN  tyconDefnRhs opt_OBLOCKSEP classDefnMembers opt_classDefn OBLOCKEND opt_classDefn
     { let m = lhs() in
       $2 (checkForMultipleAugmentations m ($4 @ $5) $7) }
  | tyconDefnRhs opt_classDefn
     { let m = lhs() in
       $1 $2 }

tyconDefnRhs:
  | tyconDefnOrSpfnSimpleRepr
     { let m = lhs() in (fun augmentation -> TyconDefnRepr_simple ($1,m),augmentation) }
  | tyconClassDefn
     { let m = lhs() in (fun augmentation -> TyconDefnRepr_class (fst $1,snd $1,m),augmentation) }
  | DELEGATE OF topType
     { let m = lhs() in
       let ty,arity = $3 in
       (fun augmentation ->
           let valSpfn = ValSpfn([],mksyn_id m "Invoke",inferredTyparDecls,ty,arity,false,false,emptyXMLDoc,None,None,m) in
           let invoke = ClassMemberDefn_slotsig(valSpfn,abstractFlags None MemberKindMember,m) in
           if nonNil augmentation then raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY m "augmentations are not permitted on delegate type definitions";
           TyconDefnRepr_class (TyconDelegate (ty,arity),[invoke],m),[]) }

tyconClassDefn:
  | classDefnBlockKindUnspecified
     { (TyconUnspecified, $1) }
  | classOrInterfaceOrStruct classDefnBlock END
     { ($1,$2) }
  | classOrInterfaceOrStruct classDefnBlock recover
     { report_parse_error_at (rhs 1) "unmatched 'class', 'interface' or 'struct'";
       ($1,$2) }
  | classOrInterfaceOrStruct error END
     { (* silent recovery *) ($1,[]) }

classDefnBlockKindUnspecified:
  | OBLOCKBEGIN  classDefnMembers OBLOCKEND
     { $2 }
/* NOTE: these rules enable a 'heavy' syntax to omit the kind of a type. However this doesn't seem necessary to support.
  NOTE: that is 'type kind inference' is only supported for #light
  | BEGIN  classDefnBlock END
     { $2 }
  | BEGIN  classDefnBlock recover
     { report_parse_error_at (rhs 1) "unmatched 'begin'";
       $2 }
  | BEGIN  error END
     { (* silent recovery *) [] }
*/

classDefnBlock:
  | OBLOCKBEGIN  classDefnMembers OBLOCKEND { $2 }
  | classDefnMembers { $1 }

classDefnMembers:
  | classDefnMember opt_seps classDefnMembers
     { $1 @  $3 }
  | error classDefnMembers
     { $2 }
  |
     { [] }

classDefnMemberGetSet:
  | WITH classDefnMemberGetSetElements
     { $2  }
  | OWITH classDefnMemberGetSetElements OEND
     { $2  }
  | OWITH classDefnMemberGetSetElements error
     { report_parse_error_at (rhs 1) "unmatched 'with' or badly formatted 'with' block";
       $2  }

classDefnMemberGetSetElements:
  | classDefnMemberGetSetElement
     { [$1]  }
  | classDefnMemberGetSetElement AND classDefnMemberGetSetElement
     { [$1;$3] }

classDefnMemberGetSetElement:
  | opt_inline bindingPattern opt_topReturnTypeWithTypeConstraints EQUALS typedSeqExprBlock
     { ($1,$2,$3,$5,rhs 5) }

memberCore:
 /* methods and simple getter properties */
  | opt_inline bindingPattern  opt_topReturnTypeWithTypeConstraints EQUALS typedSeqExprBlock
     {  let wholem = rhs2 2 5 in
        let bindm = rhs 2 in
        let rhsm = rhs 5 in
        let mpat = rhs 2 in
        let optReturnType = $3 in
        let bindingBuilder = $2 in
        (fun vis memflags attrs ->
             [ ClassMemberDefn_member_binding (bindingBuilder vis $1 false bindm wholem optReturnType $5 rhsm [] attrs (Some (memflags (computeOverloadQualifier attrs) MemberKindMember)),bindm) ]) }

 /* properties with explicit get/set, also indexer properties */
  | opt_inline bindingPattern  opt_topReturnTypeWithTypeConstraints classDefnMemberGetSet
     { let wholem = rhs2 2 4 in
       let bindm = rhs 2 in
       let propertyNameBindingBuilder = $2 in
       let optPropertyType = $3 in
       let mutableFlag = false in
       (fun vis memflags attrs ->
             $4 |> List.map (fun (optInline,bindingBuilder,optReturnType,expr,exprm) ->
                   let optInline = $1 || optInline in
                   let overloadQualifier =  (computeOverloadQualifier attrs) in

                   let binding = bindingBuilder vis optInline mutableFlag bindm wholem optReturnType expr exprm [] attrs (Some (memflags overloadQualifier MemberKindMember)) in
                   let (Binding (vis,_,pseudo,_,attrs,doc,memberInfo,pv,_,bindm)) = binding in
                   let memkind =
                         let getset =
                               let rec go p =
                                   match p with
                                   | Pat_lid ([id],_,_,_,_) ->  id.idText
                                   | Pat_as (_,nm,_,_,_) ->  nm.idText
                                   | Pat_typed (p,_,_) ->  go p
                                   | Pat_attrib (p,_,_) ->  go p
                                  | _ -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY bindm "invalid declaration syntax"  in
                               go pv in
                         if getset = "get" then MemberKindPropertyGet
                         else if getset = "set" then MemberKindPropertySet
                         else raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY bindm "get and/or set required" in


                   (* REVIEW: It's hard not to ignore the optPropertyType type annotation for 'set' properties. To apply it, *)
                   (* we should apply it to the last argument, but at this point we've already pushed the patterns that *)
                   (* make up the arguments onto the RHS. So we just always give a warning. *)

                   begin match optPropertyType with
                   | Some _ -> warning(Error("type annotations on property getters and setters should be given after the 'get()' or 'set(v)', e.g. 'with get() : string = ...'",bindm))
                   | None -> ()
                   end;

                   let optReturnType =
                       match (memkind, optReturnType) with
                       | MemberKindPropertySet,_ -> optReturnType
                       | _, None -> optPropertyType
                       | _ -> optReturnType in

                   (* REDO with the correct member kind *)
                   let binding = bindingBuilder vis pseudo mutableFlag bindm wholem optReturnType expr exprm [] attrs (Some (memflags overloadQualifier memkind)) in
                   let (Binding (vis,_,pseudo,_,attrs,doc,memberInfo,pv,rhs_after_pats,bindm)) = binding in

                   let arity =
                       match memberInfo with
                       | None ->
                           raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY bindm "unexpected empty memberInfo"
                       | Some (_,x,_) -> x in
                   let meminfo = Some (memflags overloadQualifier memkind, arity,None) in

                   (* Create the binding from the first lambda pattern in order to extract out the pattern of the *)
                   (* 'this' variable and hack it into the pattern for the get/set binding, replacing the get/set part *)
                   (* A little gross. *)
                   let pv',doc' =
                       let binding2 = propertyNameBindingBuilder vis optInline mutableFlag bindm bindm optReturnType expr exprm [] attrs (Some (memflags overloadQualifier MemberKindMember)) in
                       let (Binding (_,_,_,_,_,doc2,_,pv2,_,_)) = binding2 in

                       let lid2 =
                           match pv2 with
                           | Pat_lid (lid,None,[],_,m) ->  lid
                           | p -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY bindm "invalid declaration syntax"  in

                       let rec go p =
                           match p with
                           | Pat_lid ([id],tyargs,args,vis,m) ->  Pat_lid (lid2,tyargs,args,vis,m)
                           | Pat_as (p2,nm,_,vis,m) ->  Pat_lid (lid2,None,[],vis,m)
                           | Pat_typed (p,ty,m) ->  Pat_typed(go p,ty,m)
                           | Pat_attrib (p,attribs,m) ->  Pat_attrib(go p,attribs,m)
                           | Pat_wild(m) ->  Pat_wild(m)
                           | _ -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY bindm "invalid declaration syntax"  in
                       go pv,xmlDocMerge doc2 doc in

               ClassMemberDefn_member_binding (Binding (vis,NormalBinding,pseudo,mutableFlag,attrs,doc',meminfo,pv',rhs_after_pats,bindm),bindm)))
       }

abstractMemberFlags:
  | ABSTRACT {}
  | ABSTRACT MEMBER {}

classDefnMember:
  | opt_attributes opt_decl_visibility defnBindings
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       [mkClassMemberLocalBindings false (rhs2 3 3) $1 $2 $3] }

  | opt_attributes opt_decl_visibility hardwhiteDefnBindings
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       [mkClassMemberLocalBindings false (rhs2 3 3) $1 $2 $3] }

  | opt_attributes opt_decl_visibility doBinding
      { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        let wholem = rhs 3 in
        [ mkClassMemberLocalBindings false (rhs 3 ) $1 $2 $3 ] }

  | opt_attributes opt_decl_visibility STATIC defnBindings
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       [mkClassMemberLocalBindings true (rhs2 3 3) $1 $2 $4] }

  | opt_attributes opt_decl_visibility STATIC hardwhiteDefnBindings
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       [mkClassMemberLocalBindings true (rhs2 3 3) $1 $2 $4] }

  | opt_attributes opt_decl_visibility STATIC doBinding
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       [mkClassMemberLocalBindings true (rhs2 3 3) $1 $2 $4] }

/*
  | openDecl
      { [ClassMemberDefn_open($1,rhs 1)] }
*/

  | opt_attributes opt_decl_visibility memberFlags memberCore  opt_ODECLEND
     { if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
       $4 $2 $3 $1 }

  | opt_attributes opt_decl_visibility interfaceMember appType opt_interfaceImplDefn
     {  if $1 <> [] then warning(Error("attributes ignored on interface implementation",rhs 1));
        if isSome $2 then errorR(Error("interfaces always have the same visibility as the enclosing type",rhs 3));
        [ ClassMemberDefn_interface ($4, $5,rhs2 3 5) ] }

  | opt_attributes opt_decl_visibility abstractMemberFlags opt_inline grab_doc nameop opt_explicitValTyparDecls COLON topTypeWithTypeConstraints classMemberSpfnGetSet  opt_ODECLEND
     { let ty,arity = $9 in
       let inlineFlag,doc,id,explicitValTyparDecls = $4,$5,$6,$7 in
       let m = rhs2 3 10 in
       if isSome $2 then errorR(Error("abstract slots always have the same visibility as the enclosing type",m));
       let valSpfn = ValSpfn($1,id,explicitValTyparDecls,ty,arity, inlineFlag,false,doc, None,None,m) in
       [ ClassMemberDefn_slotsig(valSpfn,abstractFlags (computeOverloadQualifier $1) ($10 arity), m) ] }

  | opt_attributes opt_decl_visibility inheritsDefn
     {  if $1 <> [] then warning(Error("attributes ignored on 'inherits' declaration",rhs 1));
        if isSome $2 then errorR(Error("visibility declarations are not permitted on an 'inherits' declaration",rhs 1));
        [ $3 ] }

  | opt_attributes opt_decl_visibility VAL fieldDecl
     {  if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        [ ClassMemberDefn_field($4 $1 false,rhs2 3 4) ] }

  | opt_attributes opt_decl_visibility STATIC VAL fieldDecl
     {  if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        [ ClassMemberDefn_field($5 $1 true,rhs2 3 5) ] }

  | opt_attributes opt_decl_visibility NEW  atomicPattern opt_as grab_doc EQUALS typedSeqExprBlock opt_ODECLEND
     {  let m = rhs2 3 8 in
        let expr = $8 in
        let info = Some (ctorMemFlags (computeOverloadQualifier $1), TopValSynData([SynArgInfo.argdata_of_pat $4],SynArgInfo.unnamedRetVal), $5) in
        let vis = $2 in
        [ ClassMemberDefn_member_binding(Binding (None,NormalBinding,false,false,$1,$6,info, Pat_lid ([mksyn_id (rhs 3) "new"],Some noInferredTypars,[$4],vis,rhs 3),BindingExpr([],None,expr),m),m) ] }

  | opt_attributes opt_decl_visibility STATIC TYPE tyconDefn
     {  if isSome $2 then errorR(Error("visibility declarations should come immediately prior to the identifier naming a construct",rhs 2));
        [ ClassMemberDefn_tycon($5,None,rhs2 3 5) ] }

atomicPatternLongIdent:
  | pathop { (None,$1) }
  | access pathop { (Some($1), $2) }

opt_access:
  |        { None }
  | access { Some($1) }

access:
  | PRIVATE { accessPrivate }
  | PUBLIC { (accessPublic) }
  | INTERNAL { accessInternal }

/* only valid on 'NEW' */
opt_decl_visibility:
  | access { Some($1) }
  |  { None }

opt_interfaceImplDefn:
  | WITH objectImplementationBlock decl_end { Some($2) }
  |                            { None }

opt_classDefn:
  | WITH classDefnBlock decl_end { $2 }
  |                           { [] }

opt_classSpfn:
  | WITH classSpfnBlock decl_end { $2 }
  |                    { [] }


inheritsDefn:
  | INHERIT appType opt_as
     {  ClassMemberDefn_inherit($2,$3,lhs()) }
  | INHERIT appType opt_HIGH_PRECEDENCE_APP argExprAfterType opt_as
     {  ClassMemberDefn_implicit_inherit($2,$4,$5,lhs()) }

opt_as:
  | asSpec { Some($1) }
  |        { None }

asSpec:
  | AS ident { startName(rhs 2); $2 }


objectImplementationBlock:
  | OBLOCKBEGIN objectImplementationMembers OBLOCKEND { $2 }
  | objectImplementationMembers { $1 }

objectImplementationMembers:
  | objectImplementationMember opt_seps objectImplementationMembers { $1 @  $3 }
  | objectImplementationMember opt_seps { $1 }

objectImplementationMember:
  | opt_attributes memberOrOverride memberCore opt_ODECLEND
     { $3 None overrideFlags $1 }
  | opt_attributes memberOrOverride error { [] }

memberOrOverride:
  | MEMBER {}
  | OVERRIDE {}


tyconDefnOrSpfnSimpleRepr:
  | typ
     { TyconCore_abbrev ($1, lhs()) }
  | unionRepr
     { if List.exists (function Choice1 _ -> true | _ -> false) $1 then
           TyconCore_enum (chooseList (function Choice1 data -> Some(data) | Choice2(UnionConstr(_,_,_,_,_,m)) -> errorR(Error("All enum fields must be given values",m)); None) $1,lhs())
       else
           TyconCore_funion (chooseList (function Choice2 data -> Some(data) | Choice1 _ -> failwith "huh?") $1,lhs()) }
  | braceFieldDeclList
     { TyconCore_recd ($1,lhs()) }
  | LPAREN inlineAssemblyTyconRepr RPAREN
     {  libraryOnly (lhs());
        matchPair 1 3;
        $2 }


braceFieldDeclList:
  | LBRACE  recdFieldDeclList RBRACE
     { matchPair 1 3;   $2 }
  | LBRACE  recdFieldDeclList recover
     { $2 }
  | LBRACE  error RBRACE
     { matchPair 1 3;   [] }

inlineAssemblyTyconRepr:
  | HASH STRING opt_HASH
     { libraryOnly (lhs());
       let lhsm = lhs() in
       TyconCore_asm (parse_il_typ $2 (rhs 2),lhsm) }

classOrInterfaceOrStruct:
  | CLASS     { TyconClass }
  | INTERFACE { TyconInterface }
  | STRUCT    { TyconStruct }

interfaceMember:
  | INTERFACE { }
  | OINTERFACE_MEMBER    { }

tyconNameAndTyparDecls:
  | opt_access path
      { startName(rhs 2); [], $2,false,[],$1 }
  | opt_access prefixTyparDecls  path
      {startName(rhs 3);  $2, $3,false,[],$1 }
  | opt_access path postfixTyparDecls
      { startName(rhs 2);
        let tps,tpcs = $3 in
        tps, $2,true,tpcs,$1 }

prefixTyparDecls:
  | typar { [ TyparDecl([],$1) ] }
  | LPAREN prefixTyparDeclList RPAREN {  matchPair 1 3; List.rev $2 }

prefixTyparDeclList:
  | prefixTyparDeclList COMMA typarDecl { $3 :: $1 }
  | typarDecl { [$1] }

typarDecl :
  | opt_attributes typar { TyparDecl($1,$2) }

postfixTyparDecls:
  | opt_HIGH_PRECEDENCE_TYAPP LESS prefixTyparDeclList opt_typeConstraints GREATER { List.rev $3, $4 }

explicitValTyparDeclsCore:
  | prefixTyparDeclList COMMA DOT_DOT
      { deprecated "this declaration form is deprecated. Either specify all relevant type parameters or none" (lhs());
        (List.rev $1,true) }
  | DOT_DOT
      { deprecated "this declaration form is deprecated. Either specify all relevant type parameters or none" (lhs());
        ([],true) }
  | prefixTyparDeclList
      { (List.rev $1,false) }
  |
      { ([],false) }

explicitValTyparDecls:
  | opt_HIGH_PRECEDENCE_TYAPP LESS explicitValTyparDeclsCore opt_typeConstraints GREATER
      { let tps,flex = $3 in
         ValTyparDecls(tps,flex,$4) }

opt_explicitValTyparDecls:
  | explicitValTyparDecls
      { $1 }
  |
      { ValTyparDecls([],true,[]) }

opt_explicitValTyparDecls2:
  | explicitValTyparDecls
      { Some $1 }
  |
      { None }

opt_typeConstraints:
  |
     { [] }
  | WHEN typeConstraints
     { List.rev $2 }

typeConstraints:
  | typeConstraints AND typeConstraint { $3 :: $1 }
  | typeConstraint { [$1] }

typeConstraint:
  | DEFAULT typar COLON typ
      { libraryOnly (lhs()); WhereTyparDefaultsToType($2,$4,lhs()) }
  | typar COLON_GREATER typ
      { WhereTyparSubtypeOfType($1,$3,lhs()) }
  | typar COLON STRUCT
      { WhereTyparIsValueType($1,lhs()) }
  | typar COLON IDENT STRUCT
      { if $3 <> "not" then report_parse_error_at (rhs 3) ("unexpected identifier: '"^ $3 ^"'");
        WhereTyparIsReferenceType($1,lhs()) }
  | typar COLON NULL
      { WhereTyparSupportsNull($1,lhs()) }
  | typar COLON LPAREN classMemberSpfn RPAREN
      { WhereTyparSupportsMember([ $1 ],$4,lhs()) }
  | LPAREN typar OR typar RPAREN COLON LPAREN classMemberSpfn RPAREN
      { WhereTyparSupportsMember([ $2 ; $4 ],$8,lhs()) }
  | typar COLON DELEGATE typeArgs
      { WhereTyparIsDelegate($1,$4,lhs()) }
  | typar COLON IDENT typeArgs
      { match $3 with
        | "enum" -> WhereTyparIsEnum($1,$4,lhs())
        | nm -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 3) ("unexpected identifier: '"^ nm ^"'") }

unionRepr:
  /* Note the next three rules are required to disambiguate this from type x = y */
  /* Attributes can only appear on a single constructor if you've used a | */
  | BAR attrUnionCaseDecls
     { $2 }
  | firstUnionCaseDeclOfMany BAR attrUnionCaseDecls
     { $1 :: $3 }
  | firstUnionCaseDecl
     { [$1] }

attrUnionCaseDecls:
  | attrUnionCaseDecl BAR attrUnionCaseDecls  { $1 :: $3 }
  | attrUnionCaseDecl { [ $1 ] }

attrUnionCaseDecl:
  | opt_attributes opt_access grab_doc unionCaseName opt_OBLOCKSEP
      { startName(rhs 1); Choice2 (UnionConstr ( $1, $4,ConstrFields [],$3,$2,rhs 4)) }
  | opt_attributes opt_access grab_doc unionCaseName OF unionCaseRepr  opt_OBLOCKSEP
      { startName(rhs 1); Choice2 (UnionConstr ( $1, $4,ConstrFields $6,$3,$2,rhs2 4 6)) }
  | opt_attributes opt_access grab_doc unionCaseName COLON topType opt_OBLOCKSEP
      { libraryOnly(lhs());
        startName(rhs 1); Choice2 (UnionConstr ( $1, $4,ConstrFullType $6,$3,$2,rhs2 4 6)) }
  | opt_attributes opt_access grab_doc unionCaseName EQUALS constant opt_OBLOCKSEP
      { if isSome $2 then errorR(Error("visibility declarations are not permitted on enumeration fields",rhs 2));
        startName(rhs 1);
        Choice1 (EnumConstr ( $1, $4,$6,$3,rhs2 4 6)) }

/* REVIEW: unify this with operatorName! */
unionCaseName:
  | nameop
      { $1 }
  | LPAREN COLON_COLON RPAREN
      {  matchPair 1 3; ident(opname_Cons,rhs 2) }
  | LPAREN LBRACK RBRACK  RPAREN
      {  matchPair 1 4; ident(opname_Nil,rhs2 2 3) }

firstUnionCaseDeclOfMany:
  | ident opt_OBLOCKSEP
      { startName(rhs 1); Choice2 (UnionConstr ( [], $1,ConstrFields [],emptyXMLDoc,None,lhs())) }
  | ident EQUALS constant opt_OBLOCKSEP
      { startName(rhs 1); Choice1 (EnumConstr ([],$1,$3,emptyXMLDoc,lhs())) }
  | firstUnionCaseDecl opt_OBLOCKSEP
      { $1 }

firstUnionCaseDecl:
  | ident OF unionCaseRepr
     { startName(rhs 1); Choice2 (UnionConstr ( [],$1,ConstrFields $3,emptyXMLDoc,None,lhs())) }

unionCaseRepr:
  | braceFieldDeclList
     { warning(Error("This declaration form is deprecated. Consider using a separate record type instead",lhs())); $1 }
  | appType STAR tupleTypeElements
     { List.map anon_field_of_typ ($1 :: $3) }
  | appType
     { [anon_field_of_typ $1] }

recdFieldDeclList:
  | recdFieldDecl seps recdFieldDeclList
     { $1 :: $3 }
  | recdFieldDecl opt_seps
     { [$1] }

recdFieldDecl:
  | opt_attributes  fieldDecl
     { $2 $1 false }

fieldDecl:
  | opt_mutable  opt_access ident grab_doc COLON  polyType
     { startName(rhs 3);
       let rhsm = rhs2 3 6 in
       (fun attrs stat -> Field(attrs, stat,Some $3,$6,$1,$4,$2,rhsm)) }


exconDefn:
  | exconCore opt_classDefn
     { ExconDefn($1,$2, lhs()) }

exconSpfn:
  | exconCore opt_classSpfn
     { ExconSpfn($1,$2,lhs()) }

exconCore:
  | EXCEPTION opt_attributes grab_doc opt_access exconIntro exconRepr
     { ExconCore($2,$5, $6,$3,$4,lhs()) }

exconIntro:
  | ident
      { startName(rhs 1); UnionConstr ( [], $1,ConstrFields [],emptyXMLDoc,None,lhs()) }
  | ident OF unionCaseRepr
      { startName(rhs 1); UnionConstr ( [], $1,ConstrFields $3,emptyXMLDoc,None,lhs()) }

exconRepr:
  |             { None }
  | EQUALS path { Some ($2) }

openDecl:
  |  OPEN path { $2 }

defnBindings:
  | LET opt_rec localBindings
      { let letm = rhs 1 in
        let isUse,isRec,bindingsPreAttrs = $1,$2,$3 in
        (* the first binding swallow any attributes prior to the 'let' *)
        rhs 1,isRec,isUse,(fun attrs vis -> [],bindingsPreAttrs attrs vis) }
  | cPrototype
      { lhs(), false,false,$1  }
  /* REVIEW: why not do DO here too? After all we ODO below! */

doBinding:
  | DO typedSeqExprBlock
      { let letm = rhs 1 in
        let wholem = rhs2 1 2 in
        (* any attributes prior to the 'let' are left free, e.g. become top-level attributes *)
        (* associated with the module, 'main' function or assembly depending on their target *)
        letm,false,false,(fun attrs vis -> attrs,[mk_Do (true,$2,wholem)]) }


hardwhiteLetBindings:
  | OLET opt_rec localBindings hardwhiteDefnBindingsTerminator
      { $4 (rhs 1);  (* report unterminated error *)
        let letm = rhs 1 in
        let isUse,isRec,bindingsPreAttrs = $1,$2,$3 in
        (* the first binding swallow any attributes prior to the 'let' *)
        letm,isRec,isUse,(fun attrs vis -> [],bindingsPreAttrs attrs vis) }

hardwhiteDoBinding:
  | ODO typedSeqExprBlock hardwhiteDefnBindingsTerminator
      { $3 (rhs 1);  (* report unterminated error *)
        let letm = rhs 1 in
        let wholem = rhs2 1 2 in
        (* any attributes prior to the 'let' are left free, e.g. become top-level attributes *)
        (* associated with the module, 'main' function or assembly depending on their target *)
        letm,false,false,(fun attrs vis -> attrs,[mk_Do (true,$2,wholem)]) }

hardwhiteDefnBindings:
  | hardwhiteLetBindings { $1 }
  | hardwhiteDoBinding  { $1 }


hardwhiteDefnBindingsTerminator:
  |  ODECLEND
     { (fun m -> ()) }
  |  recover
     { (fun m -> report_parse_error_at m "unmatched 'let' or 'do'") }

cPrototype:
  | EXTERN cRetType opt_access ident opt_HIGH_PRECEDENCE_APP LPAREN cArgs RPAREN
      { let rty,vis,nm,args  = $2,$3,$4,$7 in
        let nmm = rhs 4 in
        let argsm = rhs 7 in
        let bindm = lhs() in
        let wholem = lhs() in
        let rhsm = lhs() in
        let rhsexpr = Expr_app(Expr_lid_get(false,[ident("failwith",rhsm)],rhsm),Expr_const(Const_string(Bytes.string_as_unicode_bytes "extern was not given a DllImport attribute",rhsm),rhsm),rhsm) in
        (fun attrs vis -> [], [mksyn_binding (grabXML(),Pat_lid ([nm],Some(noInferredTypars),[Pat_tuple(args,argsm)],vis,nmm)) vis false false bindm wholem (Some(rty)) rhsexpr rhsm [] attrs None]) }

cArgs:
  | cMoreArgs
     { List.rev $1 }
  | cArg
     { [$1] }
  |
     { [] }

cMoreArgs:
  | cMoreArgs COMMA cArg
     { $3 :: $1 }
  | cArg COMMA cArg
     { [$3; $1] }

cArg:
  | opt_attributes cType
     { let m = lhs() in Pat_typed(Pat_wild m,$2,m) |> addAttribs $1 }
  | opt_attributes cType ident
     { let m = lhs() in Pat_as (Pat_typed(Pat_wild m,$2,m),$3,false,None,m) |> addAttribs $1 }

cType:
  | ident
     { let m = lhs() in Type_app([$1],[],m) }
  | cType opt_HIGH_PRECEDENCE_APP LBRACK RBRACK
     { let m = lhs() in Type_app([ident("[]",m)],[$1],m) }
  | cType STAR
     { let m = lhs() in Type_app([ident("nativeptr",m)],[$1],m) }
  | cType AMP
     { let m = lhs() in Type_app([ident("byref",m)],[$1],m) }
  | VOID STAR
     { let m = lhs() in Type_app([ident("nativeint",m)],[],m) }

cRetType:
  | opt_attributes cType
     { ($2,TopArgSynData($1,false,None)),rhs 2 }
  | opt_attributes VOID
     { let m = rhs 2 in (Type_app([ident("unit",m)],[],m),TopArgSynData($1,false,None)),m }


localBindings:
  | attr_localBinding more_localBindings
      { (fun attrs vis ->
           match $1 with
           | Some x -> (x attrs vis None true ::  $2)
           | None -> $2) }

more_localBindings:
  | AND attr_localBinding more_localBindings
      { (match $2 with Some x -> x [] None None false :: $3 | None -> $3) }
  | %prec prec_no_more_attr_bindings
      { [] }

attr_localBinding:
  | DO typedSeqExprBlock
      { let m = rhs2 1 2 in
        Some(fun _ _ _ isFirst ->
          if isFirst then deprecated "Consider using 'let _ = expr in expr' or simply 'expr; expr' instead" m;
          mk_Do (true,$2,m)) }
  | opt_attributes localBinding
      { Some(fun attrs vis memberInfo isFirst ->
          $2 (attrs@$1) vis memberInfo) }
  | error
      { None }

localBinding:
  | opt_inline opt_mutable bindingPattern  opt_topReturnTypeWithTypeConstraints EQUALS  typedExprWithStaticOptimizationsBlock
      { let bindm = rhs 3 in
        let wholem = rhs 6 in
        let expr,opts = $6 in
        let rhsm = rhs 6 in
        let optReturnType = $4 in
        let bindingBuilder = $3 in
        (fun attrs vis memberInfo ->
            bindingBuilder vis $1 $2 bindm wholem optReturnType expr rhsm opts attrs memberInfo) }
  | opt_inline opt_mutable bindingPattern  opt_topReturnTypeWithTypeConstraints EQUALS  error
      { let bindm = rhs 3 in
        let wholem = rhs2 3 5 in
        let rhsm = rhs 5 in
        let optReturnType = $4 in
        let bindingBuilder = $3 in
        (fun attrs vis memberInfo ->
            bindingBuilder vis $1 $2 bindm wholem optReturnType (arbExpr()) rhsm [] attrs memberInfo)  }

/* REVIEW: this should probably be an expression form rather than tied to this particular part of the grammar */
typedExprWithStaticOptimizationsBlock:
  | OBLOCKBEGIN typedExprWithStaticOptimizations OBLOCKEND { $2 }
  | typedExprWithStaticOptimizations { $1 }

typedExprWithStaticOptimizations :
  | typedSeqExpr opt_staticOptimizations { $1,$2 }

opt_staticOptimizations:
  | opt_staticOptimizations staticOptimization { $2 :: $1 }
  | { [] }

staticOptimization:
  | WHEN staticOptimizationConditions EQUALS typedSeqExpr { ($2,$4) }

staticOptimizationConditions:
  | staticOptimizationConditions AND staticOptimizationCondition { $3 :: $1 }
  | staticOptimizationCondition { [$1 ] }

staticOptimizationCondition:
  | typar COLON typ { WhenTyparTyconEqualsTycon($1,$3,lhs()) }

constant:
  | INT8 { Const_int8 $1 }
  | UINT8 { Const_uint8 $1 }
  | INT16 { Const_int16 $1 }
  | UINT16 { Const_uint16 $1 }
  | INT32 { Const_int32 $1 }
  | UINT32 { Const_uint32 $1 }
  | INT64 { Const_int64 $1 }
  | UINT64 { Const_uint64 $1 }
  | NATIVEINT { Const_nativeint $1 }
  | UNATIVEINT { Const_unativeint $1 }
  | IEEE32 { Const_float32 $1 }
  | IEEE64 { Const_float $1 }
  | CHAR { Const_char $1 }
  | BIGINT { Const_bigint $1 }
  | DECIMAL { Const_decimal $1 }
  | BIGNUM { Const_bignum $1 }
  | STRING { Const_string ($1,lhs()) }
  | BYTEARRAY { Const_bytearray ($1,lhs()) }

bindingPattern:
  | headBindingPattern   grab_doc
      {  mksyn_binding ($2,$1) }

/* sp = v | sp:typ | attrs sp */
simplePattern:
  | ident
      { startName(rhs 1); SPat_as ($1,false,false,rhs 1) }
  | QMARK ident
      { startName(rhs 2); SPat_as ($2,false,true,rhs 2) }
  | simplePattern COLON typeWithTypeConstraints
      { let lhsm = lhs() in
        SPat_typed($1,$3,lhsm) }
  | attributes simplePattern %prec paren_pat_attribs
      { let lhsm = lhs()  in
        SPat_attrib($2,$1,lhsm) }

simplePatternCommaList:
  | simplePattern { [$1] }
  | simplePattern COMMA simplePatternCommaList { $1 :: $3 }

simplePatterns:
  | LPAREN simplePatternCommaList RPAREN { $2 }
  | LPAREN RPAREN { [] }
  | LPAREN simplePatternCommaList recover { report_parse_error_at (rhs 1) "unmatched '('"; [] }
  | LPAREN error RPAREN { (* silent recovery *) [] }
  | LPAREN recover {  report_parse_error_at (rhs 1) "unmatched '('"; [] }


headBindingPattern:
  | headBindingPattern AS ident
      { startName(rhs 3); Pat_as ($1,$3,false,None,rhs2 1 3) }
  | headBindingPattern BAR headBindingPattern
      { Pat_disj($1,$3,rhs2 1 3) }
  | headBindingPattern COLON_COLON  headBindingPattern
      { Pat_lid (mksyn_constr (rhs2 1 3) opname_Cons, None,[Pat_tuple ([$1;$3],rhs2 1 3)],None,lhs()) }
  | tuplePatternElements  %prec pat_tuple
      { Pat_tuple(List.rev $1, lhs()) }
  | conjPatternElements   %prec pat_conj
      { Pat_conjs(List.rev $1, lhs()) }
  | constrPattern
      { $1 }

tuplePatternElements:
  | tuplePatternElements COMMA headBindingPattern { $3 :: $1 }
  | headBindingPattern COMMA headBindingPattern { $3 :: $1 :: [] }

conjPatternElements:
  | conjPatternElements AMP headBindingPattern { $3 :: $1 }
  | headBindingPattern AMP headBindingPattern { $3 :: $1 :: [] }

constrPattern:
  | atomicPatternLongIdent explicitValTyparDecls                                                          { let vis,lid = $1 in Pat_lid (lid,Some $2,[],vis,lhs()) }
  | atomicPatternLongIdent opt_explicitValTyparDecls2                     atomicPatterns    %prec pat_app { let vis,lid = $1 in Pat_lid (lid,$2,$3,vis,lhs()) }
  | atomicPatternLongIdent opt_explicitValTyparDecls2 HIGH_PRECEDENCE_APP atomicPatterns                  { let vis,lid = $1 in Pat_lid (lid,$2,$4,vis,lhs()) }
  | COLON_QMARK atomType  %prec pat_isinst { Pat_isinst($2,lhs()) }
  | atomicPattern { $1 }

atomicPatterns:
  | atomicPattern atomicPatterns %prec pat_args { $1 :: $2 }
  | atomicPattern HIGH_PRECEDENCE_APP atomicPatterns
      { report_parse_error_at (rhs 1) "Successive patterns should be separated by spaces or tupled";
        $1 :: $3 }
  | atomicPattern { [$1] }


atomicPattern:
  | quoteExpr
      { Pat_expr($1,lhs()) }
  | CHAR DOT_DOT CHAR { Pat_range ($1,$3,rhs2 1 3) }
  | LBRACE recordPatternElements RBRACE
      { $2 }
  | LBRACK listPatternElements RBRACK
      { matchPair 1 3; Pat_array_or_list(false,$2,lhs()) }
  | LBRACK_BAR listPatternElements  BAR_RBRACK
      { matchPair 1 3; Pat_array_or_list(true,$2, lhs()) }
  | UNDERSCORE { Pat_wild (lhs()) }
  /* DELETE THIS TO ENABLE NEGATIVE DESIGN CHANGE */
  | MINUS INT32 { Pat_const (Const_int32 (Int32.neg $2),lhs()) }
  /* DELETE THIS TO ENABLE NEGATIVE DESIGN CHANGE */
  | ADJACENT_PREFIX_PLUS_MINUS_OP INT32
      { match $1 with
        | "-" -> Pat_const (Const_int32 (Int32.neg $2),lhs())
        | "+" -> Pat_const (Const_int32 $2,lhs())
        | _ -> report_parse_error_at (rhs 1) "syntax error";
               Pat_const (Const_int32 $2,lhs()) }
  | QMARK ident { Pat_opt_var($2,lhs()) }
  | atomicPatternLongIdent %prec prec_atompat_pathop
      { let vis,lid = $1 in
        if List.length lid > 1 or (let c= String.get (List.hd lid).idText 0 in Char.uppercase c = c)
        then mksyn_pat_maybe_var lid vis (lhs())
        else mksyn_pat_var vis (List.hd lid) }
  | constant { Pat_const ($1,range_of_synconst $1 (lhs())) }
  | FALSE  { Pat_const(Const_bool false,lhs()) }
  | TRUE  { Pat_const(Const_bool true,lhs()) }
  | NULL { Pat_null(lhs()) }
  | LPAREN parenPatternBody RPAREN {  matchPair 1 3; let m = (lhs()) in Pat_paren($2 m,m) }
  | LPAREN parenPatternBody recover { report_parse_error_at (rhs 1) "unmatched '('"; $2 (rhs2 1 2) }
  | LPAREN error RPAREN { (* silent recovery *) Pat_wild (lhs()) }
  | LPAREN recover {  report_parse_error_at (rhs 1) "unmatched '('"; Pat_wild (lhs())}



parenPatternBody:
  | parenPattern
      { (fun m -> $1) }
  |
      { (fun m -> Pat_const(Const_unit,m)) }

/* This duplicates out 'patterns' in order to give type annotations */
/* the desired precedence w.r.t. patterns, tuple patterns in particular. */
/* Duplication requried to minimize the disturbance to the grammar, */
/* in particular the expected property that "pat" parses the same as */
/* "(pat)"!  Here are some examples: */
/*    a,b                  parses as (a,b) */
/*    (a,b)           also parses as (a,b) */
/*    (a,b : t)            parses as (a, (b:t)) */
/*    a,b as t             parses as ((a,b) as t) */
/*    (a,b as t)      also parses as ((a,b) as t) */
/*    a,b | c,d            parses as ((a,b) | (c,d)) */
/*    (a,b | c,d)     also parses as ((a,b) | (c,d)) */
/*    (a : t,b)            parses as ((a:t),b) */
/*    (a : t1,b : t2)      parses as ((a:t),(b:t2)) */
/*    (a,b as nm : t)      parses as (((a,b) as nm) : t) */
/*    (a,b :: c : t)       parses as (((a,b) :: c) : t) */
/* */
/* Probably the most unexpected thing here is that 'as nm' binds the */
/* whole pattern to the left, whereas ': t' binds only the pattern */
/* immediately preceding in the tuple. */
/* */
/* Also, it is unexpected that '(a,b : t)' in a pattern binds differently to */
/* '(a,b : t)' in an expression. It's not that easy to solve that without */
/* duplicating the entire expression grammar, or making a fairly severe breaking change */
/* to the language. */
parenPattern:
  | parenPattern AS ident
      { startName(rhs 3); Pat_as ($1,$3,false,None,rhs2 1 3) }
  | parenPattern BAR parenPattern
      { Pat_disj($1,$3,rhs2 1 3) }
  | tupleParenPatternElements
      { Pat_tuple(List.rev $1,lhs()) }
  | conjParenPatternElements
      { Pat_conjs(List.rev $1,rhs2 1 3) }
  | parenPattern COLON  typeWithTypeConstraints %prec paren_pat_colon
      { let lhsm = lhs() in
        Pat_typed($1,$3,lhsm) }
  | attributes parenPattern  %prec paren_pat_attribs
      { let lhsm = lhs()  in
        Pat_attrib($2,$1,lhsm) }
  | parenPattern COLON_COLON  parenPattern
      { Pat_lid (mksyn_constr (rhs2 1 3) opname_Cons, None, [ Pat_tuple ([$1;$3],rhs2 1 3) ],None,lhs()) }
  | parenPattern COLON_GREATER typ
      { let lhsm = lhs() in
        Pat_typed($1, mksyn_anon_constraint $3 lhsm,lhsm) }
  | constrPattern { $1 }

tupleParenPatternElements:
  | tupleParenPatternElements COMMA parenPattern  { $3 :: $1 }
  | parenPattern COMMA parenPattern  { $3 :: $1 :: [] }

conjParenPatternElements:
  | conjParenPatternElements AMP parenPattern { $3 :: $1 }
  | parenPattern AMP parenPattern { $3 :: $1 :: [] }


recordPatternElements:
  | path EQUALS parenPattern  moreRecordPatternElements
      { Pat_recd ((frontAndBack $1,$3) :: List.rev $4,lhs()) }

moreRecordPatternElements:
  | moreRecordPatternElements seps path EQUALS  parenPattern
     { (frontAndBack $3,$5)::$1 }
  |
     { [] }

listPatternElements:
  | parenPattern  moreListPatternElements
    { $1 :: List.rev $2 }
  |
    { [] }

moreListPatternElements:
  | moreListPatternElements seps parenPattern
     { $3 :: $1 }
  |
     { [] }

/* The lexfilter likes to insert OBLOCKBEGIN/OBLOCKEND pairs */
typedSeqExprBlock:
  | OBLOCKBEGIN typedSeqExpr OBLOCKEND { $2 }
  | typedSeqExpr { $1 }

/* For some constructs the lex filter can't be sure to insert a matching OBLOCKEND, e.g. "function a -> b | c -> d" all in one line */
/* for these it only inserts a trailing ORIGHT_BLOCK_END */
typedSeqExprBlockR:
  | typedSeqExpr ORIGHT_BLOCK_END { $1 }
  | typedSeqExpr { $1 }

typedSeqExpr:
  | seqExpr COLON               typeWithTypeConstraints { Expr_typed ($1,$3, lhs()) }
  | seqExpr COLON_QMARK         typ  %prec expr_isinst  { Expr_isinst($1,$3,lhs()) }
  | seqExpr COLON_GREATER       typ                     { Expr_upcast($1,$3,lhs()) }
  | seqExpr COLON_QMARK_GREATER typ                     { Expr_downcast($1,$3,lhs()) }
  | seqExpr { $1 }

seqExpr:
  | declExpr seps seqExpr                 { Expr_seq(true,$1,$3,lhs()) }
  | declExpr seps                         { $1 }
  | declExpr             %prec SEMICOLON { $1 }
  | declExpr THEN seqExpr %prec prec_then_before { Expr_seq(false,$1,$3,lhs()) }
  | declExpr OTHEN OBLOCKBEGIN typedSeqExpr OBLOCKEND %prec prec_then_before { Expr_seq(false,$1,$4,lhs()) }

/* use this as the last terminal when performing silent error recovery */
/* in situations where a syntax error has definitely occurred. This allows */
/* the EOF token to be swallowed to help initiate error recovery. */
recover: error { }  | EOF {report_parse_error_at (lhs()) "syntax error at end of file" }

declExpr:
  | defnBindings IN typedSeqExpr  %prec expr_let
     { mkLocalBindings (rhs2 1 3) $1 $3 }
  | defnBindings IN error        %prec expr_let
     { mkLocalBindings (rhs2 1 2) $1 (arbExpr()) }
  | defnBindings error        %prec expr_let
    { report_parse_error_at (match $1 with (m,_,_,_)  -> m) "no matching 'in' found for this 'let'";
      mkLocalBindings (rhs2 1 2) $1 (arbExpr()) }
  | hardwhiteDefnBindings typedSeqExprBlock  %prec expr_let
     { mkLocalBindings (rhs2 1 2) $1 $2 }
  | hardwhiteDefnBindings error        %prec expr_let
     { report_parse_error_at (match $1 with (m,_,_,_)  -> m) "error in the return expression for this 'let'. Possible incorrect indentation";
      mkLocalBindings (rhs2 1 2) $1 (arbExpr()) }
  | hardwhiteDefnBindings OBLOCKSEP typedSeqExprBlock  %prec expr_let
     { mkLocalBindings (rhs2 1 3) $1 $3 }

  | hardwhiteDefnBindings OBLOCKSEP error        %prec expr_let
     { report_parse_error_at (match $1 with (m,_,_,_)  -> m) "error in the return expression for this 'let'. Possible incorrect indentation";
      mkLocalBindings (rhs2 1 2) $1 (arbExpr()) }
  | anonMatchingExpr %prec expr_function
      { $1 }
  | anonLambdaExpr  %prec expr_fun { $1 }

  | MATCH typedSeqExpr     withClausses              %prec expr_match { Expr_match( $2,$3,false,rhs2 1 3) }
  | TRY typedSeqExprBlockR withClausses              %prec expr_try { Expr_try_catch($2, rhs 2, $3,rhs 3, lhs()) }
  | TRY typedSeqExprBlockR FINALLY typedSeqExprBlock %prec expr_try { Expr_try_finally($2, $4,rhs 4) }

  | IF declExpr ifExprCases %prec expr_if { $3 $2 (lhs()) }

  | LAZY declExpr %prec expr_lazy { mksyn_lazy ($2,lhs()) }

  | ASSERT declExpr %prec expr_assert { Expr_assert($2, lhs()) }
  | ASSERT %prec expr_assert { let m = lhs() in mksyn_lid_get m ["Microsoft";"FSharp";"Core";"Operators"] "assert" }

  | WHILE declExpr do_or_odo typedSeqExprBlock done_term { Expr_while($2,$4,lhs()) }
  | WHILE declExpr do_or_odo typedSeqExprBlock recover { report_parse_error_at (rhs 4) "'done' expected after this expression" ;  arbExpr() }
  | WHILE declExpr do_or_odo error done_term { (* silent recovery *) arbExpr() }
  | WHILE declExpr recover { report_parse_error_at (rhs 2) "'do' expected after this expression" ; arbExpr() }
  | WHILE error done_term { (* silent recovery *) arbExpr()  }

  | FOR forLoopBinder do_or_odo typedSeqExprBlock done_term { let (a,b) = $2 in Expr_foreach(a,b,$4,lhs()) }
  | FOR forLoopRange  do_or_odo typedSeqExprBlock done_term { let (a,b,c,d) = $2 in Expr_for(a,b,c,d,$4,lhs()) }
  | FOR forLoopRange  do_or_odo typedSeqExprBlock recover {   report_parse_error_at (rhs 1) "unclosed 'for', e.g. no 'done' found to match this 'for'" ; arbExpr() }
  | FOR forLoopRange  do_or_odo error done_term { (* silent recovery *) arbExpr() }
  | FOR error do_or_odo typedSeqExprBlock done_term { (* silent recovery *) arbExpr() }

/* do not include this one - though for fairly bizarre reasons!
   If the user has simply typed 'for'as the
   start of a variable name, and intellisense parsing
   kicks in, then we can't be sure we're parsing a for-loop. The general rule is that you shoudn't
   commit to aggressive look-for-a-matching-construct error recovery until
   you're sure you're parsing a particular construct.

  This probably affects 'and' as well, but it's hard to change that.
  'for' is a particularly common prefix of identifiers.

  | FOR error done_term {  report_parse_error_at (rhs 2)  "identifier expected"; arbExpr() }
*/

  | FOR parenPattern error done_term {  report_parse_error_at (rhs 3) "'=' expected"; arbExpr() }
  | declExpr COLON_EQUALS           declExpr { mksyn_infix (rhs 2) (lhs()) $1 ":=" $3 }
  | minusExpr LARROW                declExpr { mksyn_assign (lhs()) $1 $3 }
  | tupleExpr  %prec expr_tuple  { Expr_tuple( List.rev $1,lhs()) }
  | declExpr  BAR_BAR               declExpr { mksyn_infix (rhs 2) (lhs()) $1 "||" $3 }
  | declExpr  INFIX_BAR_OP          declExpr { mksyn_infix (rhs 2) (lhs()) $1 $2 $3 }
  | declExpr  OR                    declExpr { mksyn_infix (rhs 2) (lhs()) $1 "or" $3 }
  | declExpr  AMP                   declExpr { mksyn_infix (rhs 2) (lhs()) $1 "&" $3 }
  | declExpr  AMP_AMP               declExpr { mksyn_infix (rhs 2) (lhs()) $1 "&&" $3 }
  | declExpr  INFIX_AMP_OP          declExpr { mksyn_infix (rhs 2) (lhs()) $1 $2 $3 }
  | declExpr  EQUALS                declExpr { mksyn_infix (rhs 2) (lhs()) $1 "=" $3 }
  | declExpr  INFIX_COMPARE_OP      declExpr { mksyn_infix (rhs 2) (lhs()) $1 $2 $3 }
  | declExpr  DOLLAR                declExpr { mksyn_infix (rhs 2) (lhs()) $1 "$" $3 }
  | declExpr  LESS                  declExpr { mksyn_infix (rhs 2) (lhs()) $1 "<" $3 }
  | declExpr  GREATER               declExpr { mksyn_infix (rhs 2) (lhs()) $1 ">" $3 }
  | declExpr  INFIX_AT_HAT_OP       declExpr { mksyn_infix (rhs 2) (lhs()) $1 $2 $3 }
  | declExpr  PERCENT_OP            declExpr { mksyn_infix (rhs 2) (lhs()) $1 $2 $3 }
  | declExpr  COLON_COLON           declExpr { Expr_app (mksyn_item (lhs()) opname_Cons,Expr_tuple ([$1;$3],lhs()),lhs()) }
  | declExpr  PLUS_MINUS_OP         declExpr { mksyn_infix (rhs 2)  (lhs()) $1 $2 $3 }
  | declExpr  MINUS                 declExpr { mksyn_infix (rhs 2) (lhs()) $1 "-" $3 }
  | declExpr  ADJACENT_PREFIX_PLUS_MINUS_OP declExpr
      { warning(Deprecated("In a future release of F# expressions of the form '-expr' and '+expr' in composite expressions will be treated as arguments. Consider using either 'expr-expr' or 'expr - expr' for infix subtraction, but not 'expr -expr', and likewise for addition",lhs()));
        mksyn_infix (rhs 2) (lhs()) $1 $2 $3 }
  | declExpr  STAR                  declExpr { mksyn_infix (rhs 2) (lhs()) $1 "*" $3 }
  | declExpr  INFIX_STAR_DIV_MOD_OP declExpr { mksyn_infix (rhs 2) (lhs()) $1 $2 $3 }
  | declExpr  INFIX_STAR_STAR_OP    declExpr { mksyn_infix (rhs 2) (lhs()) $1 $2 $3 }
  | declExpr  QMARK_QMARK           declExpr
      { warning(Error("This language construct is deprecated. Consider using 'match <expr1> with null -> <expr2> | v -> v",lhs()));
        Expr_ifnull($1,$3,lhs()) }
  | minusExpr %prec expr_prefix_plus_minus { $1 }

withClausses:
  | WITH withPatternClauses       { $2 }
  | OWITH withPatternClauses OEND { $2 }

withPatternClauses:
  | patternClauses { $1 }
  | BAR patternClauses
    {  (* startName(rhs 1); *) (* hack - register as name so intellisense tells us the name '|' when this trigger fires *)
       $2 }
  | BAR error
    {  (* startName(rhs 1); *) (* hack - register as name so intellisense tells us the name '|' when this trigger fires *)
       (* silent recovery *)  [] }
  | error
    {  (* silent recovery *)  [] }


patternAndGuard:
  | parenPattern patternGuard
      { $1, $2, rhs 1 }

patternClauses:
  | patternAndGuard patternResult %prec prec_pat_pat_action
     { let pat,guard,patm = $1 in [Clause(pat,guard,$2,patm)]  }
  | patternAndGuard patternResult BAR patternClauses
     { startName(rhs 3); (* hack - register as name so intellisense tells us the name '|' when this trigger fires *)
       let pat,guard,patm = $1 in Clause(pat,guard,$2,patm) :: $4 }
  | patternAndGuard patternResult BAR error
     { startName(rhs 3); (* hack - register as name so intellisense tells us the name '|' when this trigger fires *)
       let pat,guard,patm = $1 in [Clause(pat,guard,$2,patm)] (* silent recovery *) }
  | patternAndGuard patternResult error
     { let pat,guard,patm = $1 in [Clause(pat,guard,$2,patm)] (* silent recovery *) }

patternGuard:
  | WHEN declExpr
     { Some $2 }
  |
     { None }

patternResult:
  | RARROW typedSeqExprBlockR
     { $2 }

ifExprCases:
  | ifExprThen ifExprElifs { (fun g m -> Expr_cond(g,$1,$2,m)) }
/*   | error ifExprElifs      { report_parse_error_at (rhs 1) "'then' expected"; (fun g m -> g) }  */

ifExprThen:
  | THEN  declExpr %prec prec_then_if { $2 }
  | OTHEN  OBLOCKBEGIN typedSeqExpr OBLOCKEND %prec prec_then_if { $3 }

ifExprElifs:
  |
      { None }
  | ELSE declExpr
      { Some $2 }
  | OELSE  OBLOCKBEGIN typedSeqExpr OBLOCKEND
      { Some $3 }
  | ELIF declExpr ifExprCases
      { Some ($3 $2 (lhs())) }

tupleExpr:
  | tupleExpr COMMA declExpr
      { nextParameter (rhspos 2); $3 :: $1 }
  | declExpr COMMA declExpr
      { nextParameter (rhspos 2); $3 :: $1 :: [] }

minusExpr:
  | MINUS minusExpr   %prec expr_prefix_plus_minus
      { mksyn_prefix (lhs()) "~-" $2 }
  | PLUS_MINUS_OP minusExpr
      { mksyn_prefix (lhs()) ("~"^($1)) $2 }
  | ADJACENT_PREFIX_PLUS_MINUS_OP minusExpr   %prec expr_prefix_plus_minus
      { mksyn_prefix (lhs()) ("~"^($1)) $2 }
  | SPLICE_SYMBOL minusExpr
      { Expr_hole((ref None,Some (mksyn_prefix (lhs()) $1 $2)), lhs()) }
  | PERCENT_OP minusExpr
      { Expr_hole((ref None,Some (mksyn_prefix (lhs()) ("~"^($1)) $2)), lhs()) }
  | AMP  minusExpr
      { Expr_addrof(true,$2,lhs()) }
  | AMP_AMP  minusExpr
      { Expr_addrof(false,$2,lhs()) }
  | NEW appType opt_HIGH_PRECEDENCE_APP argExprAfterType
      { let arg = match $4 with (* None -> mksyn_unit (lhs()) | Some *) e -> e in
        Expr_new(false,$2,arg,lhs()) }
  | NEW appType opt_HIGH_PRECEDENCE_APP error
      { Expr_new(false,$2,arbExpr(),lhs()) }
  | UPCAST  minusExpr
      { Expr_arb_upcast($2,lhs()) }
  | DOWNCAST  minusExpr
      { Expr_arb_downcast($2,lhs())}
  | appExpr
      { $1 }

appExpr:
  | appExpr argExprNoHPA %prec expr_app
      { Expr_app ($1,$2,lhs())  }
  | argExpr
      { let arg,_ = $1 in
        arg }

argExprNoHPA:
   | argExpr
      { let arg,hpa = $1 in
        if hpa then report_parse_error_at (rhs 1) "Successive arguments should be separated by spaces or tupled, and arguments involving function or method applications should be parenthesized";
        arg }

argExpr:
  | argExpr HIGH_PRECEDENCE_APP argExpr
      { let arg1,_ = $1 in
        let arg2,_ = $3 in
        Expr_app (arg1,arg2,lhs()),true  }

  | argExpr HIGH_PRECEDENCE_TYAPP typeArgsActual
      { let arg1,_ = $1 in
        Expr_tyapp (arg1,$3,lhs()),true  }

  | PREFIX_OP  argExpr
      { let arg2,hpa2 = $2 in
        mksyn_prefix (lhs()) $1 arg2,hpa2 }
  | argExpr DOT argExprQualification
      { let arg1,hpa1 = $1 in
        $3 arg1 (lhs()) (rhs 2),hpa1 }
  | QMARK nameop
      { Expr_lid_get (true,[$2],rhs 2),false }
  | nameop
      { Expr_lid_get (false,[$1],rhs 1),false }
  | LBRACK listExprElements RBRACK
      { matchPair 1 3;
        $2 (lhs()) true,false }
  | LBRACK listExprElements recover
      { report_parse_error_at (rhs 1) "unmatched '['";
        $2 (rhs2 1 2) true, false }
  | LBRACK error RBRACK
      { matchPair 1 3;
        (* silent recovery *)
        Expr_array_or_list(false,[ ], lhs()),false  }
  | argExprAfterType
      { $1,false }

argExprQualification:
  |    identop
      { let idm = rhs 1 in
        (fun e lhsm dotm ->
              qualifyNameIfAlongside dotm idm;
              mksyn_dot lhsm e $1) }
  |    recover
      {
        (* silent recovery *) (fun e lhsm dotm -> qualifyName dotm dotm; e) }
  |   INT32
      { (fun e lhsm dotm ->
            libraryOnly (lhs());
            qualifyName dotm dotm; mksyn_dotn lhsm e $1) }
  |   LPAREN COLON_COLON RPAREN DOT INT32
      { (fun e lhsm dotm ->
            libraryOnly(lhs());
            Expr_constr_field_get (e,mksyn_constr lhsm opname_Cons,Int32.to_int $5,lhsm)) }
  |   LPAREN  typedSeqExpr RPAREN
      { startParameters (rhspos 1);
        endParameters (rhspos 3);
        matchPair 1 3;
        (fun e lhsm dotm ->
            ocamlCompat "The expression form 'expr.(expr)' is for use when OCaml compatibility is enabled. In F# code you may use 'expr.[expr]'. A type annotation may be required to indicate the first expression is an array" (lhs());
            qualifyName dotm dotm; mksyn_dot_lparen_get lhsm e $2) }
  |   LBRACK  typedSeqExpr RBRACK
      { matchPair 1 3;
        (fun e lhsm dotm ->
            qualifyName dotm dotm; mksyn_dot_lbrack_get lhsm e $2) }

  |   LBRACK  optRange RBRACK
      { matchPair 1 3;
        (fun e lhsm dotm -> qualifyName dotm dotm; mksyn_dot_lbrack_slice_get lhsm e $2) }
  |   LBRACK  optRange COMMA optRange RBRACK  %prec slice_comma
      { matchPair 1 5;
        (fun e lhsm dotm -> qualifyName dotm dotm; mksyn_dot_lbrack_slice2_get lhsm e $2 $4) }

optRange:
  | declExpr DOT_DOT declExpr { mk_optional (rhs 1) (Some $1), mk_optional (rhs 3) (Some $3) }
  | declExpr DOT_DOT { mk_optional (rhs 1) (Some $1), mk_optional (rhs 2) None }
  | DOT_DOT declExpr { mk_optional (rhs 1) None, mk_optional (rhs 2) (Some $2) }
  | STAR { mk_optional (rhs 1) None, mk_optional (rhs 1) None }


/* the start et of argExprAfterType must not overlap with the valid postfix tokens of the type syntax, e.g. new List<T>(...) */
argExprAfterType:
  | constant
      { Expr_const ($1,range_of_synconst $1 (lhs())) }
  | parenExpr
      { $1 }
  | braceExpr
      { $1 }
  | NULL
      { Expr_null(lhs()) }
  | FALSE
      { Expr_const(Const_bool false,lhs()) }
  | TRUE
      { Expr_const(Const_bool true,lhs()) }
  | quoteExpr
      { $1 }
  | arrayExpr
      { $1 }
  | beginEndExpr
      { $1 }
  | UNDERSCORE
      { Expr_hole((ref None, None),lhs()) }

beginEndExpr:
  | BEGIN typedSeqExpr END
      { Expr_paren($2,rhs2 1 3) }
  | BEGIN typedSeqExpr recover
      { report_parse_error_at (rhs 1) "unmatched 'begin'"; $2 }
  | BEGIN error END
      { (* silent recovery *) arbExpr()  }
  | BEGIN END
      { mksyn_unit (lhs()) }

quoteExpr:
  | LQUOTE typedSeqExpr RQUOTE
      { matchPair 1 3;
        if $1 <> $3 then report_parse_error_at (rhs 1) ("mismatched quotation, beginning with '"^ fst $1 ^ "'");
        (Expr_quote(mksyn_item (lhs()) (compileOpName (fst $1)), snd $1,$2,lhs())) }
  | LQUOTE typedSeqExpr recover
      { report_parse_error_at (rhs 1) ("unmatched '"^fst $1^"'");  Expr_quote(mksyn_item (lhs()) (compileOpName (fst $1)),snd $1,$2,rhs2 1 2)  }
  | LQUOTE error RQUOTE
      { matchPair 1 3; (* silent recovery *) Expr_quote(mksyn_item (lhs()) (compileOpName (fst $1)),snd $1,arbExpr(),lhs())  }

arrayExpr:
  | LBRACK_BAR listExprElements BAR_RBRACK
      {  matchPair 1 3; $2 (lhs()) false }
  | LBRACK_BAR listExprElements recover
      { report_parse_error_at (rhs 1) "unmatched '[|'";
        $2 (rhs2 1 2) false }
  | LBRACK_BAR error BAR_RBRACK
      {  matchPair 1 3; (* silent recovery *) Expr_array_or_list(true,[ ], lhs()) }

parenExpr:
  | LPAREN parenExprBody RPAREN
      { startParameters (rhspos 1); endParameters (rhspos 3); matchPair 1 3; $2 (rhs2 1 3) }
  | LPAREN parenExprBody recover
      { startParameters (rhspos 1); endParameters (rhspos 3); report_parse_error_at (rhs 1) "unmatched '('"; let lhsm = rhs2 1 2 in Expr_paren($2 lhsm,lhsm) }
  | LPAREN error RPAREN
      { startParameters (rhspos 1); endParameters (rhspos 3); matchPair 1 3; (* silent recovery *) arbExpr() }
  | LPAREN recover %prec prec_atomexpr_lparen_error
      {  startParameters (rhspos 1); endParameters (rhspos 2); report_parse_error_at (rhs 1) "unmatched '('"; arbExpr()  }

parenExprBody:
  |
      {  (fun m -> Expr_const(Const_unit,m)) }
  | TYPE typ
      {  (fun  m -> Expr_typeof($2,m)) }
  | staticallyKnownHeadTypars COLON LPAREN classMemberSpfn RPAREN  LPAREN typedSeqExpr RPAREN
      {  matchPair 3 5;
         matchPair 6 8;
         (fun m -> Expr_trait_call($1,$4,(match $7 with Expr_tuple(l,_) -> l | e -> [e]),m)) } /* disambiguate: x $a.id(x) */
  | typedSeqExpr
      { (fun m -> Expr_paren($1,m)) }
  | inlineAssemblyExpr
      { $1 }

staticallyKnownHeadTypars:
  | staticallyKnownHeadTypar { [$1] }
  | LPAREN staticallyKnownHeadTypar OR staticallyKnownHeadTypar RPAREN { [$2 ; $4 ] }

braceExpr:
  | LBRACE braceExprBody RBRACE
     {  matchPair 1 3; $2 (lhs()) }
  | LBRACE braceExprBody recover
     { report_parse_error_at (rhs 1) "unmatched '{'" ; $2 (lhs()) }
  | LBRACE error RBRACE
     { matchPair 1 3; (* silent recovery *) arbExpr()  }

braceExprBody:
  | recdExpr
     {  (fun m -> let a,b,c = $1 in Expr_recd(a,b,c,m)) }
  | objExpr
     { $1 }
  | monadicExprInitial
     { $1  }

listExprElements:
  | monadicExprInitial
     { (fun lhsm isList -> if isList then Expr_list_of_seq($1 lhsm,lhsm) else Expr_array_of_seq($1 lhsm,lhsm) ) }
  | declExpr  moreListExprElements opt_seps
     { (fun lhsm isList -> let elems = ($1 :: List.rev $2) in Expr_array_or_list(not isList, elems, lhsm)) }
  |
     { (fun lhsm isList -> Expr_array_or_list(not isList,[ ], lhsm)) }

moreListExprElements:
  | moreListExprElements seps declExpr
     { $3 :: $1 }
  |
     { [] }

monadicExprInitial:
  | monadicExprNonEmptyInitial
     { (fun lhsm -> Expr_comprehension($1,lhsm)) }
  | rangeSequenceExpr
     { $1 }

rangeSequenceExpr:
  | declExpr TO       declExpr  %prec expr_let
     { deprecated "use 'expr .. expr' instead" (lhs()); (fun m -> mksyn_infix m m $1 ".." $3) }
  | declExpr DOT_DOT  declExpr
     { (fun m -> mksyn_infix m m $1 ".." $3) }
  | declExpr DOT_DOT  declExpr DOT_DOT declExpr
     { (fun m -> mksyn_trifix m ".. .." $1 $3 $5) }

monadicExprNonEmptyNonInitial:
  | monadicExprNonEmptyInitial { $1 }
/*
  | LPAREN monadicExprNonEmptyNonInitial RPAREN %prec decl_let  { $2 }
  | BEGIN monadicExprNonEmptyNonInitial END %prec decl_let  { $2 }
*/

monadicExprNonEmptyNonInitialBlock:
  | OBLOCKBEGIN monadicExprNonEmptyNonInitial OBLOCKEND { $2 }
  | monadicExprNonEmptyNonInitial { $1 }

monadicExprNonEmptyNonInitialBlockR:
  | monadicExprNonEmptyNonInitial ORIGHT_BLOCK_END { $1 }
  | monadicExprNonEmptyNonInitial { $1 }

monadicExprNonEmptyInitial:

  | monadicExprNonEmptyInitial OBLOCKSEP monadicExprNonEmptyInitial
     { Comp_sum($1,$3,rhs 1, rhs 3) }

  | FOR forLoopBinder monadicSingleLineQualifiersThenArrowThenExprR %prec decl_let
     { let a,b= $2 in Comp_for(true,a,b,$3 (rhs 2)) }
  | FOR forLoopBinder OBLOCKSEP monadicExprNonEmptyNonInitial %prec decl_let
     { let a,b= $2 in Comp_for(true,a,b,$4) }
  | FOR forLoopBinder do_or_odo monadicExprNonEmptyNonInitialBlock done_term %prec decl_let
     { let a,b= $2 in Comp_for(false,a,b,$4) }
  | FOR forLoopRange do_or_odo  monadicExprNonEmptyNonInitialBlock done_term
      { report_parse_error_at (rhs2  3 6) ("Integer loops 'for x = a to b do ...' may not be used in computation expressions. Consider using 'for x in n .. m do' instead"); $4 }

  | monadicWhenCondition monadicSingleLineQualifiersThenArrowThenExprR %prec decl_let
     { Comp_cond(true,$1,$2 (rhs 1),Comp_zero) }
  | monadicWhenCondition OBLOCKSEP monadicExprNonEmptyNonInitial %prec decl_let
     { Comp_cond(true,$1,$3,Comp_zero) }
  | monadicWhenCondition do_or_odo monadicExprNonEmptyNonInitialBlock done_term %prec decl_let
     { Comp_cond(true,$1,$3,Comp_zero) }

  | IF declExpr monadicIfExprCases %prec expr_if
     { $3 $2 }
  | WHILE declExpr do_or_odo monadicExprNonEmptyNonInitialBlock done_term
     { Comp_while($2,$4) }

  | TRY monadicExprNonEmptyNonInitialBlockR  monadicWithClauses %prec expr_try
     { Comp_try_with($2, $3) }

  | TRY monadicExprNonEmptyNonInitialBlockR FINALLY typedSeqExprBlock       %prec expr_try
     { Comp_try_finally($2,$4) }

  | MATCH typedSeqExpr       monadicWithClauses      %prec expr_match
     { Comp_match( $2,$3) }

  | monadicExprBindings
     { $1 }

  | YIELD declExpr
     { Comp_yield(($1,not $1),$2) }
  | YIELD_BANG declExpr
     { Comp_yieldm(($1,not $1), $2) }

  | RARROW typedSeqExprBlockR
     { Comp_yield((true,true),$2) }

  /* LEGACY - always give a warning */
  | RARROW2 typedSeqExprBlockR
     { Comp_yieldm((false,false),$2) }
   /* Experimental */
/*
  | THEN declExpr SEMICOLON monadicExprNonEmptyNonInitial %prec decl_let { $4  }
  | THEN declExpr %prec decl_let { Comp_zero  }
  | OTHEN OBLOCKBEGIN declExpr OBLOCKEND OBLOCKSEP monadicExprNonEmptyNonInitial %prec decl_let { $6 }
  | OTHEN OBLOCKBEGIN declExpr OBLOCKEND %prec decl_let { Comp_zero }
*/

monadicWithClauses:
  | WITH monadicWithPatternClauses      %prec expr_match
     { $2 }
  | OWITH monadicWithPatternClauses OEND %prec expr_match
     { $2 }

monadicExprBindings:
  | hardwhiteLetBindings OBLOCKSEP monadicExprNonEmptyNonInitial %prec decl_let
     { mkComprehensionBindings (rhs 1) $1 $3 }
  | hardwhiteLetBindings monadicExprNonEmptyNonInitial           %prec decl_let
     { mkComprehensionBindings (rhs 1) $1 $2 }
  | hardwhiteDoBinding OBLOCKSEP monadicExpr          %prec decl_let
     { mkComprehensionBindings (rhs 1) $1 $3 }
  | hardwhiteDoBinding monadicExpr                    %prec decl_let
     { mkComprehensionBindings (rhs 1) $1 $2 }

  | defnBindings IN monadicExprNonEmptyNonInitial %prec decl_let
     { mkComprehensionBindings (rhs 1) $1 $3 }
  | doBinding IN monadicExprNonEmptyNonInitial %prec decl_let
     { mkComprehensionBindings (rhs 1) $1 $3 }
  | BINDER headBindingPattern EQUALS typedSeqExprBlock IN opt_OBLOCKSEP monadicExprNonEmptyNonInitial %prec decl_let
     { let m = rhs 1 in
       if $1 <> "let" && $1 <> "use" then warning(Error("the use of custom binders in computation expressions is experimental and will be deleted in the next release",m));
       Comp_bind(false,Some $1,Some $2,$4,$7) }

  | OBINDER headBindingPattern EQUALS typedSeqExprBlock hardwhiteDefnBindingsTerminator opt_OBLOCKSEP monadicExprNonEmptyNonInitial %prec decl_let
     { let m = rhs 1 in
       if $1 <> "let" && $1 <> "use" then warning(Error("the use of custom binders in computation expressions is experimental and will be deleted in the next release",m));
       Comp_bind(false,Some $1,Some $2,$4,$7) }

  | DO_BANG typedSeqExpr IN opt_OBLOCKSEP monadicExprNonEmptyNonInitial %prec decl_let
     { Comp_bind(false,Some "let",None,$2,$5) }

  | ODO_BANG typedSeqExprBlock hardwhiteDefnBindingsTerminator opt_OBLOCKSEP monadicExpr  %prec decl_let
     { Comp_bind(false,Some "let",None,$2,$5) }

monadicExpr:
  | monadicExprNonEmptyNonInitial { $1 }
  | { Comp_zero }
  | error { (* silent recovery *) Comp_zero }

monadicWhenCondition:
  | WHEN declExpr
     { $2 }


monadicWithPatternClauses:
  | monadicPatternClauses { $1 }
  | BAR monadicPatternClauses {  $2 }

monadicPatternClauses:
  | patternAndGuard monadicPatternResult %prec prec_pat_pat_action
     { let pat,guard,patm = $1 in [CompClause(pat,guard,$2,patm)]  }
  | patternAndGuard monadicPatternResult BAR monadicPatternClauses
     { let pat,guard,patm = $1 in CompClause(pat,guard,$2,patm) :: $4 }

monadicPatternResult:
  | RARROW monadicExprNonEmptyNonInitialBlockR
     { $2 }



/*
monadicExprNonEmptyR:
  | monadicExprNonEmptyNonInitial ORIGHT_BLOCK_END { $1 }
  | monadicExprNonEmptyNonInitial { $1 }
*/

/* Allow a naked yield (no "yield" or "return" or "->") immediately after a "->" */
/* Allow a naked yield (no "yield!" or "return!" or "->>") immediately after a "->>" */
/* In both cases multiple 'for' and 'when' bindings can precede */
monadicSingleLineQualifiersThenArrowThenExprR:
  | RARROW typedSeqExprBlockR { (fun m -> Comp_yield((true,false),$2)) }
/*  | RARROW monadicExprNonEmptyR { (fun m -> $2) } */
  | RARROW2 typedSeqExprBlockR { (fun m -> Comp_yieldm((true,false),$2)) }

  | FOR forLoopBinder monadicSingleLineQualifiersThenArrowThenExprR %prec decl_let
     { let a2,b2= $2 in
       (fun m ->
           Comp_for(true,a2,b2,$3 m)) }

  | monadicWhenCondition monadicSingleLineQualifiersThenArrowThenExprR %prec decl_let
     { (fun m ->
           Comp_cond(true,$1,$2 m,Comp_zero)) }

  /* LEGACY WARNING RULE: nested 'for' loops */
  | FOR forLoopBinder OBLOCKSEP monadicSingleLineQualifiersThenArrowThenExprR %prec decl_let
     { let a2,b2= $2 in
       (fun m ->
           warning(Error("'for' binders and 'when' clauses in computation expressions should be either stacked (each on a separate line starting at the same column), or each should have a matching 'do', e.g. 'for x in <expr> do for y in <expr> -> ...'",m));
           Comp_for(true,a2,b2,$4 m)) }

  /* LEGACY WARNING RULE: nested 'when' clauses */
  | monadicWhenCondition OBLOCKSEP monadicSingleLineQualifiersThenArrowThenExprR %prec decl_let
     { (fun m ->
           warning(Error("'for' binders and 'when' clauses in computation expressions should either be stacked (each on a separate line starting at the same column), or each should have a matching '->' or 'do', e.g. 'for x in <expr> do when <expr> -> ...'",m));
           Comp_cond(true,$1,$3 m,Comp_zero)) }


monadicIfExprThen:
  | THEN  monadicExprNonEmptyNonInitial %prec prec_then_if { $2 }
  | OTHEN  OBLOCKBEGIN monadicExprNonEmptyNonInitial OBLOCKEND %prec prec_then_if { $3 }

monadicIfExprElifs:
  |
      { Comp_zero }
  | ELSE monadicExprNonEmptyNonInitial
      { $2 }
  | OELSE  OBLOCKBEGIN monadicExprNonEmptyNonInitial OBLOCKEND
      { $3 }
  | ELIF declExpr monadicIfExprCases
      { $3 $2  }

monadicIfExprCases:
  | monadicIfExprThen monadicIfExprElifs { (fun g -> Comp_cond(false,g,$1,$2)) }

forLoopBinder:
  | parenPattern IN declExpr
     { ($1, $3) }
  | parenPattern IN rangeSequenceExpr
     { ($1, $3 (rhs 3)) }
  | parenPattern IN recover
     { ($1, arbExpr()) }

forLoopRange:
  | parenPattern EQUALS declExpr  direction  declExpr { id_of_pat (rhs 1) $1,$3,$4,$5 }

inlineAssemblyExpr:
  |  HASH STRING opt_inlineAssemblyTypeArg opt_curriedArgExprs  opt_inlineAssemblyReturnTypes opt_HASH
      { libraryOnly (lhs());
        let s,sm = $2,rhs 2 in
        (fun m -> Expr_asm (parse_il_instrs s sm,$3,List.rev $4,$5,m)) }

opt_curriedArgExprs:
  | opt_curriedArgExprs argExprNoHPA  %prec expr_args { $2 :: $1 }
  |  { [] }

opt_argExprAfterType:
  |  { None }
  |  argExprAfterType { Some($1) }

opt_inlineAssemblyTypeArg:
  |  { [] }
  | TYPE LPAREN typ RPAREN  {  matchPair 2 4; [$3] }

opt_inlineAssemblyReturnTypes:
  |
     { [] }
  | COLON typ
     { [$2] }
  | COLON LPAREN RPAREN
     {  matchPair 2 3; [] }

recdExpr:
  |
     { (None,None, []) }
  | INHERIT appType opt_HIGH_PRECEDENCE_APP opt_argExprAfterType recdExprBindings opt_seps
     { let arg = match $4 with None -> mksyn_unit (lhs()) | Some e -> e in
       (Some($2,arg,rhs2 2 4),None, $5) }
/* REVIEW: we shouldn't really be permitting appExpr here if we want to minimize the number of "entries" into the expression */
/* syntax. OCaml only permits argExpr here */
  | appExpr EQUALS declExpr recdExprBindings opt_seps
     { match $1 with
       | Expr_lid_get(false,v,m) -> (None,None, (frontAndBack v,$3) :: List.rev $4)
       | _ -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 2) "field bindings must have the form 'id = expr;'" }
  | appExpr WITH path EQUALS  declExpr recdExprBindings  opt_seps
     {  (None,Some $1,(frontAndBack $3,$5):: List.rev $6) }
  | appExpr OWITH path EQUALS  declExpr recdExprBindings  opt_seps OEND
     {  (None,Some $1,(frontAndBack $3,$5):: List.rev $6) }

recdExprBindings:
  | recdExprBindings seps path EQUALS declExpr { (frontAndBack $3,$5) :: $1 }
  |                                            { [] }

objExpr:
  | objExprBaseCall opt_objExprBindings opt_OBLOCKSEP objExprInterfaces
     { (fun m -> let (a,b) = $1 in Expr_impl(a,b,$2,$4, m)) }

objExprBaseCall:
  | NEW appType opt_HIGH_PRECEDENCE_APP opt_argExprAfterType opt_as
     { startName(rhs 5);
       let argInfo = match $4 with None -> None | Some e -> Some(e,$5) in
       ($2, argInfo) }

opt_objExprBindings:
  | objExprBindings { $1 }
  |                 { [] }

objExprBindings:
  | WITH localBindings { ($2 [] None) }
  | OWITH localBindings OEND { ($2 [] None) }
  | WITH objectImplementationBlock opt_decl_end
      { $2 |>
        (chooseList (function ClassMemberDefn_member_binding(b,m) -> Some b
                          | ClassMemberDefn_implicit_inherit (_, _, _, m)
                          | ClassMemberDefn_implicit_ctor (_, _, m)
                          | ClassMemberDefn_let_bindings(_,_,_,m)
                          | ClassMemberDefn_slotsig(_,_,m)
                          | ClassMemberDefn_interface(_,_,m)
                          | ClassMemberDefn_inherit(_,_,m)
                          | ClassMemberDefn_field(_,m)
                          | ClassMemberDefn_open(_,m)
                          | ClassMemberDefn_tycon(_,_,m) -> errorR(Error("This member is not permitted in an object implementation",m)); None)) }

objExprInterfaces:
  | %prec prec_interfaces_prefix { [] }
  | objExprInterface objExprInterfaces { $1 :: $2 }
  | error objExprInterfaces { (* silent recovery *) $2 }

objExprInterface:
  |  interfaceMember appType opt_objExprBindings opt_decl_end opt_OBLOCKSEP
    { InterfaceImpl($2, $3, lhs()) }

direction:
  | TO     { true }
  | DOWNTO { false }


anonLambdaExpr:
  | FUN atomicPatterns RARROW typedSeqExprBlock
     { mksyn_match_lambdas false (lhs()) $2 $4 }
  | FUN atomicPatterns RARROW error
     { mksyn_match_lambdas false (lhs()) $2 (arbExpr()) }
  | OFUN atomicPatterns RARROW typedSeqExprBlockR OEND
     { mksyn_match_lambdas false (rhs2 1 4) $2 $4 }
  | OFUN atomicPatterns RARROW error OEND
     { mksyn_match_lambdas false (rhs2 1 3) $2 (arbExpr()) }

anonMatchingExpr:
  | FUNCTION opt_bar patternClauses  %prec expr_function
      { mksyn_match_lambda false false (lhs()) $3 }
  | OFUNCTION opt_bar patternClauses  OEND %prec expr_function
      { mksyn_match_lambda false false (lhs()) $3 }
  | OFUNCTION opt_bar patternClauses  error OEND %prec expr_function
      { report_parse_error_at (rhs 1) "error in 'function' block";
        mksyn_match_lambda false false (lhs()) $3 }

/*--------------------------------------------------------------------------*/
/* TYPE ALGEBRA                                                             */

typeWithTypeConstraints:
  | typ %prec prec_wheretyp_prefix { $1 }
  | typ WHEN typeConstraints
     { Type_with_global_constraints($1, List.rev $3,lhs()) }

topTypeWithTypeConstraints:
  | topType
     { $1 }
  | topType WHEN typeConstraints
     { let ty,arity = $1 in
        (* nb. it doesn't matter where the constraints go in the structure of the type. *)
        Type_with_global_constraints(ty,List.rev $3,lhs()), arity }

opt_topReturnTypeWithTypeConstraints:
  |
     { None }
  | COLON topTypeWithTypeConstraints
     { let ty,arity = $2 in
       let arity = (match arity with TopValSynData([],rmdata)-> rmdata | _ -> SynArgInfo.unnamedRetVal) in
       Some ((ty,arity),rhs 2) }

topType:
  | topTupleType RARROW topType
     { let dty,dmdata= $1 in
       let rty,(TopValSynData(dmdatas,rmdata)) = $3 in
       Type_fun(dty,rty,lhs()), (TopValSynData(dmdata::dmdatas, rmdata)) }
  | topTupleType
     { let ty,rmdata = $1 in ty, (TopValSynData([],(match rmdata with [md] -> md | _ -> SynArgInfo.unnamedRetVal))) }

topTupleType:
  | topAppType STAR topTupleTypeElements
     { let ty,mdata = $1 in let tys,mdatas = List.split $3 in (Type_tuple(ty ::tys, lhs())),(mdata :: mdatas) }
  | topAppType
     { let ty,mdata = $1 in ty,[mdata] }

topTupleTypeElements:
  | topAppType STAR topTupleTypeElements       { $1 :: $3 }
  | topAppType %prec prec_toptuptyptail_prefix { [$1] }

/* TODO: why can't we use opt_attributes here? */
topAppType:
  | attributes appType COLON appType
     { match $2 with
       | Type_app([id],_,_) -> $4,TopArgSynData($1,false,Some id)
       | _ -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 2) "syntax error in labelled type argument"  }
  | attributes QMARK ident COLON appType
     { $5,TopArgSynData($1,true,Some $3) }
  | attributes appType
     { ($2,TopArgSynData($1,false,None)) }
  | appType COLON appType
     { match $1 with
       | Type_app([id],_,_) -> $3,TopArgSynData([],false,Some id)
       | _ -> raise_parse_error_at_USE_ONLY_IF_NOT_IN_ERROR_RECOVERY (rhs 2) "syntax error in labelled type argument"  }
  | QMARK ident COLON appType
     { $4,TopArgSynData([],true,Some $2) }
  | appType
     { $1,TopArgSynData([],false,None) }

polyType:
  | typar DOT typ { Type_forall(TyparDecl([],$1),$3,lhs()) }
  | typ { $1 }

typ:
  | tupleType RARROW typ  { Type_fun($1,$3,lhs()) }
  | tupleType %prec prec_typ_prefix { $1 }

tupleType:
  | appType STAR tupleTypeElements { Type_tuple($1 :: $3,lhs()) }
  | appType %prec prec_tuptyp_prefix { $1 }

tupleTypeElements:
  | appType STAR tupleTypeElements              { $1 :: $3 }
  | appType %prec prec_tuptyptail_prefix { [$1] }

appType:
  | appType arrayTypeSuffix
      {  Type_arr($2,$1,lhs()) }
  | appType HIGH_PRECEDENCE_APP arrayTypeSuffix
      {  Type_arr($3,$1,lhs()) }
  | appType path
      { Type_app($2,[$1],lhs()) }
  | appType LAZY
      { mksyn_lazy_ty (lhs()) $1 }
  | LPAREN appTypePrexifArguments RPAREN  path
      {  matchPair 1 3; Type_app($4,$2, lhs()) }
  | atomType
      { $1 }
  | typar      COLON_GREATER typ
      {  let tp,typ = $1,$3 in
         let m = lhs() in
         Type_with_global_constraints(Type_var (tp, rhs 1), [WhereTyparSubtypeOfType(tp,typ,m)],m)  }
  | UNDERSCORE COLON_GREATER typ %prec COLON_GREATER
      {  matchPair 1 3; mksyn_anon_constraint $3 (lhs()) }

arrayTypeSuffix:
  | LBRACK RBRACK
      {  matchPair 1 2; 1 }
  | LBRACK COMMA RBRACK
      {  matchPair 1 3; 2 }
  | LBRACK COMMA COMMA RBRACK
      {  matchPair 1 4; 3 }
  | LBRACK COMMA COMMA COMMA RBRACK
      {  matchPair 1 5; 4 }

appTypePrexifArguments:
  | typ COMMA typ typeListElements { $1 :: $3 :: List.rev $4 }

typeListElements:
  | typeListElements COMMA typ { $3 :: $1 }
  |                      { [] }

atomType:
  | HASH atomType
     { mksyn_anon_constraint $2 (lhs()) }
  | typar
     { Type_var ($1,lhs()) }
  | UNDERSCORE
     { Type_anon (lhs()) }
  | LPAREN typ RPAREN
     {  matchPair 1 3; $2 }
  | LPAREN typ recover
     { report_parse_error_at (rhs 1) "unmatched '('" ; $2 }
  | LPAREN error RPAREN
     { (* silent recovery *) Type_anon (lhs()) }
  | path %prec prec_atomtyp_path
     { Type_app($1,[],lhs()) }
  | path typeArgs %prec prec_atomtyp_path
     { Type_app($1,$2,lhs()) }
  | atomType DOT path %prec prec_atomtyp_get_path
     { Type_proj_then_app($1,$3,[],lhs()) }
  | atomType DOT path typeArgs %prec prec_atomtyp_get_path
     { Type_proj_then_app($1,$3,$4,lhs()) }


typeArgs:
  | typeArgsActual
     { $1 }
  | HIGH_PRECEDENCE_TYAPP typeArgsActual
     { $2 }

typeArgsActual:
  | LESS GREATER
     { [] }
  | LESS typ GREATER
     { [$2] }
  | LESS typ COMMA typ typeListElements GREATER
     { $2 :: $4 :: List.rev $5 }

typar:
  | QUOTE ident
     {  let id = mksyn_id (lhs()) ($2).idText in
        Typar(id ,NoStaticReq,false) }
  | DOLLAR ident
     {  libraryOnly (lhs());
        let id = mksyn_id (lhs()) ($2).idText in
        Typar(id,CompleteStaticReq,false) }
  | staticallyKnownHeadTypar
     { $1 }

staticallyKnownHeadTypar:
  | INFIX_AT_HAT_OP ident
    {  if $1 <> "^" then report_parse_error_at (rhs 1) "syntax error: unexpeced type paramter specification";
       Typar($2,HeadTypeStaticReq,false) }



ident:
  | IDENT
     { ident($1,rhs 1) }

path:
  | ident
     { startName(rhs 1); [$1] }
  | path DOT ident
     { qualifyNameIfAlongside(rhs 2) (rhs 3); (* silent recovery *) $1 @ [$3] }
  | path DOT error
     { qualifyName(rhs 2) (rhs 2); (* silent recovery *) $1  }

identop:
  | ident
     { $1 }
  | LPAREN operatorName RPAREN
     {  matchPair 1 3; ident(compileOpName $2,rhs 2) }
/* active pattern value names */
  | LPAREN barNames BAR RPAREN { ident(("|"^String.concat "|" (List.rev $2) ^ "|"),rhs2 2 4) }
  | LPAREN barNames BAR UNDERSCORE BAR RPAREN { ident(("|"^String.concat "|" (List.rev $2) ^ "|_|" ),rhs2 2 5) }

operatorName:
  | PREFIX_OP { $1 }
  | INFIX_STAR_STAR_OP  { $1 }
  | INFIX_COMPARE_OP { $1 }
  | INFIX_AT_HAT_OP  { $1 }
  | INFIX_BAR_OP  { $1 }
  | INFIX_AMP_OP { $1 }
  | PLUS_MINUS_OP  { $1 }
  | INFIX_STAR_DIV_MOD_OP { $1 }
  | DOLLAR { "$" }
  | ADJACENT_PREFIX_PLUS_MINUS_OP { $1 }
  | MINUS { "-" }
  | STAR { "*" }
  | EQUALS { "=" }
  | OR { ocamlCompat "The 'or' operator may not be re-defined unless OCaml compatibility is enabled" (lhs()); "or" }
  | LESS { "<" }
  | GREATER { ">" }
  | AMP { ocamlCompat "The '&' operator may not be re-defined unless OCaml compatibility is enabled" (lhs()); "&" }
  | AMP_AMP { ocamlCompat "The '&&' operator may not be re-defined unless OCaml compatibility is enabled" (lhs()); "&&" }
  | BAR_BAR { ocamlCompat "The '||' operator may not be re-defined unless OCaml compatibility is enabled" (lhs()); "||" }
  | COLON_EQUALS { ":=" }
  | FUNKY_OPERATOR_NAME { deprecated_op (lhs()); $1 }
  | DOT IDENT { deprecated_op (lhs()); "."^ $2 }
  | SPLICE_SYMBOL { $1 }
  | PERCENT_OP { $1 }
  | DOT_DOT { (* deprecated_op (lhs()); *) ".." }
  | DOT_DOT DOT_DOT { (* deprecated_op (lhs()); *) ".. .." }
  | LQUOTE RQUOTE
      { if $1 <> $2 then report_parse_error_at (rhs 1) ("mismatched quotation operator name, beginning with '"^fst $1^"'");
        fst $1 }

barNames:
  | BAR IDENT { [$2] }
  | barNames BAR IDENT { $3 :: $1 }

pathop:
  | identop
     { startName(rhs 1); [$1] }
  | path DOT identop
     { qualifyNameIfAlongside(rhs 2) (rhs 3); $1 @ [$3] }
  | path DOT error
     { qualifyName(rhs 2) (rhs 2); (* silent recovery *) $1 }

nameop:
  | identop  { startName(rhs 1); $1 }

top_sep:
  | SEMICOLON { }
  | SEMICOLON_SEMICOLON { }
  | OBLOCKSEP { }

top_seps:
  | top_sep                     { }
  | top_sep top_seps { }

itop_sep:
  | SEMICOLON { }
  | OBLOCKSEP { }

itop_seps:
  | itop_sep                     { }
  | itop_sep itop_seps { }

opt_itop_seps:
  | itop_sep opt_itop_seps { }
  |                        { }

opt_top_seps:
  | top_sep opt_top_seps { }
  |                      { }

seps:
  | OBLOCKSEP { }
  | SEMICOLON { }
  | OBLOCKSEP SEMICOLON { }
  | SEMICOLON OBLOCKSEP { }

/* An 'end' that's optional only in #light, where an ODECLEND gets inserted, and explicit 'end's get converted to OEND */
decl_end:
  | ODECLEND
      { }
  | OEND
      { (* report_parse_warning_at (rhs 2) "this 'end' token is not needed in #light syntax and should  be omitted. A future release of the language may require this";  *)  }
  | END
      {}

/* An 'end' that's optional in both #light and #heavy */
opt_decl_end:
  | ODECLEND
      {}
  | OEND
      { (* report_parse_warning_at (rhs 2) "this 'end' token is not needed in #light syntax and should be omitted. A future release of the language may require this";   *)  }
  | END
      {}
  |
      {}

opt_ODECLEND:
  | ODECLEND { }
  |          { }

deprecated_opt_equals:
  | EQUALS    { deprecated "No '=' symbol should follow a 'namespace' declaration" (lhs()) }
  |           {  }

opt_OBLOCKSEP:
  | OBLOCKSEP { }
  |          { }

opt_seps:
  | seps { }
  |      { }

opt_rec:
  | REC { true }
  |     { false }

opt_bar:
  | BAR { }
  |     { }

opt_inline:
  | INLINE { true }
  |        { false }

opt_mutable:
  | MUTABLE { true }
  |         { false }

do_or_odo:
  | DO  { }
  | ODO { }

done_term:
  | DONE { }
  | ODECLEND { }  /* DONE gets thrown away by the lexfilter in favour of ODECLEND */

structOrBegin:
  | STRUCT { }
  | BEGIN { }

sigOrBegin:
  | SIG { }
  | BEGIN { }

colonOrEquals:
  | COLON { }
  | EQUALS { }

opt_HASH:
  | HASH {}
  |      { report_parse_warning "this expression form is deprecated - use balanced (# ... #) instead"  }

opt_HIGH_PRECEDENCE_APP:
  | HIGH_PRECEDENCE_APP { }
  |    { }

opt_HIGH_PRECEDENCE_TYAPP:
  | HIGH_PRECEDENCE_TYAPP { }
  |    { }
