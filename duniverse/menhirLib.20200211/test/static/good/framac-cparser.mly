/****************************************************************************/
/*                                                                          */
/*  Copyright (C) 2001-2003                                                 */
/*   George C. Necula    <necula@cs.berkeley.edu>                           */
/*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          */
/*   Wes Weimer          <weimer@cs.berkeley.edu>                           */
/*   Ben Liblit          <liblit@cs.berkeley.edu>                           */
/*  All rights reserved.                                                    */
/*                                                                          */
/*  Redistribution and use in source and binary forms, with or without      */
/*  modification, are permitted provided that the following conditions      */
/*  are met:                                                                */
/*                                                                          */
/*  1. Redistributions of source code must retain the above copyright       */
/*  notice, this list of conditions and the following disclaimer.           */
/*                                                                          */
/*  2. Redistributions in binary form must reproduce the above copyright    */
/*  notice, this list of conditions and the following disclaimer in the     */
/*  documentation and/or other materials provided with the distribution.    */
/*                                                                          */
/*  3. The names of the contributors may not be used to endorse or          */
/*  promote products derived from this software without specific prior      */
/*  written permission.                                                     */
/*                                                                          */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     */
/*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       */
/*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       */
/*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          */
/*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     */
/*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    */
/*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        */
/*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      */
/*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       */
/*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         */
/*  POSSIBILITY OF SUCH DAMAGE.                                             */
/*                                                                          */
/*  File modified by CEA (Commissariat à l'énergie atomique et aux          */
/*                        énergies alternatives)                            */
/*               and INRIA (Institut National de Recherche en Informatique  */
/*                          et Automatique).                                */
/****************************************************************************/

/*  3.22.99 Hugues Cass<E9> First version.
    2.0  George Necula 12/12/00: Practically complete rewrite.
*/


%{

open Cabs
open Cabshelper
let parse_error msg = Errorloc.parse_error msg

(*
** Expression building
*)
let smooth_expression lst =
  match lst with
    [] -> { expr_loc = cabslu; expr_node = NOTHING }
  | [expr] -> expr
  | _ ->
      let beg_loc = fst (List.hd lst).expr_loc in
      let end_loc = snd (Extlib.last lst).expr_loc in
      { expr_loc = (beg_loc,end_loc); expr_node = COMMA (lst) }

(* To be called only inside a grammar rule. *)
let make_expr e =
  { expr_loc = symbol_start_pos (), symbol_end_pos ();
    expr_node = e }

let currentFunctionName = ref "<outside any function>"

(* Go through all the parameter names and mark them as identifiers *)
let rec findProto = function
    PROTO (d, args, _) when isJUSTBASE d ->
      List.iter (fun (_, (an, _, _, _)) -> !Lexerhack.add_identifier an) args
  | PROTO (d, _, _) -> findProto d
  | PARENTYPE (_, d, _) -> findProto d
  | PTR (_, d) -> findProto d
  | ARRAY (d, _, _) -> findProto d
  | _ -> raise Parsing.Parse_error

and isJUSTBASE = function
    JUSTBASE -> true
  | PARENTYPE (_, d, _) -> isJUSTBASE d
  | _ -> false

let announceFunctionName ((n, decl, _, _):name) =
  !Lexerhack.add_identifier n;
  (* Start a context that includes the parameter names and the whole body.
   * Will pop when we finish parsing the function body *)
  !Lexerhack.push_context ();
  (try findProto decl
   with Parsing.Parse_error ->
     parse_error "Cannot find the prototype in a function definition");
  currentFunctionName := n

let check_funspec_abrupt_clauses fname (spec,_) =
  List.iter
    (fun bhv ->
      List.iter
	(function
	| (Cil_types.Normal | Cil_types.Exits),_ -> ()
	| (Cil_types.Breaks | Cil_types.Continues |
            Cil_types.Returns), {Logic_ptree.lexpr_loc = (loc,_)} ->
          Kernel.error ~source:loc
            "Specification of function %s can only contain ensures or \
                 exits post-conditions" fname;
          raise Parsing.Parse_error)
	bhv.Cil_types.b_post_cond)
    spec.Cil_types.spec_behavior

let applyPointer (ptspecs: attribute list list) (dt: decl_type)
       : decl_type =
  (* Outer specification first *)
  let rec loop = function
      [] -> dt
    | attrs :: rest -> PTR(attrs, loop rest)
  in
  loop ptspecs

let doDeclaration logic_spec (loc: cabsloc) (specs: spec_elem list) (nl: init_name list)  =
  if isTypedef specs then begin
    (* Tell the lexer about the new type names *)
    List.iter (fun ((n, _, _, _), _) -> !Lexerhack.add_type n) nl;
    TYPEDEF ((specs, List.map (fun (n, _) -> n) nl), loc)
  end else
    if nl = [] then
      ONLYTYPEDEF (specs, loc)
    else begin
      (* Tell the lexer about the new variable names *)
      List.iter (fun ((n, _, _, _), _) -> !Lexerhack.add_identifier n) nl;
      !Lexerhack.push_context ();
      List.iter
        (fun ((_,t,_,_),_) ->
           try findProto t with Parsing.Parse_error -> ())
        nl;
      let logic_spec =
        match logic_spec with
            None -> None
          | Some ls ->
	      Cabshelper.continue_annot loc
		(fun () ->
                   let (_,spec) = Logic_lexer.spec ls in
                   let name =
                     match nl with
                         [ (n,_,_,_),_ ] -> n
                       | _ -> "unknown function"
                   in
                   check_funspec_abrupt_clauses name spec;
                   Some spec)
		(fun () -> None)
		"Skipping annotation"
      in
      !Lexerhack.pop_context ();
      DECDEF (logic_spec, (specs, nl), loc)
    end


let doFunctionDef spec (loc: cabsloc)
                  (lend: cabsloc)
                  (specs: spec_elem list)
                  (n: name)
                  (b: block) : definition =
  let fname = (specs, n) in
  let name = match n with (n,_,_,_) -> n in
  Extlib.may_map ~dft:() (check_funspec_abrupt_clauses name) spec;
  FUNDEF (spec, fname, b, loc, lend)

let doOldParDecl (names: string list)
                 ((pardefs: name_group list), (isva: bool))
    : single_name list * bool =
  let findOneName n =
    (* Search in pardefs for the definition for this parameter *)
    let rec loopGroups = function
        [] -> ([SpecType Tint], (n, JUSTBASE, [], cabslu))
      | (specs, names) :: restgroups ->
          let rec loopNames = function
              [] -> loopGroups restgroups
            | ((n',_, _, _) as sn) :: _ when n' = n -> (specs, sn)
            | _ :: restnames -> loopNames restnames
          in
          loopNames names
    in
    loopGroups pardefs
  in
  let args = List.map findOneName names in
  (args, isva)

let int64_to_char value =
  if (Int64.compare value (Int64.of_int 255) > 0) ||
    (Int64.compare value Int64.zero < 0) then
    parse_error (Printf.sprintf "integral literal 0x%Lx too big" value)
  else
    Char.chr (Int64.to_int value)

(* takes a not-nul-terminated list, and converts it to a string. *)
let rec intlist_to_string (str: int64 list):string =
  match str with
    [] -> ""  (* add nul-termination *)
  | value::rest ->
      let this_char = int64_to_char value in
      (String.make 1 this_char) ^ (intlist_to_string rest)

let fst3 (result, _, _) = result
let trd3 (_, _, result) = result

let fourth4 (_,_,_,result) = result

(*
   transform:  __builtin_offsetof(type, member)
   into     :  (size_t) (&(type * ) 0)->member
 *)

let sizeofType () =
  let findSpecifier name =
    let convert_one_specifier s =
      if s = "int" then Cabs.Tint
      else if s = "unsigned" then Cabs.Tunsigned
      else if s = "long" then Cabs.Tlong
      else if s = "short" then Cabs.Tshort
      else if s = "char" then Cabs.Tchar
      else
        Kernel.fatal
          ~current:true
          "initCIL: cannot find the right specifier for type %s" name
    in
    let add_one_specifier s acc =
      (Cabs.SpecType (convert_one_specifier s)) :: acc
    in
    let specs = Str.split (Str.regexp " +") name in
    List.fold_right add_one_specifier specs []
  in
  findSpecifier Cil.theMachine.Cil.theMachine.Cil_types.size_t


let transformOffsetOf (speclist, dtype) member =
  let mk_expr e = { expr_loc = member.expr_loc; expr_node = e } in
  let rec addPointer = function
    | JUSTBASE ->
	PTR([], JUSTBASE)
    | PARENTYPE (attrs1, dtype, attrs2) ->
	PARENTYPE (attrs1, addPointer dtype, attrs2)
    | ARRAY (dtype, attrs, expr) ->
	ARRAY (addPointer dtype, attrs, expr)
    | PTR (attrs, dtype) ->
	PTR (attrs, addPointer dtype)
    | PROTO (dtype, names, variadic) ->
	PROTO (addPointer dtype, names, variadic)
  in
  let nullType = (speclist, addPointer dtype) in
  let nullExpr = mk_expr (CONSTANT (CONST_INT "0")) in
  let castExpr = mk_expr (CAST (nullType, SINGLE_INIT nullExpr)) in

  let rec replaceBase e =
    let node = match e.expr_node with
      | VARIABLE field ->
	  MEMBEROFPTR (castExpr, field)
      | MEMBEROF (base, field) ->
	  MEMBEROF (replaceBase base, field)
      | INDEX (base, index) ->
	  INDEX (replaceBase base, index)
      | _ ->
	  parse_error "malformed offset expression in __builtin_offsetof"
    in { e with expr_node = node }
  in
  let memberExpr = replaceBase member in
  let addrExpr = { memberExpr with expr_node = UNARY (ADDROF, memberExpr)} in
  let sizeofType = sizeofType(), JUSTBASE in
  { addrExpr with expr_node = CAST (sizeofType, SINGLE_INIT addrExpr)}

let no_ghost_stmt s = {stmt_ghost = false ; stmt_node = s}

let no_ghost = List.map no_ghost_stmt

let in_ghost =
  let ghost_me = object
    inherit Cabsvisit.nopCabsVisitor
    method! vstmt s =
      s.stmt_ghost <- true;
      Cil.DoChildren
  end
  in
  List.map
    (fun s -> ignore (Cabsvisit.visitCabsStatement ghost_me s); s)

let in_block l =
  match l with
      [] -> no_ghost_stmt (NOP cabslu)
    | [s] -> s
    | _::_ ->
        no_ghost_stmt (BLOCK ({ blabels = []; battrs = []; bstmts = l},
                              get_statementloc (List.hd l),
                              get_statementloc (Extlib.last l)))

%}

%token <Lexing.position * string> SPEC
%token <Logic_ptree.decl list> DECL
%token <Logic_ptree.code_annot * Cabs.cabsloc> CODE_ANNOT
%token <Logic_ptree.code_annot list * Cabs.cabsloc> LOOP_ANNOT
%token <string * Cabs.cabsloc> ATTRIBUTE_ANNOT
%token <Logic_ptree.custom_tree  * string * Cabs.cabsloc> CUSTOM_ANNOT

%token <string> IDENT
%token <int64 list * Cabs.cabsloc> CST_CHAR
%token <int64 list * Cabs.cabsloc> CST_WCHAR
%token <string * Cabs.cabsloc> CST_INT
%token <string * Cabs.cabsloc> CST_FLOAT
%token <string> NAMED_TYPE

/* Each character is its own list element, and the terminating nul is not
   included in this list. */
%token <int64 list * Cabs.cabsloc> CST_STRING
%token <int64 list * Cabs.cabsloc> CST_WSTRING

%token EOF
%token<Cabs.cabsloc> BOOL CHAR INT DOUBLE FLOAT VOID INT64 INT32
%token<Cabs.cabsloc> ENUM STRUCT TYPEDEF UNION
%token<Cabs.cabsloc> SIGNED UNSIGNED LONG SHORT
%token<Cabs.cabsloc> VOLATILE EXTERN STATIC CONST RESTRICT AUTO REGISTER
%token<Cabs.cabsloc> THREAD

%token<Cabs.cabsloc> SIZEOF ALIGNOF

%token EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ
%token AND_EQ PIPE_EQ CIRC_EQ INF_INF_EQ SUP_SUP_EQ
%token ARROW DOT

%token EQ_EQ EXCLAM_EQ INF SUP INF_EQ SUP_EQ
%token<Cabs.cabsloc> PLUS MINUS STAR
%token SLASH PERCENT
%token<Cabs.cabsloc> TILDE AND
%token PIPE CIRC
%token<Cabs.cabsloc> EXCLAM AND_AND
%token PIPE_PIPE
%token INF_INF SUP_SUP
%token<Cabs.cabsloc> PLUS_PLUS MINUS_MINUS

%token RPAREN
%token<Cabs.cabsloc> LPAREN RBRACE
%token<Cabs.cabsloc> LBRACE
%token LBRACKET RBRACKET
%token COLON
%token<Cabs.cabsloc> SEMICOLON
%token COMMA ELLIPSIS QUEST

%token<Cabs.cabsloc> BREAK CONTINUE GOTO RETURN
%token<Cabs.cabsloc> SWITCH CASE DEFAULT
%token<Cabs.cabsloc> WHILE DO FOR
%token<Cabs.cabsloc> IF TRY EXCEPT FINALLY
%token ELSE

%token<Cabs.cabsloc> ATTRIBUTE INLINE ASM TYPEOF FUNCTION__ PRETTY_FUNCTION__
%token LABEL__
%token<Cabs.cabsloc> BUILTIN_VA_ARG ATTRIBUTE_USED
%token BUILTIN_VA_LIST
%token BLOCKATTRIBUTE
%token<Cabs.cabsloc> BUILTIN_TYPES_COMPAT BUILTIN_OFFSETOF
%token<Cabs.cabsloc> DECLSPEC
%token<string * Cabs.cabsloc> MSASM MSATTR
%token<string * Cabs.cabsloc> PRAGMA_LINE
%token<Cabs.cabsloc> PRAGMA
%token PRAGMA_EOL

/*Frama-C: ghost bracketing */
%token LGHOST RGHOST

/* operator precedence */
%nonassoc 	IF
%nonassoc 	ELSE


%left	COMMA
%right	EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ
                AND_EQ PIPE_EQ CIRC_EQ INF_INF_EQ SUP_SUP_EQ
%right	QUEST COLON
%left	PIPE_PIPE
%left	AND_AND
%left	PIPE
%left 	CIRC
%left	AND
%left	EQ_EQ EXCLAM_EQ
%left	INF SUP INF_EQ SUP_EQ
%left	INF_INF SUP_SUP
%left	PLUS MINUS
%left	STAR SLASH PERCENT CONST RESTRICT VOLATILE
%right	EXCLAM TILDE PLUS_PLUS MINUS_MINUS CAST RPAREN ADDROF SIZEOF ALIGNOF
%left 	LBRACKET
%left	DOT ARROW LPAREN LBRACE
%right  NAMED_TYPE     /* We'll use this to handle redefinitions of
                        * NAMED_TYPE as variables */
%left   IDENT

/* Non-terminals informations */
%start interpret file
%type <(bool*Cabs.definition) list> file interpret globals

%type <Cabs.definition> global


%type <Cabs.attribute list> attributes attributes_with_asm asmattr
%type <Cabs.constant * cabsloc> constant
%type <string * cabsloc> string_constant
%type <Cabs.expression> expression
%type <Cabs.expression> opt_expression
%type <Cabs.init_expression> init_expression
%type <Cabs.expression list> comma_expression
%type <Cabs.expression list> paren_comma_expression
%type <Cabs.expression list> arguments
%type <Cabs.expression list> bracket_comma_expression
%type <int64 list Queue.t * cabsloc> string_list
%type <int64 list * cabsloc> wstring_list

%type <Cabs.initwhat * Cabs.init_expression> initializer_single
%type <(Cabs.initwhat * Cabs.init_expression) list> initializer_list
%type <Cabs.initwhat> init_designators init_designators_opt

%type <spec_elem list * cabsloc> decl_spec_list
%type <typeSpecifier * cabsloc> type_spec
%type <Cabs.field_group list> struct_decl_list


%type <Cabs.name> old_proto_decl
%type <Cabs.single_name> parameter_decl
%type <Cabs.enum_item> enumerator
%type <Cabs.enum_item list> enum_list
%type <Cabs.definition> declaration function_def
%type <cabsloc * spec_elem list * name> function_def_start
%type <Cabs.spec_elem list * Cabs.decl_type> type_name
%type <Cabs.block * cabsloc * cabsloc> block
%type <Cabs.statement list> block_element_list
%type <string list> local_labels local_label_names
%type <string list> old_parameter_list_ne

%type <Cabs.init_name> init_declarator
%type <Cabs.init_name list> init_declarator_list
%type <Cabs.name> declarator
%type <Cabs.name * expression option> field_decl
%type <(Cabs.name * expression option) list> field_decl_list
%type <string * Cabs.decl_type> direct_decl
%type <Cabs.decl_type> abs_direct_decl abs_direct_decl_opt
%type <Cabs.decl_type * Cabs.attribute list> abstract_decl

 /* (* Each element is a "* <type_quals_opt>". *) */
%type <attribute list list * cabsloc> pointer pointer_opt
%type <Cabs.cabsloc> location
%type <Cabs.spec_elem * cabsloc> cvspec
%%

interpret: file { $1 }

file: globals EOF			{$1}

globals:
  /* empty */                           { [] }
| global globals                        { (false,$1) :: $2 }
| LGHOST ghost_globals globals          { $2 @ $3 }
| SEMICOLON globals                     { $2 }
;

location:
   /* empty */                	{ currentLoc () }  %prec IDENT
;

/* Rules for global ghosts: TODO keep the ghost status! */
ghost_globals:
| declaration ghost_globals                           { (true,$1)::$2 }
| function_def ghost_globals                          { (true,$1)::$2 }
| RGHOST                                              { [] }
;

/*** Global Definition ***/
global:
| DECL             { GLOBANNOT $1 }
| CUSTOM_ANNOT     { let (x,y,z) = $1 in CUSTOM(x,y,z) }
| declaration      { $1 }
| function_def     { $1 }

/*(* Some C header files are shared with the C++ compiler and have linkage
   * specification *)*/
| EXTERN string_constant declaration
    { LINKAGE (fst $2, (*handleLoc*) (snd $2), [ $3 ]) }
| EXTERN string_constant LBRACE globals RBRACE
    { LINKAGE (fst $2, (*handleLoc*) (snd $2),
                 List.map
                   (fun (x,y) ->
                      if x then parse_error "invalid ghost in extern linkage specification"
		      else y)
                   $4)  }
| ASM LPAREN string_constant RPAREN SEMICOLON
                                        { GLOBASM (fst $3, (*handleLoc*) $1) }
| pragma                                { $1 }
/* (* Old-style function prototype. This should be somewhere else, like in
    * "declaration". For now we keep it at global scope only because in local
    * scope it looks too much like a function call  *) */
| IDENT LPAREN old_parameter_list_ne RPAREN old_pardef_list SEMICOLON
    {
      let loc = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1 in
      (* Convert pardecl to new style *)
      let pardecl, isva = doOldParDecl $3 $5 in
      (* Make the function declarator *)
      doDeclaration None loc []
        [(($1, PROTO(JUSTBASE, pardecl,isva),
           ["FC_OLDSTYLEPROTO",[]], loc), NO_INIT)]
    }
/* (* Old style function prototype, but without any arguments *) */
| IDENT LPAREN RPAREN  SEMICOLON
    { (* Make the function declarator *)
      let loc = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1 in
      doDeclaration None loc []
        [(($1, PROTO(JUSTBASE,[],false), [], loc), NO_INIT)]
    }
| location error SEMICOLON { PRAGMA (make_expr (VARIABLE "parse_error"), $1) }
;

id_or_typename_as_id:
    IDENT				{ $1 }
|   NAMED_TYPE				{ $1 }
;

id_or_typename:
    id_or_typename_as_id		{ $1 }
;

maybecomma:
   /* empty */                          { () }
|  COMMA                                { () }
;

/* *** Expressions *** */

primary_expression:                     /*(* 6.5.1. *)*/
|		IDENT { make_expr (VARIABLE $1) }
|        	constant { make_expr (CONSTANT (fst $1)) }
|		paren_comma_expression
		        { make_expr (PAREN (smooth_expression $1)) }
|		LPAREN block RPAREN { make_expr (GNU_BODY (fst3 $2)) }
;

postfix_expression:                     /*(* 6.5.2 *)*/
| primary_expression { $1 }
| postfix_expression bracket_comma_expression
      {make_expr (INDEX ($1, smooth_expression $2))}
| postfix_expression LPAREN arguments RPAREN {make_expr (CALL ($1, $3))}
| BUILTIN_VA_ARG LPAREN expression COMMA type_name RPAREN
      { let b, d = $5 in
        let loc = Parsing.rhs_start_pos 5, Parsing.rhs_end_pos 5 in
        let loc_f = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1 in
        make_expr
          (CALL
             ({ expr_loc = loc_f;
                expr_node = VARIABLE "__builtin_va_arg"},
              [$3; { expr_loc = loc;
                     expr_node = TYPE_SIZEOF (b, d)}]))
      }
| BUILTIN_TYPES_COMPAT LPAREN type_name COMMA type_name RPAREN
      { let b1,d1 = $3 in
        let b2,d2 = $5 in
        let loc_f = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1 in
        let loc1 = Parsing.rhs_start_pos 3, Parsing.rhs_end_pos 3 in
        let loc2 = Parsing.rhs_start_pos 5, Parsing.rhs_end_pos 5 in
        make_expr
          (CALL
             ({expr_loc = loc_f;
               expr_node = VARIABLE "__builtin_types_compatible_p"},
              [ { expr_loc = loc1; expr_node = TYPE_SIZEOF(b1,d1)};
                { expr_loc = loc2; expr_node = TYPE_SIZEOF(b2,d2)}]))
      }
| BUILTIN_OFFSETOF LPAREN type_name COMMA offsetof_member_designator RPAREN
      { transformOffsetOf $3 $5 }
| postfix_expression DOT id_or_typename { make_expr (MEMBEROF ($1, $3))}
| postfix_expression ARROW id_or_typename { make_expr (MEMBEROFPTR ($1, $3)) }
| postfix_expression PLUS_PLUS { make_expr (UNARY (POSINCR, $1)) }
| postfix_expression MINUS_MINUS { make_expr (UNARY (POSDECR, $1)) }
/* (* We handle GCC constructor expressions *) */
| LPAREN type_name RPAREN LBRACE initializer_list_opt RBRACE
      { make_expr (CAST($2, COMPOUND_INIT $5)) }
;

offsetof_member_designator:	/* GCC extension for __builtin_offsetof */
|		id_or_typename { make_expr (VARIABLE $1) }
|		offsetof_member_designator DOT IDENT
			{ make_expr (MEMBEROF ($1, $3)) }
|		offsetof_member_designator bracket_comma_expression
			{ make_expr (INDEX ($1, smooth_expression $2)) }
;

unary_expression:   /*(* 6.5.3 *)*/
|               postfix_expression
                        { $1 }
|		PLUS_PLUS unary_expression
		        {make_expr (UNARY (PREINCR, $2))}
|		MINUS_MINUS unary_expression
		        {make_expr (UNARY (PREDECR, $2))}
|		SIZEOF unary_expression
		        {make_expr (EXPR_SIZEOF $2)}
|	 	SIZEOF LPAREN type_name RPAREN
		        {let b, d = $3 in make_expr (TYPE_SIZEOF (b, d)) }
|		ALIGNOF unary_expression
		        { make_expr (EXPR_ALIGNOF $2) }
|	 	ALIGNOF LPAREN type_name RPAREN
		        {let b, d = $3 in make_expr (TYPE_ALIGNOF (b, d)) }
|		PLUS cast_expression
		        { make_expr (UNARY (PLUS, $2)) }
|		MINUS cast_expression
		        { make_expr (UNARY (MINUS, $2)) }
|		STAR cast_expression
		        {make_expr (UNARY (MEMOF, $2)) }
|		AND cast_expression
		        {make_expr (UNARY (ADDROF, $2))}
|		EXCLAM cast_expression
		        { make_expr (UNARY (NOT, $2)) }
|		TILDE cast_expression
		        { make_expr (UNARY (BNOT, $2)) }
/* (* GCC allows to take address of a label (see COMPGOTO statement) *) */
|               AND_AND id_or_typename_as_id { make_expr (LABELADDR $2) }
;

cast_expression:   /*(* 6.5.4 *)*/
| unary_expression { $1 }
| LPAREN type_name RPAREN cast_expression
      { make_expr (CAST($2, SINGLE_INIT $4)) }
;

multiplicative_expression:  /*(* 6.5.5 *)*/
| cast_expression { $1 }
| multiplicative_expression STAR cast_expression
      { make_expr (BINARY(MUL, $1, $3)) }
| multiplicative_expression SLASH cast_expression
      { make_expr (BINARY(DIV, $1, $3)) }
| multiplicative_expression PERCENT cast_expression
      { make_expr (BINARY(MOD, $1, $3)) }
;

additive_expression:  /*(* 6.5.6 *)*/
|               multiplicative_expression { $1 }
|		additive_expression PLUS multiplicative_expression
			{ make_expr (BINARY(ADD, $1, $3)) }
|		additive_expression MINUS multiplicative_expression
			{ make_expr (BINARY(SUB, $1, $3)) }
;

shift_expression:      /*(* 6.5.7 *)*/
|               additive_expression { $1 }
|		shift_expression  INF_INF additive_expression
			{make_expr (BINARY(SHL, $1, $3)) }
|		shift_expression  SUP_SUP additive_expression
			{ make_expr (BINARY(SHR, $1, $3)) }
;


relational_expression:   /*(* 6.5.8 *)*/
|               shift_expression { $1 }
|		relational_expression INF shift_expression
			{ make_expr (BINARY(LT, $1, $3)) }
|		relational_expression SUP shift_expression
			{ make_expr (BINARY(GT, $1, $3)) }
|		relational_expression INF_EQ shift_expression
			{ make_expr (BINARY(LE, $1, $3)) }
|		relational_expression SUP_EQ shift_expression
			{ make_expr (BINARY(GE, $1, $3)) }
;

equality_expression:   /*(* 6.5.9 *)*/
| relational_expression { $1 }
| equality_expression EQ_EQ relational_expression
      { make_expr (BINARY(EQ, $1, $3)) }
| equality_expression EXCLAM_EQ relational_expression
      { make_expr (BINARY(NE, $1, $3)) }
;

bitwise_and_expression:   /*(* 6.5.10 *)*/
|               equality_expression { $1 }
|		bitwise_and_expression AND equality_expression
			{ make_expr (BINARY(BAND, $1, $3)) }
;

bitwise_xor_expression:   /*(* 6.5.11 *)*/
|               bitwise_and_expression { $1 }
|		bitwise_xor_expression CIRC bitwise_and_expression
			{ make_expr (BINARY(XOR, $1, $3)) }
;

bitwise_or_expression:   /*(* 6.5.12 *)*/
|               bitwise_xor_expression { $1 }
|		bitwise_or_expression PIPE bitwise_xor_expression
			{ make_expr (BINARY(BOR, $1, $3)) }
;

logical_and_expression:   /*(* 6.5.13 *)*/
|               bitwise_or_expression { $1 }
|		logical_and_expression AND_AND bitwise_or_expression
			{ make_expr (BINARY(AND, $1, $3)) }
;

logical_or_expression:   /*(* 6.5.14 *)*/
|               logical_and_expression { $1 }
|		logical_or_expression PIPE_PIPE logical_and_expression
			{ make_expr (BINARY(OR, $1, $3)) }
;

conditional_expression:    /*(* 6.5.15 *)*/
| logical_or_expression { $1 }
| logical_or_expression QUEST opt_expression COLON conditional_expression
      { make_expr (QUESTION ($1, $3, $5)) }
;

/*(* The C spec says that left-hand sides of assignment expressions are unary
 * expressions. GCC allows cast expressions in there ! *)*/

assignment_expression:     /*(* 6.5.16 *)*/
|               conditional_expression { $1 }
|		cast_expression EQ assignment_expression
			{ make_expr (BINARY(ASSIGN, $1, $3)) }
|		cast_expression PLUS_EQ assignment_expression
			{ make_expr (BINARY(ADD_ASSIGN, $1, $3)) }
|		cast_expression MINUS_EQ assignment_expression
			{ make_expr (BINARY(SUB_ASSIGN, $1, $3)) }
|		cast_expression STAR_EQ assignment_expression
			{ make_expr (BINARY(MUL_ASSIGN, $1, $3)) }
|		cast_expression SLASH_EQ assignment_expression
			{ make_expr (BINARY(DIV_ASSIGN, $1, $3)) }
|		cast_expression PERCENT_EQ assignment_expression
			{ make_expr (BINARY(MOD_ASSIGN, $1, $3)) }
|		cast_expression AND_EQ assignment_expression
			{ make_expr (BINARY(BAND_ASSIGN, $1, $3)) }
|		cast_expression PIPE_EQ assignment_expression
			{ make_expr (BINARY(BOR_ASSIGN, $1, $3)) }
|		cast_expression CIRC_EQ assignment_expression
			{ make_expr (BINARY(XOR_ASSIGN, $1, $3)) }
|		cast_expression INF_INF_EQ assignment_expression
			{ make_expr (BINARY(SHL_ASSIGN, $1, $3)) }
|		cast_expression SUP_SUP_EQ assignment_expression
			{ make_expr (BINARY(SHR_ASSIGN, $1, $3))}
;

expression:           /*(* 6.5.17 *)*/
                assignment_expression { $1 }
;


constant:
    CST_INT				{CONST_INT (fst $1), snd $1}
|   CST_FLOAT				{CONST_FLOAT (fst $1), snd $1}
|   CST_CHAR				{CONST_CHAR (fst $1), snd $1}
|   CST_WCHAR				{CONST_WCHAR (fst $1), snd $1}
|   string_constant		        {CONST_STRING (fst $1), snd $1}
|   wstring_list			{CONST_WSTRING (fst $1), snd $1}
;

string_constant:
/* Now that we know this constant isn't part of a wstring, convert it
   back to a string for easy viewing. */
    string_list                         {
     let queue, location = $1 in
     let buffer = Buffer.create (Queue.length queue) in
     Queue.iter
       (List.iter
	  (fun value ->
	    let char = int64_to_char value in
	    Buffer.add_char buffer char))
       queue;
     Buffer.contents buffer, location
   }
;
one_string_constant:
/* Don't concat multiple strings.  For asm templates. */
    CST_STRING                          {intlist_to_string (fst $1) }
;
string_list:
    one_string                          {
      let queue = Queue.create () in
      Queue.add (fst $1) queue;
      queue, snd $1
    }
|   string_list one_string              {
      Queue.add (fst $2) (fst $1);
      $1
    }
;

wstring_list:
    CST_WSTRING                         { $1 }
|   wstring_list one_string             { (fst $1) @ (fst $2), snd $1 }
|   wstring_list CST_WSTRING            { (fst $1) @ (fst $2), snd $1 }
/* Only the first string in the list needs an L, so L"a" "b" is the same
 * as L"ab" or L"a" L"b". */

one_string:
    CST_STRING				{$1}
|   FUNCTION__                          {(Cabshelper.explodeStringToInts
					    !currentFunctionName), $1}
|   PRETTY_FUNCTION__                   {(Cabshelper.explodeStringToInts
					    !currentFunctionName), $1}
;

init_expression:
     expression         { SINGLE_INIT $1 }
|    LBRACE initializer_list_opt RBRACE
			{ COMPOUND_INIT $2}

initializer_list:    /* ISO 6.7.8. Allow a trailing COMMA */
    initializer_single                             { [$1] }
|   initializer_single COMMA initializer_list_opt  { $1 :: $3 }
;
initializer_list_opt:
    /* empty */                             { [] }
|   initializer_list                        { $1 }
;
initializer_single:
    init_designators eq_opt init_expression { ($1, $3) }
|   gcc_init_designators init_expression { ($1, $2) }
|                       init_expression { (NEXT_INIT, $1) }
;
eq_opt:
   EQ                        { () }
   /*(* GCC allows missing = *)*/
|  /*(* empty *)*/               { () }
;
init_designators:
    DOT id_or_typename init_designators_opt      { INFIELD_INIT($2, $3) }
|   LBRACKET  expression RBRACKET init_designators_opt { ATINDEX_INIT($2, $4) }
|   LBRACKET  expression ELLIPSIS expression RBRACKET
        { ATINDEXRANGE_INIT($2, $4) }
;
init_designators_opt:
   /* empty */                          { NEXT_INIT }
|  init_designators                     { $1 }
;

gcc_init_designators:  /*(* GCC supports these strange things *)*/
   id_or_typename COLON                 { INFIELD_INIT($1, NEXT_INIT) }
;

arguments:
                /* empty */         { [] }
|               comma_expression    { $1 }
;

opt_expression:
	        /* empty */ {make_expr NOTHING}
|	        comma_expression {smooth_expression $1 }
;

comma_expression:
	        expression                        { [$1] }
|               expression COMMA comma_expression { $1 :: $3 }
|               error COMMA comma_expression      { $3 }
;

comma_expression_opt:
                /* empty */         { make_expr NOTHING }
|               comma_expression    { smooth_expression $1 }
;

paren_comma_expression:
  LPAREN comma_expression RPAREN                   { $2 }
| LPAREN error RPAREN                              { [] }
;

bracket_comma_expression:
  LBRACKET comma_expression RBRACKET                   { $2 }
| LBRACKET error RBRACKET                              { [] }
;


/*** statements ***/
block: /* ISO 6.8.2 */
    block_begin local_labels block_attrs block_element_list RBRACE
      {!Lexerhack.pop_context();
       { blabels = $2;
         battrs = $3;
         bstmts = $4 },
       $1, $5
      }
|   error location RBRACE                { { blabels = [];
                                             battrs  = [];
                                             bstmts  = [] },
					     $2, $3
                                         }
;
block_begin:
    LBRACE  { !Lexerhack.push_context (); $1 }
;

block_attrs:
   /* empty */                                              { [] }
|  BLOCKATTRIBUTE paren_attr_list_ne
                                        { [("__blockattribute__", $2)] }
;

/* statements and declarations in a block, in any order (for C99 support) */
block_element_list:
|   annot_list_opt                                      { $1 }
|   annot_list_opt declaration block_element_list
        { $1 @ no_ghost_stmt (DEFINITION($2)) :: $3 }
|   annot_list_opt statement block_element_list
            { $1 @ $2 @ $3 }
|   annot_list_opt pragma block_element_list            { $1 @ $3 }
/*(* GCC accepts a label at the end of a block *)*/
|   annot_list_opt id_or_typename_as_id COLON
    { let loc = Parsing.rhs_start_pos 2, Parsing.rhs_end_pos 3 in
      $1 @ no_ghost [LABEL ($2, no_ghost_stmt (NOP loc), loc)] }
;

annot_list_opt:
    /* empty */     { [] }
|   annot_list      { $1 }
;

annot_list:
   CODE_ANNOT annot_list_opt { no_ghost [Cabs.CODE_ANNOT $1] @ $2}
|  LGHOST block_element_list RGHOST annot_list_opt
       { (in_ghost $2) @ $4 }
;

local_labels:
   /* empty */                                       { [] }
|  LABEL__ local_label_names SEMICOLON local_labels  { $2 @ $4 }
;
local_label_names:
   id_or_typename_as_id                              { [ $1 ] }
|  id_or_typename_as_id COMMA local_label_names      { $1 :: $3 }
;


annotated_statement:
|   statement { $1 }
|   annot_list statement { $1 @ $2 }
;

statement:
    SEMICOLON		{ no_ghost [NOP $1] }
|   SPEC annotated_statement
      {
        let bs = $2 in
        Cabshelper.continue_annot
	  (currentLoc())
	  (fun () ->
             let (_,spec) = Logic_lexer.spec $1 in
             let spec = no_ghost [Cabs.CODE_SPEC spec] in
             spec @ $2)
	  (fun () -> bs)
	  "Skipping annotation"
      }
|   comma_expression SEMICOLON
	  { let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
            no_ghost [COMPUTATION (smooth_expression $1,loc)]}
|   block { let (x,y,z) = $1 in no_ghost [BLOCK (x, y, z)]}
|   IF paren_comma_expression annotated_statement
        { let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
          no_ghost [IF (smooth_expression $2,
                       in_block $3, no_ghost_stmt (NOP loc), loc)]}
|   IF paren_comma_expression annotated_statement ELSE annotated_statement
	{ let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
          no_ghost
            [IF (smooth_expression $2, in_block $3, in_block $5, loc)]}
|   SWITCH paren_comma_expression annotated_statement
        { let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
          no_ghost [SWITCH (smooth_expression $2, in_block $3, loc)]}
|   opt_loop_annotations
    WHILE paren_comma_expression annotated_statement
    { let loc = Parsing.rhs_start_pos 2, Parsing.symbol_end_pos () in
      no_ghost [WHILE ($1, smooth_expression $3, in_block $4, loc)] }
|   opt_loop_annotations
    DO annotated_statement WHILE paren_comma_expression SEMICOLON
    { let loc = Parsing.rhs_start_pos 2, Parsing.symbol_end_pos () in
      no_ghost [DOWHILE ($1, smooth_expression $5, in_block $3, loc)]}
|   opt_loop_annotations
    FOR LPAREN for_clause opt_expression
    SEMICOLON opt_expression RPAREN annotated_statement
    { let loc = Parsing.rhs_start_pos 2, Parsing.symbol_end_pos () in
      no_ghost [FOR ($1, $4, $5, $7, in_block $9, loc)]}
|   id_or_typename_as_id COLON  attribute_nocv_list annotated_statement
	{(* The only attribute that should appear here
            is "unused". For now, we drop this on the
            floor, since unused labels are usually
            removed anyways by Rmtmps. *)
          let loc = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 2 in
          no_ghost [LABEL ($1, in_block $4, loc)]}
|   CASE expression COLON annotated_statement
	    { let loc = Parsing.symbol_start_pos (), Parsing.rhs_end_pos 3 in
              no_ghost [CASE ($2, in_block $4, loc)]}
|   CASE expression ELLIPSIS expression COLON annotated_statement
	    { let loc = Parsing.symbol_start_pos (), Parsing.rhs_end_pos 5 in
              no_ghost [CASERANGE ($2, $4, in_block $6, loc)]}
|   DEFAULT COLON
	    { let loc = Parsing.symbol_start_pos(), Parsing.symbol_end_pos () in
              no_ghost [DEFAULT (no_ghost_stmt (NOP loc), loc)]}
|   RETURN SEMICOLON {
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
      no_ghost [RETURN ({ expr_loc = loc; expr_node = NOTHING}, loc)]
    }
|   RETURN comma_expression SEMICOLON {
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
      no_ghost [RETURN (smooth_expression $2, loc)]
    }
|   BREAK SEMICOLON     {
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
      no_ghost [BREAK loc]
    }
|   CONTINUE SEMICOLON	 {
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
      no_ghost [CONTINUE loc]
    }
|   GOTO id_or_typename_as_id SEMICOLON {
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
      no_ghost [GOTO ($2, loc)]
    }
|   GOTO STAR comma_expression SEMICOLON {
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
      no_ghost [COMPGOTO (smooth_expression $3, loc) ]
    }
|   ASM GOTO asmattr LPAREN asmtemplate asmoutputs RPAREN SEMICOLON {
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
      no_ghost [ASM ($3, $5, $6, loc)]
    }
|   ASM asmattr LPAREN asmtemplate asmoutputs RPAREN SEMICOLON {
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
      no_ghost [ASM ($2, $4, $5, loc)]
    }
|   MSASM   { no_ghost [ASM ([], [fst $1], None, snd $1)]}
|   TRY block EXCEPT paren_comma_expression block {
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
      let loc_e = Parsing.rhs_start_pos 4, Parsing.rhs_end_pos 4 in
      let b, _, _ = $2 in
      let h, _, _ = $5 in
      if not !Cprint.msvcMode then parse_error "try/except in GCC code";
      no_ghost
        [TRY_EXCEPT (b, {expr_loc = loc_e; expr_node = COMMA $4}, h, loc)]
    }
|   TRY block FINALLY block {
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos () in
      let b, _, _ = $2 in
      let h, _, _ = $4 in
      if not !Cprint.msvcMode then parse_error "try/finally in GCC code";
      no_ghost [TRY_FINALLY (b, h, loc)]
    }
|   error location   SEMICOLON   { no_ghost [NOP $2]}
;

opt_loop_annotations:
  /* epsilon */ { [] }
| loop_annotations { $1 }
;

loop_annotations:
  loop_annotation { $1 }
/* Not in ACSL Grammar
| loop_annotation loop_annotations
    { { Cil_types.invariant = $1.Cil_types.invariant @ $2.Cil_types.invariant;
        Cil_types.loop_assigns = $1.Cil_types.loop_assigns @ $2.Cil_types.loop_assigns ;
        Cil_types.variant = $1.Cil_types.variant @ $2.Cil_types.variant;
        Cil_types.pragma = $1.Cil_types.pragma @ $2.Cil_types.pragma }
    }
*/
;

loop_annotation:
 LOOP_ANNOT    { fst $1 }
;

for_clause:
    opt_expression SEMICOLON     { FC_EXP $1 }
|   declaration                  { FC_DECL $1 }
;

declaration:                                /* ISO 6.7.*/
    decl_spec_list init_declarator_list SEMICOLON
      { doDeclaration None ((snd $1)) (fst $1) $2 }
|   decl_spec_list SEMICOLON
      { doDeclaration None ((snd $1)) (fst $1) [] }
|   SPEC decl_spec_list init_declarator_list SEMICOLON
          { doDeclaration (Some $1) ((snd $2)) (fst $2) $3 }
|   SPEC decl_spec_list SEMICOLON
      { doDeclaration (Some $1) ((snd $2)) (fst $2) [] }
;

init_declarator_list:                       /* ISO 6.7 */
    init_declarator                              { [$1] }
|   init_declarator COMMA init_declarator_list   { $1 :: $3 }

;
init_declarator:                             /* ISO 6.7 */
    declarator                          { ($1, NO_INIT) }
|   declarator EQ init_expression
                                        { ($1, $3) }
;

decl_spec_wo_type:                         /* ISO 6.7 */
                                        /* ISO 6.7.1 */
|   TYPEDEF          { SpecTypedef, $1  }
|   EXTERN           { SpecStorage EXTERN, $1 }
|   STATIC           { SpecStorage STATIC, $1 }
|   AUTO             { SpecStorage AUTO, $1 }
|   REGISTER         { SpecStorage REGISTER, $1}
                                        /* ISO 6.7.4 */
|   INLINE           { SpecInline, $1 }
|   cvspec           { $1 }
|   attribute_nocv   { SpecAttr (fst $1), snd $1 }
;

decl_spec_list:
| decl_spec_wo_type decl_spec_list_opt { fst $1 :: $2, snd $1 }
| type_spec decl_spec_list_opt_no_named { SpecType(fst $1) :: $2, snd $1 }

decl_spec_list_no_named:
| decl_spec_wo_type decl_spec_list_opt_no_named { fst $1 :: $2, snd $1 }
| type_spec decl_spec_list_opt_no_named { SpecType(fst $1) :: $2, snd $1 }

/* (* In most cases if we see a NAMED_TYPE we must shift it. Thus we declare
    * NAMED_TYPE to have right associativity  *) */
decl_spec_list_opt:
    /* empty */                         { [] } %prec NAMED_TYPE
|   decl_spec_list                      { fst $1 }
;
/* (* We add this separate rule to handle the special case when an appearance
    * of NAMED_TYPE should not be considered as part of the specifiers but as
    * part of the declarator. IDENT has higher precedence than NAMED_TYPE  *)
 */
decl_spec_list_opt_no_named:
    /* empty */                         { [] } %prec IDENT
|   decl_spec_list_no_named                      { fst $1 }
;
type_spec:   /* ISO 6.7.2 */
    VOID            { Tvoid, $1}
|   CHAR            { Tchar, $1 }
|   BOOL            { Tbool, $1 }
|   SHORT           { Tshort, $1 }
|   INT             { Tint, $1 }
|   LONG            { Tlong, $1 }
|   INT64           { Tint64, $1 }
|   FLOAT           { Tfloat, $1 }
|   DOUBLE          { Tdouble, $1 }
|   SIGNED          { Tsigned, $1 }
|   UNSIGNED        { Tunsigned, $1 }
|   STRUCT                 id_or_typename
                                                   { Tstruct ($2, None,    []), $1 }
|   STRUCT just_attributes id_or_typename
                                                   { Tstruct ($3, None,    $2), $1 }
|   STRUCT                 id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tstruct ($2, Some $4, []), $1 }
|   STRUCT                                LBRACE struct_decl_list RBRACE
                                                   { Tstruct ("", Some $3, []), $1 }
|   STRUCT just_attributes id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tstruct ($3, Some $5, $2), $1 }
|   STRUCT just_attributes                LBRACE struct_decl_list RBRACE
                                                   { Tstruct ("", Some $4, $2), $1 }
|   UNION                  id_or_typename
                                                   { Tunion  ($2, None,    []), $1 }
|   UNION                  id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tunion  ($2, Some $4, []), $1 }
|   UNION                                 LBRACE struct_decl_list RBRACE
                                                   { Tunion  ("", Some $3, []), $1 }
|   UNION  just_attributes id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tunion  ($3, Some $5, $2), $1 }
|   UNION  just_attributes                LBRACE struct_decl_list RBRACE
                                                   { Tunion  ("", Some $4, $2), $1 }
|   ENUM                   id_or_typename
                                                   { Tenum   ($2, None,    []), $1 }
|   ENUM                   id_or_typename LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ($2, Some $4, []), $1 }
|   ENUM                                  LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ("", Some $3, []), $1 }
|   ENUM   just_attributes id_or_typename LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ($3, Some $5, $2), $1 }
|   ENUM   just_attributes                LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ("", Some $4, $2), $1 }
|   NAMED_TYPE      {
      (Tnamed $1, (Parsing.symbol_start_pos (), Parsing.symbol_end_pos()))
      }
|   TYPEOF LPAREN expression RPAREN     { TtypeofE $3, $1 }
|   TYPEOF LPAREN type_name RPAREN      { let s, d = $3 in
                                          TtypeofT (s, d), $1 }
;
struct_decl_list: /* (* ISO 6.7.2. Except that we allow empty structs. We
                      * also allow missing field names. *)
                   */
   /* empty */                           { [] }
|  decl_spec_list                 SEMICOLON struct_decl_list
                                         { FIELD (fst $1,
                                            [(missingFieldDecl, None)]) :: $3 }
/*(* GCC allows extra semicolons *)*/
|                                 SEMICOLON struct_decl_list
                                         { $2 }
|  decl_spec_list field_decl_list SEMICOLON struct_decl_list
                                          { FIELD (fst $1, $2)
                                            :: $4 }
/*(* MSVC allows pragmas in strange places *)*/
|  pragma struct_decl_list                { $2 }

|  error                          SEMICOLON struct_decl_list
                                          { $3 }
;
field_decl_list: /* (* ISO 6.7.2 *) */
    field_decl                           { [$1] }
|   field_decl COMMA field_decl_list     { $1 :: $3 }
;
field_decl: /* (* ISO 6.7.2. Except that we allow unnamed fields. *) */
|   declarator                      { ($1, None) }
|   declarator COLON expression attributes
                                    { let (n,decl,al,loc) = $1 in
                                      let al' = al @ $4 in
                                     ((n,decl,al',loc), Some $3) }
|              COLON expression     { (missingFieldDecl, Some $2) }
;

enum_list: /* (* ISO 6.7.2.2 *) */
    enumerator				{[$1]}
|   enum_list COMMA enumerator	        {$1 @ [$3]}
|   enum_list COMMA error               { $1 }
;
enumerator:
    IDENT			{
      let loc = Parsing.symbol_start_pos (), Parsing.symbol_end_pos() in
      ($1, { expr_node = NOTHING; expr_loc = loc }, loc)
    }
|   IDENT EQ expression		{
      ($1, $3, (Parsing.symbol_start_pos (),Parsing.symbol_end_pos()))
    }
;


declarator:  /* (* ISO 6.7.5. Plus Microsoft declarators.*) */
   pointer_opt direct_decl attributes_with_asm {
     let (n, decl) = $2 in (n, applyPointer (fst $1) decl, $3, (snd $1))
   }
;


attributes_or_static: /* 6.7.5.2/3 */
| attributes comma_expression_opt { $1,$2 }
| attribute attributes STATIC comma_expression {
    fst $1::$2  @ ["static",[]], smooth_expression $4
  }
| STATIC attributes comma_expression {
    ("static",[]) :: $2, smooth_expression $3
  }
;

direct_decl: /* (* ISO 6.7.5 *) */
                                   /* (* We want to be able to redefine named
                                    * types as variable names *) */
|   id_or_typename                 { ($1, JUSTBASE) }

|   LPAREN attributes declarator RPAREN
                                   { let (n,decl,al,_) = $3 in
                                     (n, PARENTYPE($2,decl,al)) }

|   direct_decl LBRACKET attributes_or_static  RBRACKET
                                   { let (n, decl) = $1 in
                                     let (attrs, size) = $3 in
                                     (n, ARRAY(decl, attrs, size)) }
|   direct_decl parameter_list_startscope rest_par_list RPAREN
                                   { let (n, decl) = $1 in
                                     let (params, isva) = $3 in
                                     !Lexerhack.pop_context ();
                                     (n, PROTO(decl, params, isva))
                                   }
;
parameter_list_startscope:
    LPAREN                         { !Lexerhack.push_context () }
;
rest_par_list:
|   /* empty */                    { ([], false) }
|   parameter_decl rest_par_list1  { let (params, isva) = $2 in
                                     ($1 :: params, isva)
                                   }
;
rest_par_list1:
    /* empty */                         { ([], false) }
|   COMMA ELLIPSIS                      { ([], true) }
|   COMMA parameter_decl rest_par_list1 { let (params, isva) = $3 in
                                          ($2 :: params, isva)
                                        }
;


parameter_decl: /* (* ISO 6.7.5 *) */
   decl_spec_list declarator              { (fst $1, $2) }
|  decl_spec_list abstract_decl           { let d, a = $2 in
                                            (fst $1, ("", d, a, (*CEA*) cabslu)) }
|  decl_spec_list                         { (fst $1, ("", JUSTBASE, [], (*CEA*) cabslu)) }
|  LPAREN parameter_decl RPAREN           { $2 }
;

/* (* Old style prototypes. Like a declarator *) */
old_proto_decl:
  pointer_opt direct_old_proto_decl   { let (n, decl, a) = $2 in
                                        (n, applyPointer (fst $1) decl,
                                         a, snd $1)
                                      }

;

direct_old_proto_decl:
  direct_decl LPAREN old_parameter_list_ne RPAREN old_pardef_list {
    let par_decl, isva = doOldParDecl $3 $5 in
    let n, decl = $1 in
    (n, PROTO(decl, par_decl, isva), ["FC_OLDSTYLEPROTO",[]])
  }
| direct_decl LPAREN RPAREN {
    let n, decl = $1 in (n, PROTO(decl, [], false), [])
  }

/* (* appears sometimesm but generates a shift-reduce conflict. *)
| LPAREN STAR direct_decl LPAREN old_parameter_list_ne RPAREN RPAREN
    LPAREN RPAREN old_pardef_list {
      let par_decl, isva = doOldParDecl $5 $10 in
      let n, decl = $3 in (n, PROTO(decl, par_decl, isva), [])
    }
*/
;

old_parameter_list_ne:
|  IDENT                                       { [$1] }
|  IDENT COMMA old_parameter_list_ne           { $1::$3 }
;

old_pardef_list:
   /* empty */                            { ([], false) }
|  decl_spec_list old_pardef SEMICOLON ELLIPSIS
                                          { ([(fst $1, $2)], true) }
|  decl_spec_list old_pardef SEMICOLON old_pardef_list
                                          { let rest, isva = $4 in
                                            ((fst $1, $2) :: rest, isva)
                                          }
;

old_pardef:
   declarator                             { [$1] }
|  declarator COMMA old_pardef            { $1 :: $3 }
|  error                                  { [] }
;


pointer: /* (* ISO 6.7.5 *) */
   STAR attributes pointer_opt  { $2 :: fst $3, $1 }
;
pointer_opt:
   /**/                          { let l = currentLoc () in
                                   ([], l) }
|  pointer                       { $1 }
;

type_name: /* (* ISO 6.7.6 *) */
  decl_spec_list abstract_decl { let d, a = $2 in
                                 if a <> [] then parse_error "attributes in type name" ;
                                 (fst $1, d)
                               }
| decl_spec_list               { (fst $1, JUSTBASE) }
;
abstract_decl: /* (* ISO 6.7.6. *) */
  pointer_opt abs_direct_decl attributes  { applyPointer (fst $1) $2, $3 }
| pointer                                 { applyPointer (fst $1) JUSTBASE, [] }
;

abs_direct_decl: /* (* ISO 6.7.6. We do not support optional declarator for
                     * functions. Plus Microsoft attributes. See the
                     * discussion for declarator. *) */
|   LPAREN attributes abstract_decl RPAREN
                                   { let d, a = $3 in
                                     PARENTYPE ($2, d, a)
                                   }

|   LPAREN error RPAREN
                                   { JUSTBASE }

|   abs_direct_decl_opt LBRACKET comma_expression_opt RBRACKET
                                   { ARRAY($1, [], $3) }
/*(* The next should be abs_direct_decl_opt but we get conflicts *)*/
|   abs_direct_decl  parameter_list_startscope rest_par_list RPAREN
                                   { let (params, isva) = $3 in
                                     !Lexerhack.pop_context ();
                                     PROTO ($1, params, isva)
                                   }
;
abs_direct_decl_opt:
    abs_direct_decl                 { $1 }
|   /* empty */                     { JUSTBASE }
;
function_def:  /* (* ISO 6.9.1 *) */
  SPEC function_def_start block
          {
            let (loc, specs, decl) = $2 in
            let spec =
              Cabshelper.continue_annot loc
                (fun () -> Some (snd (Logic_lexer.spec $1 )))
                (fun () -> None)
                "Ignoring specification of function %s" !currentFunctionName
            in
            currentFunctionName := "<__FUNCTION__ used outside any functions>";
            !Lexerhack.pop_context (); (* The context pushed by
                                    * announceFunctionName *)
            doFunctionDef spec loc (trd3 $3) specs decl (fst3 $3)
          }
|  function_def_start block
          { let (loc, specs, decl) = $1 in
            currentFunctionName := "<__FUNCTION__ used outside any functions>";
            !Lexerhack.pop_context (); (* The context pushed by
                                    * announceFunctionName *)
            (*OCAMLYACC BUG??? Format.printf "%a@." d_cabsloc (trd3 $2);*)
            doFunctionDef None ((*handleLoc*) loc) (trd3 $2) specs decl (fst3 $2)
          }


function_def_start:  /* (* ISO 6.9.1 *) */
  decl_spec_list declarator
                            { announceFunctionName $2;
                              (fourth4 $2, fst $1, $2)
                            }

/* (* Old-style function prototype *) */
| decl_spec_list old_proto_decl
                            { announceFunctionName $2;
                              (snd $1, fst $1, $2)
                            }
/* (* New-style function that does not have a return type *) */
| IDENT parameter_list_startscope rest_par_list RPAREN
    { let (params, isva) = $3 in
      let loc = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1 in
      let fdec =
        ($1, PROTO(JUSTBASE, params, isva), [], loc) in
      announceFunctionName fdec;
      (* Default is int type *)
      let defSpec = [SpecType Tint] in (loc, defSpec, fdec)
    }

/* (* No return type and old-style parameter list *) */
| IDENT LPAREN old_parameter_list_ne RPAREN old_pardef_list
    { (* Convert pardecl to new style *)
      let pardecl, isva = doOldParDecl $3 $5 in
      let loc = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1 in
      (* Make the function declarator *)
      let fdec = ($1, PROTO(JUSTBASE, pardecl,isva), [], loc) in
      announceFunctionName fdec;
      (* Default is int type *)
      let defSpec = [SpecType Tint] in (loc, defSpec, fdec)
    }
/* (* No return type and no parameters *) */
| IDENT LPAREN RPAREN
    { (* Make the function declarator *)
      let loc = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1 in
      let fdec = ($1, PROTO(JUSTBASE, [], false), [], loc) in
      announceFunctionName fdec;
      (* Default is int type *)
      let defSpec = [SpecType Tint] in (loc, defSpec, fdec)
    }
;

/* const/volatile as type specifier elements */
cvspec:
  | CONST                    { SpecCV(CV_CONST), $1 }
  | VOLATILE                 { SpecCV(CV_VOLATILE), $1 }
  | RESTRICT                 { SpecCV(CV_RESTRICT), $1 }
  | ATTRIBUTE_ANNOT          { let annot, loc = $1 in
			       SpecCV(CV_ATTRIBUTE_ANNOT annot), loc }
;

/*** GCC attributes ***/
attributes:
    /* empty */				{ []}
|   attribute attributes	        { fst $1 :: $2 }
;

/* (* In some contexts we can have an inline assembly to specify the name to
    * be used for a global. We treat this as a name attribute *) */
attributes_with_asm:
    /* empty */                         { [] }
|   attribute attributes_with_asm       { fst $1 :: $2 }
|   ASM LPAREN string_constant RPAREN attributes {
      let loc = Parsing.rhs_start_pos 3, Parsing.rhs_end_pos 3 in
      ("__asm__",
       [{ expr_node = CONSTANT(CONST_STRING (fst $3)); expr_loc = loc}])
      :: $5
    }
;

/* things like __attribute__, but no const/volatile */
attribute_nocv:
    ATTRIBUTE LPAREN paren_attr_list RPAREN
                                        { ("__attribute__", $3), $1 }
/*(*
|   ATTRIBUTE_USED                      { ("__attribute__",
                                             [ VARIABLE "used" ]), $1 }
*)*/
|   DECLSPEC paren_attr_list_ne         { ("__declspec", $2), $1 }
|   MSATTR                              { (fst $1, []), snd $1 }
                                        /* ISO 6.7.3 */
|   THREAD                              { ("__thread",[]), $1 }
;

attribute_nocv_list:
    /* empty */				{ []}
|   attribute_nocv attribute_nocv_list  { fst $1 :: $2 }
;

/* __attribute__ plus const/volatile */
attribute:
    attribute_nocv        { $1 }
|   CONST                 { ("const", []), $1 }
|   RESTRICT              { ("restrict",[]), $1 }
|   VOLATILE              { ("volatile",[]), $1 }
|   ATTRIBUTE_ANNOT       { let annot, loc = $1 in
			    ("$annot:" ^ annot, []), loc }
;

/* (* sm: I need something that just includes __attribute__ and nothing more,
 *  to support them appearing between the 'struct' keyword and the type name.
 * Actually, a declspec can appear there as well (on MSVC) *)  */
just_attribute:
    ATTRIBUTE LPAREN paren_attr_list RPAREN
                                        { ("__attribute__", $3) }
|   DECLSPEC paren_attr_list_ne         { ("__declspec", $2) }
;

/* this can't be empty, b/c I folded that possibility into the calling
 * productions to avoid some S/R conflicts */
just_attributes:
    just_attribute                      { [$1] }
|   just_attribute just_attributes      { $1 :: $2 }
;

/** (* PRAGMAS and ATTRIBUTES *) ***/
pragma:
| PRAGMA PRAGMA_EOL		        {
    PRAGMA (make_expr (VARIABLE ("")), $1)
  }
| PRAGMA attr PRAGMA_EOL		{ PRAGMA ($2, $1) }
| PRAGMA attr SEMICOLON PRAGMA_EOL	{ PRAGMA ($2, $1) }
| PRAGMA_LINE                           {
    PRAGMA (make_expr (VARIABLE (fst $1)), snd $1)
  }
;

/* (* We want to allow certain strange things that occur in pragmas, so we
    * cannot use directly the language of expressions *) */
var_attr:
|   IDENT { make_expr (VARIABLE $1) }
|   NAMED_TYPE { make_expr (VARIABLE $1) }
|   DEFAULT COLON CST_INT { make_expr (VARIABLE ("default:" ^ fst $3)) }
      /* Const when it appears in attribute lists, is translated to aconst */
|   CONST { make_expr (VARIABLE "aconst") }
/*(** GCC allows this as an attribute for functions, synonym for noreturn **)*/
|   VOLATILE { make_expr (VARIABLE ("__noreturn__")) }
|   IDENT COLON CST_INT { make_expr (VARIABLE ($1 ^ ":" ^ fst $3)) }
|   NAMED_TYPE COLON CST_INT { make_expr (VARIABLE ($1 ^ ":" ^ fst $3)) }
/*(* The following rule conflicts with the ? : attributes. We give it a very
   * low priority *)*/
|   CST_INT COLON CST_INT { make_expr (VARIABLE (fst $1 ^ ":" ^ fst $3)) }
;

basic_attr:
|   CST_INT { make_expr (CONSTANT(CONST_INT (fst $1))) }
|   var_attr { $1 }
;
basic_attr_list_ne:
    basic_attr				{ [$1] }
|   basic_attr basic_attr_list_ne	{ $1::$2 }
;

parameter_attr_list_ne:
    basic_attr_list_ne			{ $1 }
|   basic_attr_list_ne string_constant	{
      $1 @ [make_expr (CONSTANT(CONST_STRING (fst $2)))]
    }
|   basic_attr_list_ne string_constant parameter_attr_list_ne {
      $1 @ ([make_expr (CONSTANT(CONST_STRING (fst $2)))] @ $3)
    }
;
param_attr_list_ne:
    parameter_attr_list_ne { $1 }
|   string_constant { [make_expr (CONSTANT(CONST_STRING (fst $1)))] }
;
primary_attr:
    basic_attr { $1 }
|   LPAREN attr RPAREN { $2 }
|   string_constant { make_expr (CONSTANT(CONST_STRING (fst $1))) }
;
postfix_attr:
    primary_attr { $1 }
|   id_or_typename_as_id paren_attr_list_ne {
        let loc = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1 in
        make_expr (CALL({ expr_loc = loc; expr_node = VARIABLE $1}, $2)) }
      /* (* use a VARIABLE "" so that the parentheses are printed *) */
|   id_or_typename_as_id LPAREN  RPAREN {
      let loc1 = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1 in
      let loc2 = Parsing.rhs_start_pos 2, Parsing.rhs_end_pos 3 in
      let f = { expr_node = VARIABLE $1; expr_loc = loc1 } in
      let arg = { expr_node = VARIABLE ""; expr_loc = loc2 } in
      make_expr (CALL(f, [arg]))
    }
      /* (* use a VARIABLE "" so that the parameters are printed without
          * parentheses nor comma *) */
|   basic_attr param_attr_list_ne  {
      let loc = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1 in
      make_expr (CALL({ expr_node = VARIABLE ""; expr_loc = loc}, $1::$2)) }

|   postfix_attr ARROW id_or_typename   { make_expr (MEMBEROFPTR ($1, $3))}
|   postfix_attr DOT id_or_typename     { make_expr (MEMBEROF ($1, $3)) }
|   postfix_attr LBRACKET attr RBRACKET { make_expr (INDEX ($1, $3)) }
;

/*(* Since in attributes we use both IDENT and NAMED_TYPE as indentifiers,
 * that leads to conflicts for SIZEOF and ALIGNOF. In those cases we require
 * that their arguments be expressions, not attributes *)*/
unary_attr:
    postfix_attr                        { $1 }
|   SIZEOF unary_expression             { make_expr (EXPR_SIZEOF $2) }
|   SIZEOF LPAREN type_name RPAREN
		                        {let b, d = $3 in
                                         make_expr (TYPE_SIZEOF (b, d)) }

|   ALIGNOF unary_expression            {make_expr (EXPR_ALIGNOF $2) }
|   ALIGNOF LPAREN type_name RPAREN     {let b, d = $3 in
                                         make_expr (TYPE_ALIGNOF (b, d)) }
|   PLUS cast_attr                      {make_expr (UNARY (PLUS, $2))}
|   MINUS cast_attr                     {make_expr (UNARY (MINUS, $2)) }
|   STAR cast_attr		        {make_expr (UNARY (MEMOF, $2)) }
|   AND cast_attr
	                                { make_expr (UNARY (ADDROF, $2)) }
|   EXCLAM cast_attr    	        { make_expr (UNARY (NOT, $2)) }
|   TILDE cast_attr                     { make_expr (UNARY (BNOT, $2)) }
;

cast_attr:
    unary_attr                          { $1 }
;

multiplicative_attr:
    cast_attr                           { $1 }
|   multiplicative_attr STAR cast_attr  {make_expr (BINARY(MUL ,$1 , $3))}
|   multiplicative_attr SLASH cast_attr	{make_expr (BINARY(DIV ,$1 , $3))}
|   multiplicative_attr PERCENT cast_attr {make_expr (BINARY(MOD ,$1 , $3))}
;


additive_attr:
    multiplicative_attr                 { $1 }
|   additive_attr PLUS multiplicative_attr  {make_expr (BINARY(ADD ,$1 , $3))}
|   additive_attr MINUS multiplicative_attr {make_expr (BINARY(SUB ,$1 , $3))}
;

shift_attr:
    additive_attr                       { $1 }
|   shift_attr INF_INF additive_attr	{make_expr (BINARY(SHL ,$1 , $3))}
|   shift_attr SUP_SUP additive_attr	{make_expr (BINARY(SHR ,$1 , $3))}
;

relational_attr:
    shift_attr                          { $1 }
|   relational_attr INF shift_attr	{make_expr (BINARY(LT ,$1 , $3))}
|   relational_attr SUP shift_attr	{make_expr (BINARY(GT ,$1 , $3))}
|   relational_attr INF_EQ shift_attr	{make_expr (BINARY(LE ,$1 , $3))}
|   relational_attr SUP_EQ shift_attr	{make_expr (BINARY(GE ,$1 , $3))}
;

equality_attr:
    relational_attr                     { $1 }
|   equality_attr EQ_EQ relational_attr	    {make_expr (BINARY(EQ ,$1 , $3))}
|   equality_attr EXCLAM_EQ relational_attr {make_expr (BINARY(NE ,$1 , $3))}
;


bitwise_and_attr:
    equality_attr                       { $1 }
|   bitwise_and_attr AND equality_attr	{make_expr (BINARY(BAND ,$1 , $3))}
;

bitwise_xor_attr:
    bitwise_and_attr                       { $1 }
|   bitwise_xor_attr CIRC bitwise_and_attr {make_expr (BINARY(XOR ,$1 , $3))}
;

bitwise_or_attr:
    bitwise_xor_attr                      { $1 }
|   bitwise_or_attr PIPE bitwise_xor_attr {make_expr (BINARY(BOR ,$1 , $3))}
;

logical_and_attr:
    bitwise_or_attr                             { $1 }
|   logical_and_attr AND_AND bitwise_or_attr
        {make_expr (BINARY(AND ,$1 , $3))}
;

logical_or_attr:
    logical_and_attr                           { $1 }
|   logical_or_attr PIPE_PIPE logical_and_attr
        {make_expr (BINARY(OR ,$1 , $3))}
;

conditional_attr:
    logical_or_attr                        { $1 }
/* This is in conflict for now */
|   logical_or_attr QUEST conditional_attr COLON conditional_attr
    { make_expr (QUESTION($1, $3, $5)) }


attr: conditional_attr                    { $1 }
;

attr_list_ne:
|  attr                                  { [$1] }
|  attr COMMA attr_list_ne               { $1 :: $3 }
|  error COMMA attr_list_ne              { $3 }
;
attr_list:
  /* empty */                            { [] }
| attr_list_ne                           { $1 }
;
paren_attr_list_ne:
   LPAREN attr_list_ne RPAREN            { $2 }
|  LPAREN error RPAREN                   { [] }
;
paren_attr_list:
   LPAREN attr_list RPAREN               { $2 }
|  LPAREN error RPAREN                   { [] }
;
/*** GCC ASM instructions ***/
asmattr:
     /* empty */                        { [] }
|    VOLATILE  asmattr                  { ("volatile", []) :: $2 }
|    CONST asmattr                      { ("const", []) :: $2 }
;
asmtemplate:
    one_string_constant                          { [$1] }
|   one_string_constant asmtemplate              { $1 :: $2 }
;
asmoutputs:
  /* empty */           { None }
| COLON asmoperands asminputs
                        { let (ins, (clobs,labels)) = $3 in
                          Some {aoutputs = $2; ainputs = ins; aclobbers = clobs;
				alabels = labels } }
;
asmoperands:
     /* empty */                        { [] }
|    asmoperandsne                      { List.rev $1 }
;
asmoperandsne:
     asmoperand                         { [$1] }
|    asmoperandsne COMMA asmoperand     { $3 :: $1 }
;
asmoperand:
     asmopname string_constant LPAREN expression RPAREN    { ($1, fst $2, $4) }
|    asmopname string_constant LPAREN error RPAREN         {
       let loc = Parsing.rhs_start_pos 4, Parsing.rhs_end_pos 4 in
       ($1, fst $2, { expr_loc = loc; expr_node = NOTHING} )
     }
;
asminputs:
  /* empty */           { ([], ([],[])) }
| COLON asmoperands asmclobber
                        { ($2, $3) }
;
asmopname:
    /* empty */                         { None }
|   LBRACKET IDENT RBRACKET             { Some $2 }
;

asmclobber:
    /* empty */                         { [],[] }
| COLON asmlabels                       { [],$2 }
| COLON asmcloberlst_ne asmlabels       { $2,$3 }
;
asmcloberlst_ne:
   one_string_constant                  { [$1] }
|  one_string_constant COMMA asmcloberlst_ne     { $1 :: $3 }
;
asmlabels:
| /* empty */                          { [] }
| COLON local_label_names              { $2 }
%%
