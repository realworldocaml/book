/***************************************************************************

Parser for JavaCard source files

VerifiCard Project - Démons research team - LRI - Université Paris XI

$Id: java.mly,v 1.1 2005/08/23 11:15:13 fpottier Exp $

***************************************************************************/

%{

  open Location
  open Ast_types
  open Ast

  let rec build_array_type t n =
    if n=0 then t else Array_type_expr(build_array_type t (pred n))

  let rec build_array_creation_expr t (l,n) =
    match l with
      | [] ->
	  Implicit_array_creation(build_array_type t n)
      | a::b ->
	  Explicit_array_creation(a,(build_array_creation_expr t (b,n)))

  let extend_loc l =
    Location.extend_loc !Java_parser_base.cur_base l (symbol_end())

%}

/*s Start symbols */

%start compilation_unit
%type  <Ast_types.compilation_unit> compilation_unit

%type <Ast_types.qualified_ident> name

/*s Tokens */

/* Literals */

%token <Location.t * string> ID
%token <Location.t * int> INTEGER
%token <Location.t * Why.float_repr> REAL
%token <Location.t * string> STRING
%token <Location.t * char> CHARACTER
%token <Location.t> TRUE FALSE NULL THIS

/* Keywords */

%token <Location.t> NEW SUPER
%token ABSTRACT BOOLEAN BYTE BYVALUE CASE CAST CATCH
%token CHAR CLASS CONST DEFAULT DOUBLE ELSE EXTENDS
%token FINAL FINALLY FLOAT FUTURE GENERIC GOTO
%token IMPLEMENTS IMPORT INNER INSTANCEOF INT INTERFACE LONG
%token NATIVE OPERATOR OUTER PACKAGE PRIVATE PROTECTED
%token PUBLIC REST SHORT STATIC
%token THROWS TRANSIENT VAR VOID VOLATILE
%token <Location.t> WHILE DO FOR IF SWITCH BREAK CONTINUE RETURN TRY SYNCHRONIZED THROW

/* Others symbols */

%token <Location.t> LEFTPAR
%token RIGHTPAR LEFTBRACE RIGHTBRACE LEFTBRACKET RIGHTBRACKET
%token SEMICOLON COLON COMMA QUESTIONMARK DOT
%token <string> DOC_COMMENT
%token <Ast_types.jml_declaration list> JML_DECLARATIONS
%token <Ast_types.method_specification> JML_METHOD_SPECIFICATION
%token <Ast_types.loop_annotation> JML_LOOP_ANNOTATION
%token <Ast_types.statement_specification> JML_STATEMENT_SPECIFICATION
%token <Ast_types.expr> JML_ASSERTION
%token JML_PURE JML_SPEC_PUBLIC
%token EOF

/* Operators (see precedences below for details) */

%token <string> ASSIGNOP
%token EQ
%token VERTICALBARVERTICALBAR
%token AMPERSANDAMPERSAND
%token VERTICALBAR
%token CARET
%token AMPERSAND
%token <string> EQOP
%token <string> COMP
%token <string> SHIFT
%token <Location.t> PLUS MINUS
%token STAR SLASH PERCENT
%token <Location.t> PLUSPLUS MINUSMINUS TILDA BANG

/*s Operator precedences */

%nonassoc THEN
%nonassoc ELSE

%right EQ ASSIGNOP
  /*r ["="], ["*="],  ["/="], ["%="], ["+="], ["-="], ["<<="], [">>="],
  [">>>="], ["&="], ["^="] and ["|="] */
%right IFEXPR QUESTIONMARK     /*r [" ? : "] */
%left VERTICALBARVERTICALBAR  /*r conditional OR ["||"] */
%left AMPERSANDAMPERSAND  /*r conditional AND ["&&"] */
%left VERTICALBAR         /*r bitwise or boolean OR ["|"] */
%left CARET               /*r bitwise or boolean XOR ["^"] */
%left AMPERSAND           /*r bitwise or boolean AND ["&"] */
%left EQOP                /*r ["=="] and ["!="] */
%left COMP INSTANCEOF  /*r ["<"], ["<="], [">"], [">="] and ["instanceof"] */
%left SHIFT                 /*r ["<<"], [">>"] and [">>>"] */
%left PLUS MINUS             /*r ["+"] and ["-"] */
%left STAR SLASH PERCENT     /*r ["*"], ["/"] and ["%"] */
%right UMINUS UPLUS PLUSPLUS MINUSMINUS TILDA BANG CAST
           /*r unary ["+"], ["-"], ["++"], ["--"], ["~"], ["!"] and cast */


%%

/*s Compilation units */

compilation_unit:
| package_declaration import_declarations type_declarations EOF
    { { cu_package = $1 ;
	cu_imports = $2 ;
	cu_type_decls = $3} }
;

package_declaration:
| /* $\varepsilon$ */
    { [] }
| PACKAGE name SEMICOLON
    { $2 }
;

import_declarations:
| /* $\varepsilon$ */
    { [] }
| IMPORT import_declaration SEMICOLON import_declarations
    { $2::$4 }
;


import_declaration:
| name DOT STAR
    { Import_package($1) }
| name
    { Import_class_or_interface($1) }
;

/*s type declarations */

type_declarations:
| /* $\varepsilon$ */
    { [] }
| type_declaration type_declarations
    { $1::$2 }
|  SEMICOLON type_declarations
    { $2 }
;

doc_comment:
| /* $\varepsilon$ */
    { "" }
| DOC_COMMENT
    { $1 }
;

jml_loop_annot:
| /* $\varepsilon$ */
    { default_loop_annotation }
| JML_LOOP_ANNOTATION
    { $1 }
;

type_declaration:
| doc_comment class_declaration
    { ($1,Class($2)) }
| doc_comment interface_declaration
    { ($1,Interface($2)) }
;

/*s Class declarations */

class_declaration:
| modifiers CLASS ident extends_decl implements_decl
    LEFTBRACE field_declarations RIGHTBRACE
    { { class_modifiers = $1;
	class_name = $3;
	class_extends = $4;
	class_implements = $5;
	class_fields = $7 }}
;

extends_decl:
| /* $\varepsilon$ */
    { None }
| EXTENDS name
    { Some $2 }
;

implements_decl:
| /* $\varepsilon$ */
    { [] }
| IMPLEMENTS name_comma_list
    { $2 }
;

field_declarations:
| /* $\varepsilon$ */
    { [] }
| field_declaration field_declarations
    { $1::$2 }
;

field_declaration:
| doc_comment method_declaration
    { ($1,$2) }
| doc_comment constructor_declaration
    { ($1,Constructor($2)) }
| doc_comment variable_declaration
    { ($1,Variable($2)) }
| doc_comment static_initializer
    { ($1,Static_initializer($2)) }
| doc_comment jml_declaration
    { ($1,Specification($2)) }
;

/*s JML declarations */

jml_declaration:
| JML_DECLARATIONS
    { $1 }
;
/*s variable declarations */

variable_declaration:
| modifiers type_expr variable_declarators SEMICOLON
    { { variable_modifiers = $1 ;
	variable_type = $2 ;
	variable_decls = $3 } }
;

variable_declarators:
| variable_declarator
    { [$1] }
| variable_declarator COMMA variable_declarators
    { $1::$3 }
;

variable_declarator:
| variable_declarator_id
    { { variable_id = $1 ;
	variable_initializer = None } }
| variable_declarator_id EQ variable_initializer
    { { variable_id = $1 ;
	variable_initializer = Some $3 } }
;

variable_declarator_id:
| ident
    { let (loc,id)=$1 in Simple_id(loc,id) }
| variable_declarator_id LEFTBRACKET RIGHTBRACKET
    { Array_id($1) }

variable_initializer:
| expr
    { Simple_initializer($1) }
| LEFTBRACE variable_initializers RIGHTBRACE
    { Array_initializer($2) }
;

variable_initializers:
| /* $\varepsilon$ */
    { [] }
| variable_initializer
    { [$1] }
| variable_initializer COMMA variable_initializers
    { $1::$3 }
;


/*s static initializers */

static_initializer:
| STATIC block
    { $2 }
;


/*s method declarations */

method_declaration:
| method_header method_body
    { Method($1,$2) }
;

method_header:
| method_specification modifiers type_expr method_declarator
  throws_decl
    { { method_specification = $1 ;
	method_modifiers = $2 ;
	method_return_type = Some($3) ;
	method_declarator = $4 ;
	method_throws = $5 } }
| modifiers type_expr method_declarator
  throws_decl
    { { method_specification = default_method_specification ;
	method_modifiers = $1 ;
	method_return_type = Some($2) ;
	method_declarator = $3 ;
	method_throws = $4 } }
| method_specification modifiers VOID method_declarator
  throws_decl
    { { method_specification = $1 ;
	method_modifiers = $2 ;
	method_return_type = None ;
	method_declarator = $4 ;
	method_throws = $5 } }
| modifiers VOID method_declarator
  throws_decl
    { { method_specification = default_method_specification ;
	method_modifiers = $1 ;
	method_return_type = None ;
	method_declarator = $3 ;
	method_throws = $4 } }
;

method_specification:
| JML_METHOD_SPECIFICATION
    { $1 }
| JML_METHOD_SPECIFICATION method_specification
    { concat_specs $1 $2 }
;

method_declarator:
| ident method_parameters
    { Simple_method_declarator($1,$2) }
| method_declarator LEFTBRACKET RIGHTBRACKET
    { Array_method_declarator($1) }
;


method_parameters:
| LEFTPAR RIGHTPAR
    { [] }
| LEFTPAR parameter_comma_list RIGHTPAR
    { $2 }
;

parameter_comma_list:
| parameter
    { [$1] }
| parameter COMMA parameter_comma_list
    { $1::$3 }
;

parameter:
| type_expr ident
    { Simple_parameter($1,$2) }
| parameter LEFTBRACKET RIGHTBRACKET
    { Array_parameter($1) }

throws_decl:
| /* $\varepsilon$ */
    { [] }
| THROWS name_comma_list
    { $2 }
;

method_body:
| block
    { Some($1) }
| SEMICOLON
    { None }
;

/*s constructor declarations */

constructor_declaration:
| method_specification modifiers ident method_parameters
  throws_decl constructor_body
    { { constr_specification = $1 ;
	constr_modifiers = $2 ;
	constr_name = $3 ;
	constr_parameters = $4 ;
	constr_throws = $5 ;
	constr_body = $6 } }
| modifiers ident method_parameters
  throws_decl constructor_body
    { { constr_specification = default_method_specification ;
	constr_modifiers = $1 ;
	constr_name = $2 ;
	constr_parameters = $3 ;
	constr_throws = $4 ;
	constr_body = $5 } }
;

constructor_body:
| LEFTBRACE explicit_constructor_invocation statements RIGHTBRACE
    { ($2,$3) }
| LEFTBRACE statements RIGHTBRACE
    { (Invoke_none,$2) }
| SEMICOLON
    { (Invoke_none,[]) }
/*
| LEFTBRACE explicit_constructor_invocation RIGHTBRACE
    { ($2,[]) }
| LEFTBRACE RIGHTBRACE
    { (Invoke_none,[]) }
;
*/

explicit_constructor_invocation:
| THIS LEFTPAR argument_list RIGHTPAR SEMICOLON
    { Invoke_this($3) }
| SUPER LEFTPAR argument_list RIGHTPAR SEMICOLON
    { Invoke_super($3) }
;

argument_list:
| /* $\varepsilon$ */
    { [] }
| expr_comma_list
    { $1 }
;

/*s interface declarations */

interface_declaration:
| modifiers INTERFACE ident extends_interfaces_decl
    LEFTBRACE interface_member_declarations RIGHTBRACE
    { { interface_modifiers = $1;
	interface_name = $3;
	interface_extends = $4;
	interface_members = $6 }}
;

extends_interfaces_decl:
| /* $\varepsilon$ */
    { [] }
| EXTENDS name_comma_list
    { $2 }
;

interface_member_declarations:
| /* $\varepsilon$ */
    { [] }
| interface_member_declaration interface_member_declarations
    { $1::$2 }
;

interface_member_declaration:
| doc_comment variable_declaration
    { ($1,Constant_declaration($2)) }
| doc_comment method_header SEMICOLON
    { ($1,Method_header($2)) }
;




/*s type expressions */

base_type:
| SHORT
    { "short" }
| BOOLEAN
    { "boolean" }
| BYTE
    { "byte" }
| CHAR
    { "char" }
| INT
    { "int" }
| FLOAT
    { "float" }
| LONG
    { "long" }
| DOUBLE
    { "double" }
;

type_expr:
| name
    { Type_name($1) }
| base_type
    { Base_type($1) }
| array_type_expr
    { Array_type_expr($1) }
;

array_type_expr:
| base_type LEFTBRACKET RIGHTBRACKET
    { Base_type($1) }
| name LEFTBRACKET RIGHTBRACKET
    { Type_name($1) }
| array_type_expr LEFTBRACKET RIGHTBRACKET
    { Array_type_expr($1) }
;


/*s modifiers */

modifiers:
| /* $\varepsilon$ */
    { [] }
/*
| modifier
    { [$1] }
*/
| modifier modifiers
    { $1::$2 }
;
modifier:
| STATIC
    { `STATIC }
| FINAL
    { `FINAL }
| PUBLIC
    { `PUBLIC }
| PRIVATE
    { `PRIVATE }
| PROTECTED
    { `PROTECTED }
| NATIVE
    { `NATIVE }
| SYNCHRONIZED
    { `SYNCHRONIZED }
| ABSTRACT
    { `ABSTRACT }
/* "threadsafe" ? */
| TRANSIENT
    { `TRANSIENT }
| JML_PURE
    { `PURE }
| JML_SPEC_PUBLIC
    { `SPEC_PUBLIC }
;

/*s Statements */

block:
| LEFTBRACE statements RIGHTBRACE
    { $2 }

statements:
| statement statements
    { $1::$2 }
| /* $\varepsilon$ */
    { [] }
;

/*s Statements */

local_variable_declaration:
| type_expr variable_declarators
    { { variable_modifiers = [] ;
	variable_type = $1 ;
	variable_decls = $2 } }
;

statement:
| annotated_statement { $1 }
| jml_loop_annot WHILE LEFTPAR expr RIGHTPAR statement %prec WHILE
    { mk_statement (extend_loc $2) (While($1,$4,$6)) }
| jml_loop_annot DO statement WHILE LEFTPAR expr RIGHTPAR
    { mk_statement (extend_loc $2) (Do($1,$3,$6)) }
| jml_loop_annot FOR LEFTPAR statement_expr_list SEMICOLON for_cond SEMICOLON
	statement_expr_list RIGHTPAR statement
    { mk_statement (extend_loc $2) (For($1,$4,$6,$8,$10)) }
| jml_loop_annot FOR LEFTPAR local_variable_declaration SEMICOLON for_cond SEMICOLON
	statement_expr_list RIGHTPAR statement
    { mk_statement (extend_loc $2) (For_decl($1,$4,$6,$8,$10)) }
| JML_ASSERTION
    { mk_statement_no_loc (Jml_assert($1)) }
;

annotated_statement:
| JML_METHOD_SPECIFICATION other_statement
    { mk_statement $2.statement_loc
	(Annotated(to_statement_spec $1,$2)) }
| other_statement
    { $1 }

other_statement:
| expr SEMICOLON
    { mk_statement $1.expr_loc (Expr($1)) }
| SEMICOLON
    { mk_statement_no_loc Skip }
| local_variable_declaration SEMICOLON
    { mk_statement_no_loc (Var_decl($1)) }
| IF LEFTPAR expr RIGHTPAR statement %prec THEN
    { mk_statement (extend_loc $1)
	(If_statement($3,$5,mk_statement_no_loc Skip)) }
| IF LEFTPAR expr RIGHTPAR statement ELSE statement %prec ELSE
    { mk_statement (extend_loc $1)
	(If_statement($3,$5,$7)) }
| SWITCH LEFTPAR expr RIGHTPAR LEFTBRACE switch_block RIGHTBRACE
    { mk_statement (extend_loc $1)
	(Switch($3,$6)) }
| block
    { make_block($1) }
| ident COLON statement
    { mk_statement (extend_loc (fst $1)) (Label(snd $1,$3)) }
| BREAK SEMICOLON
    { mk_statement (extend_loc $1) (Break("")) }
| BREAK ident SEMICOLON
    { mk_statement (extend_loc $1) (Break(snd $2)) }
| CONTINUE SEMICOLON
    { mk_statement (extend_loc $1) (Continue("")) }
| CONTINUE ident SEMICOLON
    { mk_statement (extend_loc $1) (Continue(snd $2)) }
| RETURN SEMICOLON
    { mk_statement (extend_loc $1) (Return(None)) }
| RETURN expr SEMICOLON
    { mk_statement (extend_loc $1) (Return(Some($2))) }
| THROW expr SEMICOLON
    { mk_statement (extend_loc $1) (Throw($2)) }
| TRY block catch_clauses
    { mk_statement (extend_loc $1) (Try($2,$3,None)) }
| TRY block catch_clauses FINALLY block
    { mk_statement (extend_loc $1) (Try($2,$3,Some($5))) }
| TRY block FINALLY block
    { mk_statement (extend_loc $1) (Try($2,[],Some($4))) }
| SYNCHRONIZED LEFTPAR expr RIGHTPAR block
    { mk_statement (extend_loc $1) (Synchronized($3,$5)) }

/*
for_init:
| statement_expr_list
    { $1 }
| local_variable_declaration
    { [Var_decl($1)] }
;
*/

for_cond:
| expr
    { $1 }
| /* $\varepsilon$ */
    { mk_expr_no_loc (Lit(Bool(true))) }
;

switch_block:
| /* $\varepsilon$ */
    { [] }
| switch_labels
    { [($1,[])] }
| switch_labels statement statements switch_block
    { ($1,$2::$3)::$4 }
;

switch_labels:
| switch_label
    { [$1] }
| switch_label switch_labels
    { $1::$2 }
;

switch_label:
| CASE expr COLON
    { Case($2) }
| DEFAULT COLON
    { Default }
;

catch_clauses:
| catch_clause
    { [$1] }
| catch_clause catch_clauses
    { $1::$2 }
;

catch_clause:
| CATCH LEFTPAR parameter RIGHTPAR block
    { ($3,$5) }
;

/*s Expressions */

field_access:
| SUPER DOT ident
    { let (l,_) as id = $3 in
      (merge_locs $1 l, Super_access(id)) }
| primary_expr DOT ident
    { let e = $1 and (l,_) as id = $3 in
      (merge_locs e.expr_loc l,Primary_access(e,id)) }
;

primary_expr:
| primary_no_new_array
    { $1 }
| array_creation_expression
    { $1 }
;

primary_no_new_array:
| INTEGER
    { let (loc,n)=$1 in mk_lit loc (Int(n)) }
| REAL
    { let (loc,r)=$1 in mk_lit loc (Float(r)) }
| TRUE
    { let loc=$1 in mk_lit loc (Bool(true)) }
| FALSE
    { let loc=$1 in mk_lit loc (Bool(false)) }
| STRING
    { let (loc,n)=$1 in mk_lit loc (String(n)) }
| NULL
    { let loc=$1 in mk_lit loc Null }
| CHARACTER
    { let (loc,n)=$1 in mk_lit loc (Char(n)) }
| THIS
    { let loc=$1 in mk_expr loc This }
| LEFTPAR expr_no_name RIGHTPAR
    { $2 }
| LEFTPAR name RIGHTPAR
    { expand_name $2 }
| field_access
    { let (loc,fa)=$1 in mk_expr loc (Field_access(fa)) }
| ident LEFTPAR argument_list RIGHTPAR
    { let (l,_) as id = $1 in
      mk_expr
	(extend_loc l)
	(Method_call(None,id,$3)) }
| name DOT ident LEFTPAR argument_list RIGHTPAR
    { let n = expand_name $1 in
      mk_expr
	(extend_loc n.expr_loc)
	(Method_call(Some n,$3,$5)) }
| primary_expr DOT ident LEFTPAR argument_list RIGHTPAR
    { let e = $1 in
      mk_expr
	(extend_loc e.expr_loc)
	(Method_call(Some e,$3,$5)) }
| NEW name LEFTPAR argument_list RIGHTPAR
    { let l = $1 in
      mk_expr
	(extend_loc l)
	(Class_instance_creation($2,$4)) }
| array_access
    { let (l,a,b)=$1 in
      mk_expr l (Array_access(a,b)) }
;

array_access:
| primary_no_new_array LEFTBRACKET expr RIGHTBRACKET
    { let e=$1 in
      (extend_loc e.expr_loc,e,$3) }
| name LEFTBRACKET expr RIGHTBRACKET
    { let e = expand_name $1 in
      (extend_loc e.expr_loc,e,$3) }
;

array_creation_expression:
| NEW base_type array_dims
    { let l = $1 in
      mk_expr
	(extend_loc l)
	(Array_creation(build_array_creation_expr (Base_type($2)) $3)) }
| NEW name array_dims
    { let l = $1 in
      mk_expr
	(extend_loc l)
	(Array_creation(build_array_creation_expr (Type_name($2)) $3)) }
;

array_dims:
| LEFTBRACKET expr RIGHTBRACKET implicit_dims
    { ([$2],$4) }
| LEFTBRACKET expr RIGHTBRACKET array_dims
    { let (a,b) = $4 in ($2::a,b) }
;

implicit_dims:
| /* $\varepsilon$$ */
    { 0 }
| LEFTBRACKET RIGHTBRACKET implicit_dims
    { succ $3 }
;

castable_expr:
| primary_expr
    { $1 }
| name
    { expand_name $1 }
| non_basic_cast
    { $1 }

non_basic_cast:
| LEFTPAR array_type_expr RIGHTPAR castable_expr %prec CAST
    { let l=$1 and e=$4 in
      mk_expr
	(merge_locs l e.expr_loc)
	(Cast(Array_type_expr($2),e)) }
| LEFTPAR name RIGHTPAR castable_expr %prec CAST
    { let l=$1 and e=$4 in
      mk_expr
	(merge_locs l e.expr_loc)
	(Cast(Type_name($2),e)) }
;

/*
statement_expr:
| expr
    { Expr($1) }
;
*/

statement_expr_list:
| /* $\varepsilon$ */
    { [] }
| statement_expr_ne_list
    { $1 }
;

statement_expr_ne_list:
| expr
    { [mk_statement $1.expr_loc (Expr($1))] }
| expr COMMA statement_expr_ne_list
    { (mk_statement $1.expr_loc (Expr($1)))::$3 }
;



expr:
| name
    { expand_name $1 }
| expr_no_name
    { $1 }
;

expr_no_name:
| name assign_op expr %prec ASSIGNOP
    { let n = $1 in
      let e = expand_name n in
      mk_expr
	(extend_loc e.expr_loc)
	(Assign_name(n,$2,$3)) }
| field_access assign_op expr %prec ASSIGNOP
    { let (l,f) = $1 in
      mk_expr
	(extend_loc l)
	(Assign_field(f,$2,$3)) }
| array_access assign_op expr %prec ASSIGNOP
    { let (l,a,b)=$1 in
      mk_expr
	(extend_loc l)
	(Assign_array(a,b,$2,$3)) }
| PLUSPLUS expr
    { let l=$1 and e=$2 in
      mk_prepost (merge_locs l e.expr_loc) `PREINCR e }
| MINUSMINUS expr
    { let l=$1 and e=$2 in
      mk_prepost (merge_locs l e.expr_loc) `PREDECR e }
| expr PLUSPLUS
    { let e = $1 and l=$2 in
      mk_prepost (merge_locs e.expr_loc l) `POSTINCR e }
| expr MINUSMINUS
    { let e = $1 and l=$2 in
      mk_prepost (merge_locs e.expr_loc l) `POSTDECR e }
| primary_expr
    { $1 }
| expr QUESTIONMARK expr COLON expr %prec IFEXPR
    { let e1=$1 and e3=$5 in
      mk_expr
	(merge_locs e1.expr_loc e3.expr_loc)
	(If_expr(e1,$3,e3)) }
| expr VERTICALBARVERTICALBAR expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "||" e2 }
| expr AMPERSANDAMPERSAND expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "&&" e2 }
| expr VERTICALBAR expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "|" e2 }
| expr CARET expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "^" e2 }
| expr AMPERSAND expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "&" e2 }
| expr EQOP expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 $2 e2 }
| expr COMP expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 $2 e2 }
| expr SHIFT expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 $2 e2  }
| expr PLUS expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "+" e2 }
| expr MINUS expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "-" e2 }
| expr STAR expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "*" e2 }
| expr SLASH expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "/" e2 }
| expr PERCENT expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "%" e2 }
| PLUS expr %prec UPLUS
    { let l = $1 and e=$2 in
      mk_un (merge_locs l e.expr_loc) "+" e }
| MINUS expr %prec UMINUS
    { let l = $1 and e=$2 in
      mk_un (merge_locs l e.expr_loc) "-" e }
| BANG expr
    { let l = $1 and e=$2 in
      mk_un (merge_locs l e.expr_loc) "!" e }
| TILDA expr
    { let l = $1 and e=$2 in
      mk_un (merge_locs l e.expr_loc) "~" e }
/*

  CAST expressions

  we distinguish cast types because of syntax ambiguities:
  is (id1)-id2  a cast of a unary minus, or a binary - ?

  solution:

  if id1 is a base type, it is a cast else it is a binary operation.
  it is enough because result of unary - cannot be casted to something
  else than a base type.

  moreover, we distinguish between cast to a type identifier
  "(name) expr" and a complex type expr, because of LALR constraint:
  (name) can be both an expr and a cast, so it is factorised.

*/
| LEFTPAR base_type RIGHTPAR expr %prec CAST
    { let l=$1 and e=$4 in
      mk_expr
	(merge_locs l e.expr_loc)
	(Cast(Base_type($2),e)) }
| non_basic_cast
    { $1 }
/*
  instanceof operator
*/
| expr INSTANCEOF type_expr
    { let e=$1 in
      mk_expr
	(extend_loc e.expr_loc)
	(Instanceof(e,$3)) }
;

expr_comma_list:
| expr
    { [$1] }
| expr COMMA expr_comma_list
    { $1::$3 }
;

assign_op:
| EQ
    { "=" }
| ASSIGNOP
    { $1 }
;

/*s identifiers */


name:
| ident
    { [$1] }
| name DOT ident
    { $3::$1 }
;

name_comma_list:
| name
    { [$1] }
| name COMMA name_comma_list
    { $1::$3 }
;

ident:
| ID
    { $1 }
;
