/***************************************************************************

Parser for Jml assertions

Krakatoa Project

(c) Démons & Logical research groups
    LRI/Université Paris XI & INRIA Futurs

$Id: jml_parser.mly,v 1.19 2005/03/08 13:36:53 marche Exp $

***************************************************************************/

%{

  open Location
  open Ast_types
  open Ast

  let extend_loc l =
    Location.extend_loc !Jml_parser_base.cur_base l (symbol_end())

%}

/*s Start symbols */

%start jml_specification_eof
%type <Ast_types.jml_specification> jml_specification_eof

/*s Tokens */

/* Literals */

%token <Location.t * string> ID
%token <Location.t * int> INTEGER
%token <Location.t * Why.float_repr> REAL
%token <Location.t * string> STRING
%token <Location.t * char> CHARACTER
%token <Location.t> TRUE FALSE NULL THIS BSRESULT BSFPI

/* Keywords */

%token <Location.t> NEW SUPER BSOLD BSFRESH
%token <Location.t> BSTYPE BSTYPEOF

%token ASSERT INVARIANT LOOP_INVARIANT
%token BEHAVIOR NORMAL_BEHAVIOR EXCEPTIONAL_BEHAVIOR ALSO AND
%token FORALL OLD MODEL GHOST
%token REQUIRES WHEN MEASURED_BY
%token SIGNALS DECREASES ENSURES ASSIGNABLE IF DIVERGES
%token BSEVERYTHING BSNOTHING BSFIELDSOF
%token BSFORALL BSEXISTS
%token PROTECTED PRIVATE PUBLIC PURE SPEC_PUBLIC FINAL STATIC
%token INSTANCEOF
%token SHORT BOOLEAN BYTE CHAR INT FLOAT LONG DOUBLE
%token REPRESENTS

/* Others symbols */

%token <Location.t> LEFTPAR
%token RIGHTPAR LEFTBRACE RIGHTBRACE LEFTBRACKET RIGHTBRACKET
%token LEFTBRACEBAR BARRIGHTBRACE
%token SEMICOLON COLON COMMA QUESTIONMARK DOT DOTDOT
%token LEFTARROW
%token EOF

/* Operators (see precedences below for details) */

%token <string> ASSIGNOP
%token EQ
%token LTEQEQGT LTEQBANGEQGT EQEQGT LTEQEQ
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
%right LTEQEQGT LTEQBANGEQGT   /*r ["<==>"] and ["<=!=>"] */
%right EQEQGT LTEQEQ           /*r ["==>"] and ["<=="] */
%left VERTICALBARVERTICALBAR  /*r conditional OR ["||"] */
%left AMPERSANDAMPERSAND  /*r conditional AND ["&&"] */
%left VERTICALBAR         /*r bitwise or boolean OR ["|"] */
%left CARET               /*r bitwise or boolean XOR ["^"] */
%left AMPERSAND           /*r bitwise or boolean AND ["&"] */
%left EQOP                /*r ["=="] and ["!="] */
%left COMP INSTANCEOF  /*r ["<"], ["<="], [">"], [">="], ["<:"] and ["instanceof"] */
%left SHIFT                 /*r ["<<"], [">>"] and [">>>"] */
%left PLUS MINUS             /*r ["+"] and ["-"] */
%left STAR SLASH PERCENT     /*r ["*"], ["/"] and ["%"] */
%right UMINUS UPLUS PLUSPLUS MINUSMINUS TILDA BANG CAST
           /*r unary ["+"], ["-"], ["++"], ["--"], ["~"], ["!"] and cast */


%%

jml_specification_eof:
| jml_specification EOF
    { $1 }
;

jml_specification:
| PURE
    { Jml_pure }
| SPEC_PUBLIC
    { Jml_spec_public }
| method_specification
    { Jml_method_specification($1) }
| jml_declarations
    { Jml_declaration($1) }
| loop_annotation
    { Jml_loop_annotation($1) }
| ASSERT expr_semicolon
    { Jml_assertion($2) }
;



loop_annotation:
| LOOP_INVARIANT expr_semicolon
  assignable_clauses
  decreases_clause
    {
      let m = $3 in
      { loop_invariant = $2 ;
	loop_modifies = if m=[] then None else Some m;
	typed_loop_modifies = None;
	loop_decreases = $4} }
;

decreases_clause:
| /* $\varepsilon$ */    { expr_zero }
| DECREASES expr_semicolon { $2 }
;

/*s method specifications.

    from "Desugaring JML specifications" */

/* top-level method specifications */

method_specification :
| non_extending_specification
    { Non_extending($1) }
| extending_specification
    { $1 }
;

extending_specification:
| ALSO additive_specification
    { Extending_also($2) }
| AND conjoinable_spec_seq
    { Extending_and($2) }
;

non_extending_specification:
| spec_case_seq
    { $1 }
;

spec_case_seq:
| spec_case
    { [$1] }
| spec_case ALSO spec_case_seq
    { $1::$3 }
;

spec_case:
| generic_spec_case
    { Generic($1) }
| behavior_spec
    { $1 }
;


additive_specification:
| spec_case_seq
    { $1 }
;

/* generic specification cases */

generic_spec_case:
| spec_var_decls spec_header generic_spec_body
    { { generic_spec_var_decls = $1 ;
	  generic_spec_header = $2 ;
	  generic_spec_body = $3 } }
;

spec_header:
| requires_clauses when_clauses measured_clauses
    { { requires_clauses = $1 ;
	when_clauses = $2 ;
	measured_clauses = $3 } }
;

generic_spec_body:
| simple_spec_body
    { Simple_spec_body($1) }
| LEFTBRACEBAR generic_spec_case_seq BARRIGHTBRACE
    { Nested_spec_body($2) }
;

generic_spec_case_seq:
| generic_spec_case
    { [$1] }
| generic_spec_case ALSO generic_spec_case_seq
    { $1::$3 }
;

simple_spec_body:
| assignable_clause assignable_clauses
    ensures_clauses signals_clauses diverges_clauses
    { { assignable_clauses = $1::$2 ;
	ensures_clauses = $3 ;
	signals_clauses = $4 ;
	diverges_clauses = $5 } }
| ensures_clause ensures_clauses signals_clauses diverges_clauses
    { { assignable_clauses = [] ;
	ensures_clauses = $1::$2 ;
	signals_clauses = $3 ;
	diverges_clauses = $4 } }
| signals_clause signals_clauses diverges_clauses
    { { assignable_clauses = [] ;
	ensures_clauses = [] ;
	signals_clauses = $1::$2 ;
	diverges_clauses = $3 } }
| diverges_clause diverges_clauses
    { { assignable_clauses = [] ;
	ensures_clauses = [] ;
	signals_clauses = [] ;
	diverges_clauses = $1::$2 } }
;

/* behavior specification */

behavior_spec:
| privacy BEHAVIOR generic_spec_case
    { Behavior($1,$3) }
| privacy EXCEPTIONAL_BEHAVIOR exceptional_spec_case
    { Exceptional_behavior($1,$3) }
| privacy NORMAL_BEHAVIOR normal_spec_case
    { Normal_behavior($1,$3) }
;

exceptional_spec_case:
| spec_var_decls spec_header exceptional_spec_body
    { { exceptional_spec_var_decls = $1 ;
	exceptional_spec_header = $2 ;
	exceptional_spec_body = $3 } }
;

privacy:
| /* $\varepsilon$ */
    { Privacy_none }
| PUBLIC
    { Privacy_public }
| PROTECTED
    { Privacy_protected }
| PRIVATE
    { Privacy_private }
;

exceptional_spec_body:
| exceptional_simple_spec_body
    { Simple_exceptional_spec_body($1) }
| LEFTBRACEBAR exceptional_spec_case_seq BARRIGHTBRACE
    { Nested_exceptional_spec_body($2) }
;

exceptional_simple_spec_body:
| assignable_clauses signals_clauses diverges_clauses
    { { exceptional_assignable_clauses = $1 ;
	exceptional_signals_clauses = $2 ;
	exceptional_diverges_clauses = $3 } }
;

exceptional_spec_case_seq:
| exceptional_spec_case
    { [$1] }
| exceptional_spec_case ALSO exceptional_spec_case_seq
    { $1::$3 }
;

normal_spec_case:
| spec_var_decls spec_header normal_spec_body
    { { normal_spec_var_decls = $1 ;
	normal_spec_header = $2 ;
	normal_spec_body = $3 } }
;

normal_spec_body:
| normal_simple_spec_body
    { Simple_normal_spec_body($1) }
| LEFTBRACEBAR normal_spec_case_seq BARRIGHTBRACE
    { Nested_normal_spec_body($2) }
;

normal_simple_spec_body:
| assignable_clauses ensures_clauses diverges_clauses
    { { normal_assignable_clauses = $1 ;
	normal_ensures_clauses = $2 ;
	normal_diverges_clauses = $3 } }
;

normal_spec_case_seq:
| normal_spec_case
    { [$1] }
| normal_spec_case ALSO normal_spec_case_seq
    { $1::$3 }
;

/* conjoinable specification syntax */

conjoinable_spec_seq:
| conjoinable_spec
    { [$1] }
| conjoinable_spec AND conjoinable_spec_seq
    { $1::$3 }
;

conjoinable_spec:
| generic_conjoinable_spec
    { Generic_conjoinable_spec($1) }
| behavior_conjoinable_spec
    { Behavior_conjoinable_spec($1) }
;

generic_conjoinable_spec:
| spec_var_decls simple_spec_body
    { { conjoinable_spec_var_decls = $1 ;
	conjoinable_spec_body = $2 } }
;

behavior_conjoinable_spec:
| privacy BEHAVIOR spec_var_decls simple_spec_body
    { Behavior($1,
	       { generic_spec_var_decls = $3 ;
		 generic_spec_header = default_spec_header ;
		 generic_spec_body = Simple_spec_body($4) } ) }
| privacy EXCEPTIONAL_BEHAVIOR spec_var_decls exceptional_simple_spec_body
    { Exceptional_behavior($1,
	       { exceptional_spec_var_decls = $3 ;
		 exceptional_spec_header = default_spec_header ;
		 exceptional_spec_body = Simple_exceptional_spec_body($4) } ) }
| privacy NORMAL_BEHAVIOR spec_var_decls normal_simple_spec_body
    { Normal_behavior($1,
	       { normal_spec_var_decls = $3 ;
		 normal_spec_header = default_spec_header ;
		 normal_spec_body = Simple_normal_spec_body($4) } ) }

/* obsolete
| modifier BEHAVIOR requires assignable_list ensures signals_list
      { { requires = $3 ; assignable = $4 ; ensures = $5 ; signals = $6 } }
*/

/*s spec clauses */

spec_var_decls:
| forall_var_decls let_var_decls
    { { forall_var_decls = $1 ;
	let_var_decls = $2 } }
;

forall_var_decls:
| /* $\varepsilon$ */
    { [] }
| forall_var_decl forall_var_decls
    { $1::$2 }
;

forall_var_decl:
| FORALL variable_declaration SEMICOLON
    { $2 }
;

let_var_decls:
| /* $\varepsilon$ */
    { [] }
| OLD local_spec_var_decls
    { $2 }
;

local_spec_var_decls:
| local_spec_var_decl
    { [$1] }
| local_spec_var_decl local_spec_var_decls
    { $1::$2 }
;

local_spec_var_decl:
| MODEL variable_declaration SEMICOLON
    { let (t,idl) = $2 in Local_spec_var_decl_model(t,idl) }
| GHOST variable_declaration SEMICOLON
    { let (t,idl)=$2 in Local_spec_var_decl_ghost(t,idl) }
;

requires_clauses:
| /* $\varepsilon$ */
    { [] }
| requires_clause requires_clauses
    { $1::$2 }
;

requires_clause:
| REQUIRES expr_semicolon
    { $2 }


when_clauses:
| /* $\varepsilon$ */
    { [] }
| when_clause when_clauses
    { $1::$2 }
;

when_clause:
| WHEN expr_semicolon
    { $2 }


measured_clauses:
| /* $\varepsilon$ */
    { [] }
| measured_clause measured_clauses
    { $1::$2 }
;

measured_clause:
| MEASURED_BY expr_semicolon
    { $2 }


assignable_clauses:
| /* $\varepsilon$ */
    { [] }
| assignable_clause assignable_clauses
    { $1::$2 }
;

assignable_clause:
| ASSIGNABLE assignable_objects SEMICOLON { $2 }
;

assignable_objects:
| assignable_entry
    { [$1] }
| assignable_entry COMMA assignable_objects
    { $1::$3 }
;

assignable_entry:
| assignable_any_object
    { ($1,None) }
| assignable_any_object IF expr
    { ($1,Some $3) }
;

assignable_any_object:
| BSEVERYTHING
    { Assignable_everything }
| BSNOTHING
    { Assignable_nothing }
| BSFIELDSOF LEFTPAR expr RIGHTPAR
    {Assignable_all_fields ($3)}
| expr
    {Assignable_set_ref ($1,[])}
| primary_no_new_array set_ref_sep set_ref_list {Assignable_set_ref ($1,$2::$3)}
| name set_ref_sep set_ref_list {let e = expand_name $1 in Assignable_set_ref (e,$2::$3)}
;

set_ref_sep:
| LEFTBRACKET expr DOTDOT expr RIGHTBRACKET
    {Set_array_interval($2,$4)}
| LEFTBRACKET STAR RIGHTBRACKET
    {Set_array}
;

set_ref:
| set_ref_sep {$1}
| LEFTBRACKET expr RIGHTBRACKET
    {Set_array_index($2)}
| DOT ident
    {Set_fieldraw($2)}
;

set_ref_list:
|  {[]}
| set_ref set_ref_list {$1::$2}
;

ensures_clauses:
| /* $\varepsilon$ */
    { [] }
| ensures_clause ensures_clauses
    { $1::$2 }
;

ensures_clause:
| ENSURES expr_semicolon
    { $2 }
;

signals_clauses:
| /* $\varepsilon$ */
    { [] }
| signals_clause signals_clauses
    { $1::$2 }
;

signals_clause:
| SIGNALS LEFTPAR type_expr ident RIGHTPAR opt_expr_semicolon
   { { signals_exception = $3 ;
       signals_variable = Some $4 ;
       signals_post = match $6 with | None -> expr_true | Some e -> e ; } }
| SIGNALS LEFTPAR type_expr RIGHTPAR opt_expr_semicolon
   { { signals_exception = $3 ;
       signals_variable = None ;
       signals_post = match $5 with | None -> expr_true | Some e -> e ; } }
;

diverges_clauses:
| /* $\varepsilon$ */
    { [] }
| diverges_clause diverges_clauses
    { $1::$2 }
;

diverges_clause:
| DIVERGES expr_semicolon
    { $2 }
;





/*s */

jml_declarations:
| /* $\varepsilon$ */
    { [] }
| jml_declaration jml_declarations
    { $1::$2 }
;

jml_declaration:
| privacy INVARIANT expr_semicolon
      { Invariant($3) }
| modifiers type_expr variable_declarators SEMICOLON
    { Jml_variable_declaration(
      { jml_variable_modifiers = $1;
	jml_variable_type = $2;
	jml_variable_decls = $3 } ) }
| REPRESENTS ident LEFTARROW expr SEMICOLON
    { Represents($2,$4) }
;

/*s modifiers */

modifiers:
| /* $\varepsilon$ */
    { [] }
| modifier modifiers
    { $1::$2 }
;

modifier:
| MODEL
    { `MODEL }
| FINAL
    { `FINAL }
| STATIC
    { `STATIC }
| PUBLIC
    { `PUBLIC }
| PRIVATE
    { `PRIVATE }
| PROTECTED
    { `PROTECTED }
;

expr_semicolon:
| expr SEMICOLON
    { $1 }
;

opt_expr_semicolon:
| expr SEMICOLON
    { Some($1) }
| SEMICOLON
    { None }
;

/*
argument_list:
| $\varepsilon$
    { [] }
| expr_comma_list
    { $1 }
;
*/

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
| BSRESULT
    { let loc=$1 in mk_expr loc Result }
| BSOLD LEFTPAR expr RIGHTPAR
    { let loc=$1 and e = $3 in
      mk_expr (extend_loc loc) (Old(e)) }
| BSFRESH LEFTPAR expr RIGHTPAR
    { let loc=$1 and e = $3 in
      mk_expr (extend_loc loc) (Fresh(e)) }
| BSTYPE LEFTPAR type_expr RIGHTPAR
    { let loc=$1 and te = $3 in
      mk_expr (extend_loc loc) (Type(te)) }
| BSTYPEOF LEFTPAR expr RIGHTPAR
    { let loc=$1 and e = $3 in
      mk_expr (extend_loc loc) (Typeof(e)) }
| BSFPI LEFTPAR expr COMMA REAL COMMA REAL RIGHTPAR
    { let loc = $1 and (_,f1) = $5 and (_,f2) = $7 in
      mk_expr (extend_loc loc) (Fpi($3,f1,f2)) }
| LEFTPAR BSFORALL variable_declaration SEMICOLON expr SEMICOLON expr RIGHTPAR
    { let loc=$1 and (t,idl)=$3 in
      mk_expr
	(extend_loc loc)
	(Quantifier(Forall,t,idl,$5,$7)) }
| LEFTPAR BSFORALL variable_declaration SEMICOLON expr RIGHTPAR
    { let loc=$1 and (t,idl)=$3 in
      mk_expr
	(extend_loc loc)
	(Quantifier(Forall,t,idl,expr_true,$5)) }
| LEFTPAR BSEXISTS variable_declaration SEMICOLON expr SEMICOLON expr RIGHTPAR
    { let loc=$1 and (t,idl)=$3 in
      mk_expr
	(extend_loc loc)
	(Quantifier(Exists,t,idl,$5,$7)) }
| LEFTPAR BSEXISTS variable_declaration SEMICOLON expr RIGHTPAR
    { let loc=$1 and (t,idl)=$3 in
      mk_expr
	(extend_loc loc)
	(Quantifier(Exists,t,idl,mk_expr_no_loc (Lit(Bool(true))),$5)) }
| LEFTPAR expr_no_name RIGHTPAR
    { $2 }
| parenthesized_name
    { expand_name $1 }
| field_access
    { let (l,f)=$1 in mk_expr l (Field_access(f)) }
| ident LEFTPAR argument_list RIGHTPAR
    { let (l,_) as id = $1 in
      mk_expr
	(extend_loc l)
	(Method_call(None,id,$3)) }
| name DOT ident LEFTPAR argument_list RIGHTPAR
    { let e = expand_name $1 in
      mk_expr
	(extend_loc e.expr_loc)
	(Method_call(Some e,$3,$5)) }
| primary_expr DOT ident LEFTPAR argument_list RIGHTPAR
    { let e = $1 in
      mk_expr
	(extend_loc e.expr_loc)
	(Method_call(Some e,$3,$5)) }
| NEW name LEFTPAR argument_list RIGHTPAR
    { let l=$1 in
      mk_expr
	(extend_loc l)
	(Class_instance_creation($2,$4)) }
| array_access
    { $1 }
;

array_access:
| primary_no_new_array LEFTBRACKET expr RIGHTBRACKET
    { let e=$1 in
      let eend = symbol_end() in
      (*i
	Format.printf "Array_access : symbol_end = %d@." eend;
	i*)
      mk_expr
	(extend_loc e.expr_loc)
	(Array_access(e,$3)) }
| name LEFTBRACKET expr RIGHTBRACKET
    { let e = expand_name $1 in
      mk_expr
	(extend_loc e.expr_loc)
	(Array_access(e,$3)) }
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

primary_expr_or_name:
| primary_expr
    { $1 }
| name
    { expand_name $1 }
;

expr:
| name
    { expand_name $1 }
| expr_no_name
    { $1 }
;

expr_no_name:
| primary_expr
    { $1 }
| expr QUESTIONMARK expr COLON expr %prec IFEXPR
    { let e1=$1 and e3=$5 in
      mk_expr
	(merge_locs e1.expr_loc e3.expr_loc)
	(If_expr(e1,$3,e3)) }
| expr EQEQGT expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "==>" e2 }
| expr LTEQEQ expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "<==" e2 }
| expr LTEQEQGT expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "<==>" e2 }
| expr LTEQBANGEQGT expr
    { let e1=$1 and e2=$3 in
      mk_bin
	(merge_locs e1.expr_loc e2.expr_loc)
	e1 "<=!=>" e2 }
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
	e1 $2 e2 }
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
| LEFTPAR array_type_expr RIGHTPAR primary_expr_or_name %prec CAST
    { let l=$1 and e=$4 in
      mk_expr
	(merge_locs l e.expr_loc)
	(Cast(Array_type_expr($2),e)) }
| LEFTPAR name RIGHTPAR primary_expr_or_name %prec CAST
    { let l=$1 and e=$4 in
      mk_expr
	(merge_locs l e.expr_loc)
	(Cast(Type_name($2),e)) }
/*
  instanceof operator
*/
| expr INSTANCEOF type_expr
    { let e=$1 in
      mk_expr
	(extend_loc e.expr_loc)
	(Instanceof(e,$3)) }
;

parenthesized_name:
| LEFTPAR name RIGHTPAR
    { $2 }
;

name:
| ident
    { [$1] }
| name DOT ident
    { $3::$1 }
;

argument_list:
| /* $\varepsilon$ */
    { [] }
| expr_comma_list
    { $1 }
;

expr_comma_list:
| expr
    { [$1] }
| expr COMMA expr_comma_list
    { $1::$3 }
;


variable_declaration:
| type_expr variable_declarators
    { ($1,$2) }
;

variable_declarators:
| variable_declarator_id
    { [$1] }
| variable_declarator_id COMMA variable_declarators
    { $1::$3 }
;

variable_declarator_id:
| ident
    { let (loc,id)=$1 in Simple_id(loc,id) }
| variable_declarator_id LEFTBRACKET RIGHTBRACKET
    { Array_id($1) }

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
| type_expr_no_name
    { $1 }
;

type_expr_no_name:
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

ident:
| ID
    { $1 }
