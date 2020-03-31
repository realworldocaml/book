/*********************************************************************/
/*                                                                   */
/*                         Dependent ML                              */
/*                                                                   */
/*                       (c) Hongwei Xi                              */
/*                           July 2000                               */
/*                                                                   */
/*                     University of Cincinnati                      */
/*                                                                   */
/*                Distributed by permission only.                    */
/*                                                                   */
/*********************************************************************/
/* dmlparser.mly: the parser definition for DML */
%{

open Dmlsyn

%}

%token <string> IDENT /* begin with a lower letter */
%token <char> CONSTCHAR
%token <float> CONSTFLOAT
%token <int> CONSTINT
%token <string> CONSTSTRING
%token EOF

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token LT GT LTEQ GTEQ LTGT
%token EQ BANGEQ
%token AMPERAMPER BARBAR PERCENT LAND LOR
%token PLUS MINUS TIMES DIV
%token QUOTE COLON COMMA DOT SEMICOLON TILDE
%token AMPER BAR COLONCOLON
%token LTMINUS MINUSGT EQGT
%token APPEND AND AS BOOL BREAK CARET CASE CHAR CONTINUE DATATYPE
%token ELSE END FALSE FN FUN IF IN INT NAT LET OF OP
%token RETURN SORT STRING THEN TRUE TYPE UNDERSCORE UNIT
%token VAL WITH WITHTYPE

%left COMMA
%right EQ
%right prec_if
%right prec_case
%right MINUSGT
%right prec_exp_lop
%left BARBAR
%left AMPERAMPER
%left prec_ind_bop1
%left prec_exp_bop1
%left LT GT LTEQ GTEQ
%left LTDOT GTDOT LTEQDOT GTEQDOT
%left EQEQ LTGT
%left prec_ind_bop2
%left prec_exp_bop2
%left PLUS MINUS
%left prec_ind_aop1
%left prec_exp_aop1
%left TIMES DIV PERCENT
%left prec_ind_aop2
%left prec_exp_aop2
%left DOT
%right prec_exi
%right PLUSPLUS MINUSMINUS BANG
%left BAR
%left AMPER
%left CARET
%left LOR
%left LAND

%type <Dmlsyn.decl list> top

%start top

%%

top:
    decls EOF				{ $1 }
;

decls:
    decl decls				{ $1 :: $2 }
  | decl				{ [$1] }
;

decl:
    FUN fun_decls			{ Fdecl $2 }
  | VAL var_typ_decl			{ VTdecl $2 }
  | VAL val_decls			{ Vdecl $2 }
  | SORT isort_decl			{ Sdecl $2 }
  | DATATYPE union_decls		{ Udecl $2 }
;

ident:
    IDENT				{ $1 }
;

constant:
    TRUE				{ Cboo true }
  | FALSE				{ Cboo false}
  | CONSTCHAR				{ Ccha $1 }
  | CONSTFLOAT				{ Cflo $1 }
  | CONSTINT				{ Cint $1 }
  | CONSTSTRING				{ Cstr $1 }

;

opname:
    OP PLUS				{ "+" }
  | OP MINUS				{ "-" }
  | OP TIMES				{ "*" }
  | OP DIV				{ "/" }
  | OP PERCENT				{ "%" }
  | OP GT				{ ">" }
  | OP GTEQ				{ ">=" }
  | OP LT				{ "<" }
  | OP LTEQ				{ "<=" }
  | OP EQ				{ "=" }
  | OP LTGT				{ "<>" }
  | OP AMPERAMPER			{ "&&" }
  | OP BARBAR				{ "||" }
  | OP APPEND				{ "@" }

var_typ_decl:
    IDENT COLON	dtype			{ {var_nam = $1; var_typ = Some $3} }
  | opname COLON dtype			{ {var_nam = $1; var_typ = Some $3} }
;

val_decl:
    pattern EQ exp			{ {vd_pat = $1; vd_exp = $3} }
;

val_decls:
    val_decl AND val_decls              { $1 :: $3 }
  | val_decl                            { [$1] }
;

fun_args:
    simple_pattern fun_args		{ $1 :: $2 }
  | simple_pattern			{ [$1] }
;

fun_decl_clause:
    IDENT fun_args EQ exp		{ ($1, ($2, $4)) }
;

fun_decl_clauses:
    fun_decl_clause BAR fun_decl_clauses
					{ $1 :: $3 }
  | fun_decl_clause			{ [$1] }
;

owithtype:
    WITHTYPE dtype			{ Some $2 }
  | /* empty */				{ None }
;

fun_decl:
    tvar_para fun_decl_clauses owithtype
					{ mk_function_d $1 $2 $3 }
;

fun_decls:
    fun_decl AND fun_decls		{ $1 :: $3 }
  | fun_decl				{ [$1] }

isort_decl:
    IDENT EQ isort			{ { sd_nam = $1; sd_def = $3 } }
;

union_decl:
     tvar_para IDENT ud_sorts EQ union_field_decls
					{ { ud_nam = $2;
				            ud_tvs = $1;
				            ud_sts = $3;
					    ud_fds = $5 } }
;

union_decls:
    union_decl AND union_decls		{ $1 :: $3 }
  | union_decl				{ [$1] }


open_tvar_para:
    tvar RPAREN				{ [TDvar (to_btv $1)] }
  | tvar COMMA open_tvar_para		{ TDvar (to_btv $1) :: $3 }
;

tvar_para:
    LPAREN open_tvar_para		{ $2 }
  | tvar				{ [TDvar (to_btv $1)] }
  | /* empty */				{ [] }
;

ud_sorts:
    WITH isort				{ [$2] }
  | WITH LPAREN open_isort_list   	{ $3 }
  | /* empty */				{ [] }
;

open_isort_list:
    isort RPAREN			{ [$1] }
  | isort COMMA open_isort_list		{ $1 :: $3 }
;

open_tvar_decls:
    tvar RPAREN				{ [TDvar (to_btv $1)] }
  | tvar COMMA open_tvar_decls		{ TDvar (to_btv $1) :: $3 }
;

tvar_decls:
    LPAREN open_tvar_decls		{ $2 }
;

tvar: QUOTE IDENT			{ $2 }
;

ivar_decl:
    ident COLON isort			{ IDvar (to_ubiv $1 $3) }
  | iprop				{ IDprop $1 }

ivar_decls:
    ivar_decl COMMA ivar_decls		{ $1 :: $3 }
  | ivar_decl BAR ivar_decls		{ $1 :: $3 }
  | ivar_decl 				{ [$1] }
;

uni_ivar_decls:
    LBRACE ivar_decls RBRACE		{ $2 }

exi_ivar_decls:
    LBRACKET ivar_decls RBRACKET	{ $2 }

ind_bop1:
    LAND				{ INFop "/\\" }
  | LOR					{ INFop "\\/" }
;

ind_bop2:
    LT					{ INFop "<" }
  | LTEQ				{ INFop "<=" }
  | GT					{ INFop ">" }
  | GTEQ				{ INFop ">=" }
  | EQ					{ INFop "=" }
  | LTGT				{ INFop "<>" }
;

ind_aop1:
    PLUS				{ INFop "+" }
  | MINUS				{ INFop "-" }
  | BARBAR				{ INFop "||" }
;

ind_aop2:
    TIMES				{ INFop "*" }
  | DIV					{ INFop "/" }
  | PERCENT				{ INFop "mod" }
  | AMPERAMPER				{ INFop "&&" }
;

simple_index:
    ident				{ Ivar $1 }
  | CONSTINT				{ Iint $1 }
  | MINUS simple_index                  { Ifun("uminus", [$2]) }
  | ident LPAREN open_comma_index_list  { Ifun($1, $3) }
  | LPAREN index RPAREN			{ $2 }
;

int_index:
    simple_index			{ $1 }
  | int_index ind_aop1 int_index %prec prec_ind_aop1
					{ mk_infix_ind $1 $2 $3 }
  | int_index ind_aop2 int_index %prec prec_ind_aop2
					{ mk_infix_ind $1 $2 $3 }
;

bool_index_seq:
    int_index ind_bop2 int_index	{ ($2, $3, [mk_infix_ind $1 $2 $3]) }
  | bool_index_seq ind_bop2 int_index %prec prec_ind_bop2
                                        { mk_infix_bool_ind_seq $1 $2 $3 }
;

bool_index:
    bool_index_seq			{ ind_of_bool_ind_seq $1 }
  | bool_index ind_bop1 bool_index	{ mk_infix_ind $1 $2 $3 }
  | LPAREN bool_index RPAREN		{ $2 }
;

index:
    int_index				{ $1 }
  | bool_index				{ $1 }
;

open_comma_index_list:
    index COMMA open_comma_index_list	{ $1 :: $3 }
  | index RPAREN			{ [$1] }
;

iprop: index				{ ip_of_ind $1 }
;

isort:
    INT					{ ISint }
  | NAT					{ isnat }
  | IDENT				{ ISnam $1 }
  | LBRACE ident COLON isort BAR iprop RBRACE
                                        { ISsub (to_ubiv $2 $4, $6) }
;

named_dtype:
    IDENT				{ ([], $1) }
  | simple_dtype IDENT			{ ([$1], $2) }
  | LPAREN dtype open_comma_dtype_list IDENT
					{ ($2 :: $3, $4) }
;

open_comma_dtype_list:
    COMMA dtype open_comma_dtype_list
					{ $2 :: $3 }
  | COMMA dtype LPAREN	        	{ [$2] }
;

simple_dtype:
    tvar				{ mkdt (DTvar $1) }
  | BOOL				{ mkdt (DTnam([], "bool", [])) }
  | BOOL LPAREN index RPAREN		{ mkdt (DTnam([], "bool", [$3])) }
  | INT					{ mkdt (DTnam([], "int", [])) }
  | NAT					{ mkdt dtnat_dsc }
  | INT LPAREN index RPAREN		{ mkdt (DTnam([], "int", [$3])) }
  | INT LPAREN index COMMA index RPAREN	{ mkdt (dtint_lr_dsc $3 $5) }
  | INT LPAREN index COMMA index RBRACKET
					{ mkdt (dtint_lR_dsc $3 $5) }
  | INT LBRACKET index COMMA index RPAREN
					{ mkdt (dtint_Lr_dsc $3 $5) }
  | INT LBRACKET index COMMA index RBRACKET
					{ mkdt (dtint_LR_dsc $3 $5) }
  | named_dtype				{ let (dts, name) = $1
                                          in mkdt_nam (dts, name, [])}
  | named_dtype LPAREN open_comma_index_list
					{ let (dts, name) = $1
                                          in mkdt_nam (dts, name, $3) }
  | exi_ivar_decls simple_dtype %prec prec_exi
					{ mkdt (DTexi ($1, $2)) }
  | LPAREN dtype RPAREN			{ $2 }
;

tuple_dtype:
    rev_tuple_dtype			{ mkdt_tup (List.rev $1) }
;

rev_tuple_dtype: /* the list is reversed! */
    simple_dtype			{ [$1] }
  | rev_tuple_dtype TIMES simple_dtype
					{ $3 :: $1 }
;

metric:
    LT int_index_list GT	{ $2 }
  | LTGT			{ [] }
;

int_index_list:
    int_index comma_int_index_list	{ $1 :: $2 }
  | /* empty */				{ [] }
;

comma_int_index_list:
    COMMA int_index comma_int_index_list
					{ $2 :: $3 }
  | /* empty */				{ [] }
;

dtype:
    tuple_dtype				{ $1 }
  | tuple_dtype MINUSGT dtype		{ mkdt (DTfun ($1, $3)) }
  | metric EQGT dtype			{ mkdt (DTmet ($1, $3)) }
  | uni_ivar_decls dtype   	  	{ mkdt (DTuni ($1, $2)) }
  | tvar_para DOT dtype			{ mkdt (DTlam ($1, $3)) }
;

dtype_opt:
    OF dtype				{ Some $2 }
  | /* empty */				{ None }
;

union_field_decl:
    IDENT dtype_opt			{ { uf_nam = $1;
					    uf_arg = $2;
                                            uf_res = [];
                                            uf_qua = [] } }
  | IDENT LPAREN open_comma_index_list dtype_opt
					{ { uf_nam = $1;
					    uf_arg = $4;
                                            uf_res = $3;
                                            uf_qua = [] } }
  | uni_ivar_decls IDENT dtype_opt
					{ { uf_nam = $2;
					    uf_arg = $3;
                                            uf_res = [];
                                            uf_qua = $1 } }
  | uni_ivar_decls IDENT LPAREN open_comma_index_list dtype_opt
					{ { uf_nam = $2;
					    uf_arg = $5;
                                            uf_res = $4;
                                            uf_qua = $1 } }
;

union_field_decls:
    union_field_decl BAR union_field_decls
					{ $1 :: $3 }
  | union_field_decl			{ [$1] }
;

exp_bop1:
    AMPERAMPER				{ INFop "&&" }
  | BARBAR				{ INFop "||" }
;

exp_bop2:
    LT					{ INFop "<" }
  | LTEQ				{ INFop "<=" }
  | GT					{ INFop ">" }
  | GTEQ				{ INFop ">=" }
  | LTGT				{ INFop "<>" }
  | EQ					{ INFop "=" }
;

exp_aop1:
    PLUS				{ INFop "+" }
  | MINUS				{ INFop "-" }
;

exp_aop2:
    TIMES				{ INFop "*" }
  | DIV					{ INFop "/" }
  | PERCENT				{ INFop "%" }
;

exp_lop:
    COLONCOLON				{ INFop "cons" }
  | APPEND				{ INFop "@" }
;

simple_exp:
    IDENT				{ mkexp (Evar $1) }
  | constant				{ mkexp (Ecst $1) }
  | TILDE simple_exp			{ mkexp (Eapp (mkexp (Evar "uminus"),
					               $2)) }
  | LET decls IN exp END		{ mkexp (Elet ($2, $4)) }
  | LPAREN exp_list RPAREN		{ mkexp_tup $2 }
  | LPAREN exp COLON dtype RPAREN	{ mkexp (Eann ($2, $4)) }
  | LBRACKET exp_list RBRACKET		{ mkexp_list $2 }
;

app_exp:
    app_exp simple_exp			{ mkexp (Eapp ($1, $2)) }
  | simple_exp				{ $1 }

exp:
    app_exp				{ $1 }
  | exp exp_bop1 exp %prec prec_exp_bop1
                                        { mk_infix_exp $1 $2 $3 }
  | exp exp_bop2 exp %prec prec_exp_bop2
					{ mk_infix_exp $1 $2 $3 }
  | exp exp_aop1 exp %prec prec_exp_aop1
					{ mk_infix_exp $1 $2 $3 }
  | exp exp_aop2 exp %prec prec_exp_aop2
					{ mk_infix_exp $1 $2 $3 }
  | exp exp_lop exp %prec prec_exp_lop
					{ mk_infix_exp $1 $2 $3 }
  | IF exp THEN exp ELSE exp %prec prec_if
					{ mkexp_if ($2, $4, $6) }
  | CASE exp OF case_clauses		{ mkexp (Ecas ($2, $4)) }
  | FN case_clauses			{ mkexp (Efn $2) }
;

exp_list:
    exp comma_exp_list			{ $1 :: $2 }
  | /* empty */				{ [] }

comma_exp_list:
    COMMA exp comma_exp_list		{ $2 :: $3 }
  | /* empty */				{ [] }
;

pattern:
    simple_pattern			{ $1 }
  | simple_pattern COLONCOLON pattern	{ mkpat (Pcstr ("cons",
                                                        mkpat_tup [$1; $3])) }
  | IDENT simple_pattern		{ mkpat (Pcstr ($1, $2)) }
  | IDENT AS pattern			{ mkpat (Pas ($1, $3)) }
;

simple_pattern:
    IDENT				{ mkpat (Pvar $1) }
  | UNDERSCORE				{ mkpat (Pany) }
  | constant				{ mkpat (Pcst $1) }
  | LPAREN pattern_list RPAREN		{ mkpat_tup $2 }
  | LBRACKET pattern_list RBRACKET	{ mkpat_list $2 }
;

pattern_list:
    pattern comma_pattern_list		{ $1 :: $2 }
  | /* empty */				{ [] }

comma_pattern_list:
    COMMA pattern comma_pattern_list	{ $2 :: $3 }
  | /* empty */		               	{ [] }
;

case_clause:
    pattern EQGT exp %prec prec_case	{ ($1, $3) }
;

case_clauses:
    case_clause BAR case_clauses  	{ $1 :: $3 }
  | case_clause			  	{ [$1] }
;