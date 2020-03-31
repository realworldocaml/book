(* Original file: memcad.1.0.0/memcad-1.0.0/c/mc_parser.mly *)
%{
(** MemCAD analyzer
 **
 ** mc_parser.mly
 ** MemCAD command parser
 ** Xavier Rival, 2011/12/26 *)
open C_sig

(* Creation of type-less typed expressions and l-values *)
let mkve (e: c_exprk): c_expr =
  { cek = e ;
    cet = Ctvoid (* for now *) }
let mkvl (l: c_lvalk): c_lval =
  { clk = l ;
    clt = Ctvoid (* for now *) }
%}

%token T_comma T_pipe
%token T_lparen T_rparen
%token T_lbrack T_rbrack
%token T_arrow
%token T_subseteq T_setin T_equal T_setempty T_euplus
%token T_eq T_ne T_lt T_gt T_le T_ge
%token T_seg

%token <bool>      V_bool
%token <int>       V_int
%token <int>       V_size_arrow
%token <string>    V_string
%token <string>    V_astring

%token T_add_inductive
%token T_add_segment
%token T_check_segment
%token T_add_setexprs
%token T_assume
%token T_decl_setvars
%token T_check_bottomness
%token T_check_inductive
%token T_check_setexprs
%token T_force_live
%token T_kill_flow
%token T_merge
%token T_output_dot
%token T_output_stdout
%token T_reduce_eager
%token T_reduce_localize
%token T_sel_merge
%token T_unfold
%token T_unfold_bseg
%token T_unroll
%token T_array_check
%token T_array_assume
%token T_eof

%type <C_sig.c_memcad_com>             memcad_command
%type <string>                         indname
%type <string list>                    var_name_list
%type <C_sig.c_memcad_iparams option>  indparams
%type <C_sig.c_memcad_iparam list>     lparams
%type <C_sig.c_memcad_iparam>          param
%type <C_sig.c_var>                    c_var
%type <C_sig.c_lval>                   c_lval
%type <C_sig.c_var list>               c_var_list
%type <C_sig.c_memcad_setexpr>         set_expr
%type <C_sig.c_memcad_setexpr list>    set_exprs
%type <C_sig.c_num_expr list>          num_exprs
%type <C_sig.c_memcad_setvar list>     lparams_set
%type <C_sig.c_memcad_setvar list>     setvars

%start memcad_command
%%

memcad_command:
| T_add_inductive T_lparen c_lval T_comma indname indparams T_rparen
    { Mc_add_inductive ($3, Some $5, $6) }
| T_add_segment T_lparen c_lval T_comma indname indparams T_seg
    c_lval T_comma indname indparams T_rparen
    { Mc_add_segment ($3, $5, $6, $8, $10, $11 ) }
| T_check_segment T_lparen c_lval T_comma indname indparams T_seg
    c_lval T_comma indname indparams T_rparen
    { Mc_check_segment ($3, $5, $6, $8, $10, $11 ) }
| T_add_setexprs T_lparen set_exprs T_rparen
    { Mc_add_setexprs $3 }
| T_assume T_lparen num_exprs T_rparen
    { Mc_assume $3 }
| T_decl_setvars T_lparen setvars T_rparen
    { Mc_decl_setvars $3}
| T_check_setexprs T_lparen set_exprs T_rparen
    { Mc_check_setexprs $3 }
| T_check_inductive T_lparen c_lval T_rparen
    { Mc_check_inductive ($3, None, None) }
| T_check_inductive T_lparen c_lval T_comma indname indparams T_rparen
    { Mc_check_inductive ($3, Some $5, $6) }
| T_check_bottomness T_lparen V_bool T_rparen
    { Mc_check_bottomness $3 }
| T_unfold T_lparen c_lval T_rparen
    { Mc_unfold $3 }
| T_unfold_bseg T_lparen c_lval T_rparen
    { Mc_unfold_bseg $3 }
| T_unroll T_lparen V_int T_rparen
    { Mc_unroll $3 }
| T_merge T_lparen T_rparen
    { Mc_merge }
| T_output_dot T_lparen var_name_list T_pipe var_name_list T_rparen
    { Mc_output_ext (Flags.Out_dot ($3, $5)) }
| T_output_stdout T_lparen T_rparen
    { Mc_output_stdout }
| T_sel_merge T_lparen c_var_list T_rparen
    { Mc_sel_merge (List.rev $3) }
| T_force_live T_lparen c_var_list T_rparen
    { Mc_force_live (List.rev $3) }
| T_kill_flow
    { Mc_kill_flow }
| T_reduce_eager
    { Mc_reduce_eager }
| T_reduce_localize T_lparen c_lval T_rparen
    { Mc_reduce_localize $3 }
| T_array_check V_astring
    { Array_pred_sig.hint_array := $2; Mc_array_check }
| T_array_assume V_astring
    { Array_pred_sig.assume_array := $2; Mc_array_assume }

indname:
| V_string { $1 }

indparams:
| { None }
| T_comma T_lbrack lparams T_pipe lparams T_pipe lparams_set T_rbrack
      { Some { mc_pptr = $3 ;
               mc_pint = $5 ;
               mc_pset = $7 } }

lparams:
| { [ ] }
| param { [ $1 ] }
| param T_comma lparams { $1 :: $3 }

param:
| V_int    { Cmp_const $1 }
| c_lval   { Cmp_lval  $1 }

setvars:
| {[ ]}
| setvar   { [ $1 ] }
| setvar T_comma setvars { $1::$3 }

setvar:
| V_string { { mc_setvar_name = $1;
               mc_setvar_uid  = -1 } }
lparams_set:
| {[ ]}
| setvar { [$1] }
| setvar T_comma lparams_set { $1::$3 }

set_exprs:
| { [ ] }
| set_expr { [ $1 ] }
| set_expr T_comma set_exprs { $1 :: $3 }

set_expr:
| setvar T_subseteq setvar     { Cmp_subset (  $1, $3 ) }
| c_lval T_setin setvar        { Cmp_mem (   $1,  $3 ) }
| setvar T_equal T_setempty    { Cmp_empty $1 }
| setvar T_equal c_lval T_euplus setvar { Cmp_euplus ($1, $3, $5) }

num_exprs:
| { [] }
| num_expr { [$1] }
| num_expr T_comma num_exprs { ($1 :: $3) }

num_expr:
| c_lval T_eq param { (Cbeq, $1, $3) }
| c_lval T_ne param { (Cbne, $1, $3) }
| c_lval T_lt param { (Cblt, $1, $3) }
| c_lval T_gt param { (Cbgt, $1, $3) }
| c_lval T_le param { (Cble, $1, $3) }
| c_lval T_ge param { (Cbge, $1, $3) }

c_var:
| V_string { { cv_name     = $1;
               cv_uid      = -1;
               cv_type     = Ctvoid;
               cv_volatile = false } }

c_lval:
| c_var { { clk = Clvar $1 ;
            clt = Ctvoid } }
| c_lval T_arrow V_string
    { { clk = Clfield (mkvl (Clderef (mkve (Celval $1))), $3);
        clt = Ctvoid } }

c_var_list:
| { [ ] }
| c_var { [ $1 ] }
| c_var_list T_comma c_var { $3 :: $1 }

var_name_list:
| { [ ] }
| V_string { [ $1 ] }
| V_string T_comma var_name_list { $1 :: $3 }
