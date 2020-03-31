(* Original file: KaSim.4.0.0/KaSim-4.0/grammar/kappaParser.mly *)
/******************************************************************************/
/*  _  __ * The Kappa Language                                                */
/* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  */
/* | ' /  *********************************************************************/
/* | . \  * This file is distributed under the terms of the                   */
/* |_|\_\ * GNU Lesser General Public License Version 3                       */
/******************************************************************************/

%{
  let add_pos x =
    (x,Locality.of_pos (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()))
  let rhs_pos i =
  Locality.of_pos (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i)
%}

%token EOF NEWLINE SEMICOLON COMMA DOT OP_PAR CL_PAR OP_CUR CL_CUR AT TYPE LAR
%token CPUTIME EMAX TMAX PLOTENTRY DELETE INTRO TRACK DO SET REPEAT SPECIES_OF
%token UNTIL LOG PLUS MULT MINUS MAX MIN DIV SINUS COSINUS TAN POW ABS MODULO
%token SQRT EXPONENT INFINITY TIME EVENT NULL_EVENT PIPE EQUAL AND OR NOT
%token GREATER SMALLER TRUE FALSE DIFF KAPPA_RAR KAPPA_LRAR KAPPA_LNK
%token SIGNATURE INIT LET PLOT PERT OBS TOKEN CONFIG KAPPA_WLD KAPPA_SEMI
%token FLUX ASSIGN PRINTF STOP SNAPSHOT RUN THEN ELSE ALARM APPLY
%token <int> INT
%token <string> ID
%token <string> KAPPA_MRK LABEL
%token <float> FLOAT
%token <string> STRING

%left MINUS PLUS
%left MULT DIV
%left MODULO
%right POW
%nonassoc LOG SQRT EXPONENT SINUS COSINUS ABS TAN

%left OR
%left AND

%nonassoc THEN

%start start_rule
%type <Ast.parsing_compil -> Ast.parsing_compil> start_rule

%start interactive_command
%type <(Ast.mixture,Ast.mixture,string,Ast.rule) Ast.command> interactive_command

%start standalone_effect_list
%type <(Ast.mixture,Ast.mixture,string,Ast.rule) Ast.modif_expr list> standalone_effect_list

%start standalone_bool_expr
%type <(Ast.mixture,string) Alg_expr.bool Locality.annot> standalone_bool_expr

%% /*Grammar rules*/

newline:
    | NEWLINE start_rule {$2}
    | EOF {fun c -> c};

start_rule:
    | newline {$1}
    | LABEL rule_expression newline
        {let out = (Some ($1, rhs_pos 1),$2) in
	fun c -> let r = $3 c in {r with Ast.rules = out::r.Ast.rules}}
    | rule_expression newline
        {fun c -> let r = $2 c in {r with Ast.rules = (None,$1)::r.Ast.rules}}
    | LABEL EQUAL alg_expr newline
        {let out = (($1,rhs_pos 1),$3) in
	fun c -> let r = $4 c in {r with Ast.variables = out::r.Ast.variables}}
    | instruction newline
		  { fun c -> let r = $2 c in
		      match $1 with
		      | Ast.RULE ru ->
			 {r with Ast.rules = ru::r.Ast.rules}
		      | Ast.SIG ag ->
			 {r with Ast.signatures=ag::r.Ast.signatures}
		      | Ast.TOKENSIG (str_pos) ->
			 {r with Ast.tokens=str_pos::r.Ast.tokens}
		      | Ast.VOLSIG (vol_type,vol,vol_param) ->
			 {r with Ast.volumes=(vol_type,vol,vol_param)::r.Ast.volumes}
		      | Ast.INIT (alg,init_t) ->
			 {r with Ast.init=(alg,init_t)::r.Ast.init}
		      | Ast.DECLARE var ->
			 {r with Ast.variables = var::r.Ast.variables}
		      | Ast.OBS ((lbl,pos),_ as var) ->
			 (*for backward compatibility, shortcut for %var + %plot*)
			   {r with
			     Ast.variables = var::r.Ast.variables;
			     Ast.observables = (Alg_expr.ALG_VAR lbl,pos)
						 ::r.Ast.observables}
		      | Ast.PLOT expr ->
			 {r with Ast.observables = expr::r.Ast.observables}
		      | Ast.PERT ((alarm,pre,effect,opt),pos) ->
			 {r with
			  Ast.perturbations =
			   ((alarm,pre,effect,opt),pos)::r.Ast.perturbations}
		      | Ast.CONFIG (param_name,value_list) ->
			 {r with
			  Ast.configurations = (param_name,value_list)::r.Ast.configurations}
		  }
    | error
	{raise (ExceptionDefn.Syntax_Error (add_pos "Syntax error"))}
    ;

instruction:
    | SIGNATURE agent_expression {Ast.SIG $2}
    | TOKEN ID {Ast.TOKENSIG ($2,rhs_pos 2)}
    | SIGNATURE error {raise (ExceptionDefn.Syntax_Error
				(add_pos "Malformed agent signature, I was expecting something of the form '%agent: A(x,y~u~v,z)'"))}

    | INIT init_declaration {Ast.INIT $2}
    | INIT error
	{ raise (ExceptionDefn.Syntax_Error
		   (add_pos "Malformed initial condition"))}

    | LET variable_declaration {Ast.DECLARE $2}
    | OBS variable_declaration {Ast.OBS $2}
    | PLOT alg_expr {Ast.PLOT $2}
    | PLOT error {raise (ExceptionDefn.Syntax_Error
			   (add_pos "Malformed plot instruction, an algebraic expression is expected"))}
    | PERT perturbation_declaration {Ast.PERT (add_pos $2)}
    | CONFIG STRING value_list
	     {Ast.CONFIG (($2,rhs_pos 2),$3)}
    ;

init_declaration:
    | alg_expr non_empty_mixture
    { ($1,Ast.INIT_MIX ($2,rhs_pos 2)) }
    | alg_expr OP_PAR non_empty_mixture CL_PAR
    { ($1,Ast.INIT_MIX ($3, rhs_pos 3)) }
    | ID LAR alg_expr {($3,Ast.INIT_TOK [$1,rhs_pos 1])}
    | alg_expr ID {($1,Ast.INIT_TOK [$2,rhs_pos 2])}
    ;

value_list:
    | STRING {[$1, rhs_pos 1]}
    | STRING value_list {($1,rhs_pos 1)::$2}
    ;

perturbation_alarm:
  {None}
  | ALARM nbr {Some $2}

perturbation_post:
  {None}
  | REPEAT bool_expr {Some $2}
  | UNTIL bool_expr
   /* backward compatibility */
	 {ExceptionDefn.deprecated
	      ~pos:(Locality.of_pos (Parsing.symbol_start_pos ())
				    (Parsing.symbol_end_pos ()))
	      "perturbation"
	      (fun f -> Format.pp_print_string
			  f "use the 'repeat' construction");
	    Some (add_pos (Alg_expr.UN_BOOL_OP (Operator.NOT,$2)))}


perturbation_declaration:
    | perturbation_alarm bool_expr DO effect_list perturbation_post
    { ($1,Some $2,$4,$5) }
    | perturbation_alarm DO effect_list perturbation_post
    { ($1,None,$3,$4) }
    | REPEAT bool_expr DO effect_list UNTIL bool_expr
       /* backward compatibility */
	   {ExceptionDefn.deprecated
	      ~pos:(Locality.of_pos (Parsing.symbol_start_pos ())
				    (Parsing.symbol_end_pos ()))
	      "perturbation"
	      (fun f -> Format.pp_print_string
			  f "deprecated KaSim3 syntax");
              let () = if List.exists
			  (fun effect ->
			   match effect with
			   | (Ast.CFLOWLABEL _ | Ast.CFLOWMIX _
			      | Ast.DIN _ | Ast.DINOFF _
			      | Ast.SPECIES_OF _) -> true
			   | (Ast.STOP _ | Ast.APPLY _
			     | Ast.UPDATE _ | Ast.PRINT _
			     | Ast.SNAPSHOT _ | Ast.PLOTENTRY) -> false
			  ) $4
		     then
		       ExceptionDefn.warning
			 ~pos:(Locality.of_pos (Parsing.symbol_start_pos ())
					       (Parsing.symbol_end_pos ()))
			 (fun f ->
			  Format.pp_print_string
			    f "Perturbation need not be applied repeatedly") in
	    (None,Some $2,$4,Some (add_pos (Alg_expr.UN_BOOL_OP (Operator.NOT,$6))))}
     | perturbation_alarm bool_expr SET effect_list
		{ExceptionDefn.deprecated
		   ~pos:(Locality.of_pos (Parsing.symbol_start_pos ())
					 (Parsing.symbol_end_pos ()))
		   "perturbation"
		   (fun f -> Format.pp_print_string
			       f "'set' keyword is replaced by 'do'");
		 ($1,Some $2,$4,None)} /*For backward compatibility*/
    ;

standalone_effect_list: effect_list EOF {$1}

effect_list:
    | OP_PAR effect_list CL_PAR {$2}
    | effect {[$1]}
    | effect SEMICOLON effect_list {$1::$3}
    ;

effect:
    | ASSIGN ID alg_expr /*updating the rate of a rule*/
						      {Ast.UPDATE (($2,rhs_pos 2),$3)}
    | ASSIGN LABEL alg_expr /*updating the rate of a rule*/
						      {Ast.UPDATE (($2,rhs_pos 2),$3)}
    | TRACK LABEL boolean
	    {Ast.CFLOWLABEL ($3,($2,rhs_pos 2))}
    | TRACK pattern boolean
	    {Ast.CFLOWMIX ($3,($2,rhs_pos 2))}
    | FLUX nonempty_print_expr boolean
	   {if $3 then Ast.DIN (Primitives.RELATIVE,$2) else Ast.DINOFF $2}
    | FLUX nonempty_print_expr STRING boolean
	   {if $4 && $3 = "absolute" then Ast.DIN (Primitives.ABSOLUTE,$2)
	   else if $4 && $3 = "probability" then
	     Ast.DIN (Primitives.PROBABILITY,$2)
	   else if $4 && $3 = "relative" then Ast.DIN (Primitives.RELATIVE,$2)
	     else raise (ExceptionDefn.Syntax_Error
	       ("Incorrect DIN expression",rhs_pos 3))}
    | APPLY small_alg_expr rule_content
		 { Ast.APPLY(
		   $2,
		   ({ Ast.rewrite = fst $3;
		      Ast.bidirectional = false;
		      Ast.k_def=Alg_expr.const Nbr.zero;Ast.k_un=None;
		      Ast.k_op=None; Ast.k_op_un=None},rhs_pos 3))
		 }
    | INTRO alg_expr non_empty_mixture
        { Ast.APPLY($2,
                ({Ast.rewrite =
		  Ast.Arrow {Ast.lhs=[]; Ast.rm_token = [];
                  Ast.rhs=$3; Ast.add_token = [];};
		  Ast.bidirectional=false;
                  Ast.k_def=Alg_expr.const Nbr.zero; Ast.k_un=None;
                  Ast.k_op=None; Ast.k_op_un=None},rhs_pos 3))
        }
    | INTRO error
       {raise (ExceptionDefn.Syntax_Error
                 (add_pos "Malformed perturbation instruction, I was expecting '$ADD alg_expression kappa_expression'"))}
    | DELETE alg_expr non_empty_mixture
       { Ast.APPLY($2,
               ({Ast.rewrite =
		 Ast.Arrow {Ast.lhs=$3; Ast.rm_token = [];
                 Ast.rhs=[]; Ast.add_token = [];};
		 Ast.bidirectional=false;
                 Ast.k_def=Alg_expr.const Nbr.zero; Ast.k_un=None;
                 Ast.k_op=None; Ast.k_op_un=None},rhs_pos 3))
       }

    | DELETE error
       {raise (ExceptionDefn.Syntax_Error
                 (add_pos "Malformed perturbation instruction, I was expecting '$DEL alg_expression kappa_expression'"))}
    | ID LAR alg_expr /*updating the value of a token*/
       {
       let tk = ($1,rhs_pos 1) in
        Ast.APPLY(Alg_expr.const Nbr.one,
           add_pos
               ({Ast.rewrite =
		 Ast.Edit
		 {Ast.mix=[];
		  Ast.delta_token =
		  [(Alg_expr.BIN_ALG_OP(Operator.MINUS,$3,(Alg_expr.TOKEN_ID $1,rhs_pos 1)),rhs_pos 1),tk];
                  };
                 Ast.bidirectional=false;
                 Ast.k_def=Alg_expr.const Nbr.zero; Ast.k_un=None;
                 Ast.k_op=None; Ast.k_op_un=None}))
       }
    | SNAPSHOT print_expr {Ast.SNAPSHOT $2}
    | STOP print_expr {Ast.STOP $2}
    | PRINTF print_expr SMALLER print_expr GREATER { Ast.PRINT ($2,$4) }
    | PLOTENTRY { Ast.PLOTENTRY }
    | SPECIES_OF nonempty_print_expr non_empty_mixture boolean { Ast.SPECIES_OF ($4,$2,($3, rhs_pos 3))}
    ;

nonempty_print_expr:
    | STRING {[Primitives.Str_pexpr (add_pos $1)]}
    | mid_alg_expr {[Primitives.Alg_pexpr $1]}
    | STRING DOT nonempty_print_expr {Primitives.Str_pexpr ($1, rhs_pos 1)::$3}
    | mid_alg_expr DOT nonempty_print_expr {Primitives.Alg_pexpr $1::$3}
    ;
print_expr:
    /*empty*/ {[]}
    | nonempty_print_expr {$1}

boolean:
    | TRUE {true}
    | FALSE {false}
    ;

variable_declaration:
    | LABEL non_empty_mixture
	    {let () =
	       ExceptionDefn.deprecated
		~pos:(Locality.of_pos (Parsing.symbol_start_pos ())
				      (Parsing.symbol_end_pos ()))
		 "variable"
		 (fun f -> Format.pp_print_string
			     f "use |kappa instance| instead.")
	      in
	      (($1,rhs_pos 1),(Alg_expr.KAPPA_INSTANCE $2,rhs_pos 2))}
    | LABEL alg_expr {(($1,rhs_pos 1),$2)}
    | ID alg_expr {(($1,rhs_pos 1),$2)}
    | LABEL error
	    {raise
	       (ExceptionDefn.Syntax_Error
		  (add_pos ("Illegal definition of variable '"^$1^"'")))
	    }
    ;

small_bool_expr:
    | OP_PAR bool_expr CL_PAR {$2}
    | TRUE {add_pos Alg_expr.TRUE}
    | FALSE {add_pos Alg_expr.FALSE}

bool_expr:
    | small_bool_expr { $1 }
    | NOT small_bool_expr {add_pos (Alg_expr.UN_BOOL_OP(Operator.NOT,$2))}
    | bool_expr AND bool_expr {add_pos (Alg_expr.BIN_BOOL_OP(Operator.AND,$1,$3))}
    | bool_expr OR bool_expr {add_pos (Alg_expr.BIN_BOOL_OP(Operator.OR,$1,$3))}
    | alg_expr GREATER alg_expr
      {add_pos (Alg_expr.COMPARE_OP(Operator.GREATER,$1,$3))}
    | alg_expr SMALLER alg_expr
      {add_pos (Alg_expr.COMPARE_OP(Operator.SMALLER,$1,$3))}
    | alg_expr EQUAL alg_expr
      {add_pos (Alg_expr.COMPARE_OP(Operator.EQUAL,$1,$3))}
    | alg_expr DIFF alg_expr
      {add_pos (Alg_expr.COMPARE_OP(Operator.DIFF,$1,$3))}
    ;

standalone_bool_expr: bool_expr EOF {$1}

token_expr:
  /*empty*/ {[]}
    | PIPE sum_token {$2}
    | PIPE error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos  "Malformed token expression, I was expecting a_0 t_0 + ... + a_n t_n, where t_i are tokens and a_i any algebraic formula"))}
    ;

sum_token:
    | OP_PAR sum_token CL_PAR {$2}
    | alg_expr ID {[($1,($2,rhs_pos 2))]}
    | alg_expr ID PLUS sum_token {let l = $4 in ($1,($2,rhs_pos 2))::l}
    | alg_expr TYPE ID {[($1,($3,rhs_pos 3))]}
    | alg_expr TYPE ID PLUS sum_token {let l = $5 in ($1,($3,rhs_pos 3))::l}

rule_content:
  | pattern token_expr arrow pattern token_expr
    {Ast.Arrow {Ast.lhs=$1; Ast.rm_token = $2; Ast.rhs=$4; Ast.add_token = $5},
     $3}
  | pattern token_expr arrow token_expr
    {Ast.Arrow {Ast.lhs=$1; Ast.rm_token = $2; Ast.rhs=[]; Ast.add_token = $4},
     $3}
  | token_expr arrow pattern token_expr
    {Ast.Arrow {Ast.lhs=[]; Ast.rm_token = $1; Ast.rhs=$3; Ast.add_token = $4},
     $2}
  | token_expr arrow token_expr
    {Ast.Arrow {Ast.lhs=[]; Ast.rm_token = $1; Ast.rhs=[]; Ast.add_token = $3},
     $2}
  | pattern token_expr
    { Ast.Edit {Ast.mix = $1; Ast.delta_token = $2},false }
  | PIPE sum_token
    { Ast.Edit {Ast.mix = []; Ast.delta_token = $2},false };

rule_expression:
  | rule_content birate
    { let (k_def,k_un,k_op,k_op_un) = $2 in
      let rewrite,bidirectional = $1 in
      add_pos {
        Ast.rewrite;Ast.bidirectional;
        Ast.k_def; Ast.k_un; Ast.k_op; Ast.k_op_un;
      } };

arrow:
    | KAPPA_RAR {false}
    | KAPPA_LRAR {true}
    ;

nbr:
    | INFINITY { Nbr.F infinity }
    | FLOAT { Nbr.F $1 }
    | INT { Nbr.I $1 }

constant:
    | nbr {add_pos (Alg_expr.CONST $1)}
    | EMAX {add_pos (Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR))}
    | TMAX {add_pos (Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR))}
    | CPUTIME {add_pos (Alg_expr.STATE_ALG_OP (Operator.CPUTIME))}
    ;

variable:
    | PIPE ID PIPE {add_pos (Alg_expr.TOKEN_ID ($2))}
    | PIPE non_empty_mixture PIPE { add_pos (Alg_expr.KAPPA_INSTANCE $2) }
    | ID {add_pos (Alg_expr.ALG_VAR ($1))}
    | LABEL {add_pos (Alg_expr.ALG_VAR ($1))}
    | TIME {add_pos (Alg_expr.STATE_ALG_OP (Operator.TIME_VAR))}
    | EVENT {add_pos (Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR))}
    | NULL_EVENT {add_pos (Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR))}
    ;

small_alg_expr:
    | OP_PAR alg_expr CL_PAR {$2}
    | constant {$1}
    | variable {$1}
    | MAX small_alg_expr small_alg_expr
	  {add_pos (Alg_expr.BIN_ALG_OP(Operator.MAX,$2,$3))}
    | MIN small_alg_expr small_alg_expr
	  {add_pos (Alg_expr.BIN_ALG_OP(Operator.MIN,$2,$3))}
    | EXPONENT mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.EXP,$2))}
    | SINUS mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.SINUS,$2))}
    | COSINUS mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.COSINUS,$2))}
    | TAN mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.TAN,$2))}
    | ABS mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.INT,$2))}
    | SQRT mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.SQRT,$2))}
    | LOG mid_alg_expr {add_pos (Alg_expr.UN_ALG_OP(Operator.LOG,$2))}
    ;

mid_alg_expr:
    | MINUS mid_alg_expr { add_pos (Alg_expr.UN_ALG_OP(Operator.UMINUS,$2)) }
    | small_alg_expr { $1 }
    | mid_alg_expr MULT mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.MULT,$1,$3))}
    | mid_alg_expr PLUS mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.SUM,$1,$3))}
    | mid_alg_expr DIV mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.DIV,$1,$3))}
    | mid_alg_expr MINUS mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.MINUS,$1,$3))}
    | mid_alg_expr POW mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.POW,$1,$3))}
    | mid_alg_expr MODULO mid_alg_expr {add_pos (Alg_expr.BIN_ALG_OP(Operator.MODULO,$1,$3))}

alg_expr:
    | mid_alg_expr {$1}
    | bool_expr THEN alg_expr ELSE small_alg_expr {add_pos (Alg_expr.IF($1,$3,$5))}

birate:
    | AT rate {let (k2,k1) = $2 in (k2,k1,None,None)}
    | AT rate COMMA rate {let (k2,k1) = $2 in
			  let (kback,kback1) = $4 in
			  (k2,k1,Some kback,kback1)}
    | error {raise (ExceptionDefn.Syntax_Error (add_pos "rule rate expected"))}
    ;

rate:
    | alg_expr OP_CUR alg_with_radius CL_CUR {($1,Some $3)}
    | alg_expr OP_PAR alg_with_radius CL_PAR {
      ExceptionDefn.deprecated
         ~pos:(Locality.of_pos (Parsing.rhs_start_pos 2)
			       (Parsing.rhs_end_pos 4))
	      "unimolecular rate"
	      (fun f -> Format.pp_print_string
			  f "use {} instead of ()");
			  ($1,Some $3)
    }
    | alg_expr {($1,None)}
    | OP_CUR alg_with_radius CL_CUR
      {(Locality.dummy_annot (Alg_expr.CONST Nbr.zero),Some $2)}
    | alg_expr OP_CUR CL_CUR
      {($1,Some (Locality.dummy_annot (Alg_expr.CONST Nbr.zero),None))}
    | {raise (ExceptionDefn.Syntax_Error (add_pos "missing rule rate"))}
    ;

alg_with_radius:
    | alg_expr {($1,None)}
    | alg_expr TYPE alg_expr {($1, Some $3)}
    ;

pattern:
    | OP_PAR pattern CL_PAR {$2}
    | agent_expression COMMA pattern {$1 :: $3}
    | agent_expression {[$1]}
;

non_empty_mixture:
    | ID OP_PAR interface_expression CL_PAR
    { [Ast.Present (($1,rhs_pos 1), $3, None)] }
    | ID OP_PAR interface_expression CL_PAR COMMA pattern
    { Ast.Present (($1,rhs_pos 1), $3, None) :: $6}
    ;

mod_agent:
	| { None }
	| PLUS { Some Ast.Create }
	| MINUS { Some Ast.Erase };

agent_expression:
    | mod_agent ID OP_PAR interface_expression CL_PAR
	 {Ast.Present (($2,rhs_pos 2), $4, $1)}
    | mod_agent ID error
	 { raise (ExceptionDefn.Syntax_Error
		    (add_pos ("Malformed agent '"^$2^"'")))}
    ;

interface_expression:
  /*empty*/ {[]}
    | port_expression COMMA interface_expression {$1::$3}
    | port_expression {[$1]}
    ;

counter_test:
   | TYPE INT { Some (Ast.CEQ $2,rhs_pos 2)}
   | TYPE GREATER INT { Some (Ast.CGTE $3,rhs_pos 3)}
   | TYPE ID { Some (Ast.CVAR $2,rhs_pos 2)}

port_expression:
    | ID internal_state link_state_mod
	 { Ast.Port
	   {Ast.port_nme=($1,rhs_pos 1); Ast.port_int=$2; Ast.port_lnk=[];
	    Ast.port_int_mod = None; Ast.port_lnk_mod = $3; } }
    | ID internal_state link_state link_state_mod
	 { Ast.Port
	   {Ast.port_nme=($1,rhs_pos 1); Ast.port_int=$2; Ast.port_lnk=$3;
	    Ast.port_int_mod = None; Ast.port_lnk_mod = $4; } }
    | ID internal_state DIV KAPPA_MRK link_state_mod
	 { Ast.Port
	   {Ast.port_nme=($1,rhs_pos 1); Ast.port_int=$2; Ast.port_lnk=[];
	    Ast.port_int_mod = Some($4,rhs_pos 4); Ast.port_lnk_mod = $5; } }
    | ID internal_state DIV KAPPA_MRK link_state link_state_mod
	 { Ast.Port
	   {Ast.port_nme=($1,rhs_pos 1); Ast.port_int=$2; Ast.port_lnk=$5;
	    Ast.port_int_mod = Some($4,rhs_pos 4); Ast.port_lnk_mod = $6; } }
    | ID PLUS EQUAL INT
         { Ast.Counter
	   { Ast.count_nme = ($1,rhs_pos 1);
	   Ast.count_test = None;
	   Ast.count_delta = ($4,rhs_pos 4)} }
    | ID PLUS EQUAL MINUS INT
         { Ast.Counter
	   { Ast.count_nme = ($1,rhs_pos 1);
	   Ast.count_test = None;
	   Ast.count_delta = (-$5,rhs_pos 5)} }
    | ID counter_test PLUS EQUAL INT
         { Ast.Counter
	   { Ast.count_nme = ($1,rhs_pos 1);
	   Ast.count_test = $2;
	   Ast.count_delta = ($5,rhs_pos 5)} }
   | ID counter_test PLUS EQUAL MINUS INT
         { Ast.Counter
	   { Ast.count_nme = ($1,rhs_pos 1);
	   Ast.count_test = $2;
	   Ast.count_delta = (- $6,rhs_pos 6)} }
   | ID counter_test
         { Ast.Counter
	   { Ast.count_nme = ($1,rhs_pos 1);
	   Ast.count_test = $2;
	   Ast.count_delta = Locality.dummy_annot 0} }
    ;

internal_state:
  /*empty*/ {[]}
    | KAPPA_MRK internal_state {(Some $1,rhs_pos 1)::$2}
    | error
       {raise (ExceptionDefn.Syntax_Error
       (add_pos "Issue after internal state"))}
    ;

link_state_mod:
	| {None}
	| DIV KAPPA_LNK DOT {Some None}
	| DIV KAPPA_LNK INT {Some (Some ($3,rhs_pos 3))}
	| DIV error
	{raise (ExceptionDefn.Syntax_Error
	  (add_pos "Incorrect link modification"))};


a_link_state:
    | KAPPA_LNK DOT {(Ast.LNK_FREE,rhs_pos 2)}
    | KAPPA_LNK INT {(Ast.LNK_VALUE ($2,()),rhs_pos 2)}
    | KAPPA_LNK KAPPA_SEMI {(Ast.LNK_SOME,rhs_pos 2)}
    | KAPPA_LNK ID DOT ID {add_pos (Ast.LNK_TYPE
				      (($2,rhs_pos 2),($4,rhs_pos 4)))}
    | KAPPA_WLD {add_pos Ast.LNK_ANY}
    | KAPPA_LNK error
	{raise (ExceptionDefn.Syntax_Error
		  (add_pos "Invalid link state"))}
;

link_state:
	| a_link_state link_state {$1::$2}
	| a_link_state {[$1]};

interactive_command:
	| RUN NEWLINE {Ast.RUN (Locality.dummy_annot Alg_expr.FALSE)}
	| RUN bool_expr NEWLINE {Ast.RUN $2}
	| effect_list NEWLINE {Ast.MODIFY $1}
	| EOF {Ast.QUIT}
	| error
	{raise (ExceptionDefn.Syntax_Error (add_pos "Unrecognized command"))}
%%
