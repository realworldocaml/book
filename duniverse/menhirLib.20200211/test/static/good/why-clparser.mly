/*
 * The Caduceus certification tool
 * Copyright (C) 2003 Jean-Christophe Filliâtre - Claude Marché
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU General Public License version 2 for more details
 * (enclosed in the file GPL).
 */

/* Grammar for C annotations */

%{
  open Ctypes
  open Cast
  open Clogic

  let loc () = (symbol_start (), symbol_end ())
  let loc_i i = (rhs_start i, rhs_end i)
  let info x = { Clogic.lexpr_node = x; lexpr_loc = loc () }

  type ghost_decl =
    | Dsimple
    | Darray of ghost_decl * lexpr option

  let rec expr_of_lexpr e =
    match e.lexpr_node with
      | PLconstant c ->
	  { Cast.node = Cast.CEconstant c ; Cast.loc = e.lexpr_loc }
      | _ -> Creport.error e.lexpr_loc "not a constant"

  let option_app f = function None -> None | Some x -> Some (f x)

  let rec ghost_type ty gd =
    match gd with
      | Dsimple -> ty
      | Darray(gd,size) ->
	    Cast_misc.noattr
	      (Cast.CTarray (ghost_type ty gd, option_app expr_of_lexpr size))

  let ghost ty (id,gd,cinit) =
    let gty = ghost_type ty gd in
    LDghost(gty,id,cinit)


%}

%token <string> IDENTIFIER STRING_LITERAL TYPENAME
%token <Clogic.constant> CONSTANT
%token LPAR RPAR IF ELSE COLON COLONCOLON DOT DOTDOT AMP
%token INT FLOAT LT GT LE GE EQ NE COMMA ARROW EQUAL
%token FORALL EXISTS IFF IMPLIES AND OR NOT
%token TRUE FALSE OLD AT RESULT BLOCK_LENGTH BASE_ADDR
%token SEPARATED FULLSEPARATED VALID VALID_INDEX VALID_RANGE FRESH THEN
%token QUESTION MINUS PLUS STAR SLASH PERCENT LSQUARE RSQUARE EOF
%token INVARIANT VARIANT DECREASES FOR LABEL ASSERT SEMICOLON NULL
%token REQUIRES ENSURES ASSIGNS LOOP_ASSIGNS NOTHING
%token READS LOGIC PREDICATE AXIOM LBRACE RBRACE GHOST SET
%token VOID CHAR SIGNED UNSIGNED SHORT LONG DOUBLE STRUCT ENUM UNION

%right prec_named
%nonassoc prec_forall prec_exists
%right IMPLIES IFF
%left OR
%left AND
%nonassoc prec_not
%nonassoc prec_if
%right QUESTION prec_question
%left prec_relation LT GT LE GE EQ NE
%left PLUS MINUS
%left STAR SLASH PERCENT AMP
%right prec_uminus
%right prec_cast
%left DOT ARROW LSQUARE
%right prec_par

%type <Cast.parsed_annot> annot
%start annot

%%

lexpr:
  /* predicates */
  lexpr IMPLIES lexpr { info (PLimplies ($1, $3)) }
| lexpr IFF lexpr { info (PLiff ($1, $3)) }
| lexpr OR lexpr     { info (PLor ($1, $3)) }
| lexpr AND lexpr    { info (PLand ($1, $3)) }
| NOT lexpr %prec prec_not { info (PLnot $2) }
| TRUE { info PLtrue }
| FALSE { info PLfalse }
| lexpr relation lexpr %prec prec_relation { info (PLrel ($1, $2, $3)) }
| IF lexpr THEN lexpr ELSE lexpr %prec prec_if
      { info (PLif ($2, $4, $6)) }
| FORALL ne_parameters SEMICOLON lexpr %prec prec_forall
      { info (PLforall ($2, $4)) }
| EXISTS ne_parameters SEMICOLON lexpr %prec prec_exists
      { info (PLexists ($2, $4)) }
| SEPARATED LPAR lexpr COMMA lexpr RPAR { info (PLseparated ($3,$5)) }
| FULLSEPARATED LPAR lexpr COMMA lexpr RPAR { info (PLfullseparated ($3,$5)) }
| VALID LPAR lexpr RPAR { info (PLvalid ($3)) }
| VALID_INDEX LPAR lexpr COMMA lexpr RPAR { info (PLvalid_index ($3,$5)) }
| VALID_RANGE LPAR lexpr COMMA lexpr COMMA lexpr RPAR
      { info (PLvalid_range ($3,$5,$7)) }
| FRESH LPAR lexpr RPAR { info (PLfresh ($3)) }
/* terms */
| NULL { info PLnull }
| CONSTANT { info (PLconstant $1) }
| lexpr PLUS lexpr { info (PLbinop ($1, Badd, $3)) }
| lexpr MINUS lexpr { info (PLbinop ($1, Bsub, $3)) }
| lexpr STAR lexpr { info (PLbinop ($1, Bmul, $3)) }
| lexpr SLASH lexpr { info (PLbinop ($1, Bdiv, $3)) }
| lexpr PERCENT lexpr { info (PLbinop ($1, Bmod, $3)) }
| lexpr ARROW IDENTIFIER { info (PLarrow ($1, $3)) }
| lexpr DOT IDENTIFIER { info (PLdot ($1, $3)) }
| lexpr LSQUARE lexpr RSQUARE { info (PLarrget ($1, $3)) }
| lexpr LSQUARE lexpr_option DOTDOT lexpr_option RSQUARE
   { info (PLrange ($1, $3, $5)) }
| MINUS lexpr %prec prec_uminus { info (PLunop (Uminus, $2)) }
| PLUS lexpr %prec prec_uminus { $2 }
| STAR lexpr { info (PLunop (Ustar, $2)) }
| AMP lexpr { info (PLunop (Uamp, $2)) }
| lexpr QUESTION lexpr COLON lexpr %prec prec_question
    { info (PLif ($1, $3, $5)) }
| OLD LPAR lexpr RPAR { info (PLold $3) }
| AT LPAR lexpr COMMA IDENTIFIER RPAR { info (PLat ($3, $5)) }
| BASE_ADDR LPAR lexpr RPAR { info (PLbase_addr $3) }
| BLOCK_LENGTH LPAR lexpr RPAR { info (PLblock_length $3) }
| RESULT { info PLresult }
/* both terms and predicates */
| LPAR lexpr RPAR %prec prec_par { $2 }
| IDENTIFIER { info (PLvar (Info.default_var_info $1)) }
| IDENTIFIER LPAR lexpr_list RPAR
    { info (PLapp (Info.default_logic_info $1, $3)) }
/* Cast. TODO: (identifier *) lexpr needs TYPENAME (see below) */
| LPAR logic_type_not_id RPAR lexpr %prec prec_cast { info (PLcast ($2, $4)) }
| LPAR lexpr RPAR lexpr %prec prec_cast
    { match $2.lexpr_node with
	| PLvar x -> info (PLcast (LTvar x.Info.var_name, $4))
	| _ -> raise Parse_error }
| IDENTIFIER COLONCOLON lexpr %prec prec_named { info (PLnamed ($1, $3)) }
;

lexpr_option:
| /* epsilon */ { None }
| lexpr         { Some $1 }
;

logic_type:
  IDENTIFIER { LTvar $1 }
| IDENTIFIER stars { $2 (LTvar $1) }
| logic_type_not_id { $1 }
;

logic_type_not_id:
| VOID           { LTvoid }
| CHAR           { LTint }       /** [char] */
| SIGNED CHAR    { LTint }      /** [signed char] */
| UNSIGNED CHAR  { LTint }      /** [unsigned char] */
| SIGNED INT     { LTint }        /** [int] */
| INT            { LTint }        /** [int] */
| UNSIGNED INT   { LTint }       /** [unsigned int] */
| SIGNED SHORT   { LTint }      /** [short] */
| SHORT          { LTint }      /** [short] */
| UNSIGNED SHORT { LTint }     /** [unsigned short] */
| SIGNED LONG    { LTint }       /** [long] */
| LONG           { LTint }       /** [long] */
| UNSIGNED LONG  { LTint }      /** [unsigned long] */
| SIGNED LONG LONG { LTint }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| LONG LONG   { LTint }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| UNSIGNED LONG LONG { LTint }  /** [unsigned long long]
                                (or [unsigned _int64] on Microsoft Visual C) */
| FLOAT       { LTfloat }
| DOUBLE      { LTfloat }
| LONG DOUBLE { LTfloat }
/***
| STRUCT IDENTIFIER { LTstruct $2 }
| ENUM IDENTIFIER { LTenum $2 }
| UNION IDENTIFIER { LTunion $2 }
***/
| TYPENAME         { LTvar $1 } /* TODO: Logic_lexer should make it */
| logic_type_not_id STAR { LTpointer $1 }
;

stars:
  STAR { fun t -> LTpointer t }
| stars STAR { fun t -> $1 (LTpointer t) }
;

relation:
  | LT    { Lt }
  | GT    { Gt }
  | LE    { Le }
  | GE    { Ge }
  | EQ    { Eq }
  | NE    { Neq }
;

lexpr_list:
| /* epsilon */ { [] }
| ne_lexpr_list  { $1 }
;

ne_lexpr_list:
| lexpr                    { [$1] }
| lexpr COMMA ne_lexpr_list { $1 :: $3 }
;

pre_condition:
  /* epsilon */ { None }
| REQUIRES lexpr { Some $2 }
;

post_condition:
  /* epsilon */  { None }
| ENSURES lexpr { Some $2 }
;

spec:
  pre_condition effects post_condition decreases
    { { requires = $1; assigns = $2; ensures = $3; decreases = $4 } }
;

loop_annot:
  invariant loop_effects variant
    { { invariant = Some $1; loop_assigns = $2; variant = Some $3 } }
| loop_effects variant
    { { invariant = None; loop_assigns = $1; variant = Some $2 } }
| invariant loop_effects
    { { invariant = Some $1; loop_assigns = $2; variant = None } }
| ne_loop_effects
    { { invariant = None; loop_assigns = Some $1; variant = None } }
;

invariant:
| INVARIANT lexpr { $2 }
;

variant:
  VARIANT lexpr FOR IDENTIFIER { ($2, Some $4) }
| VARIANT lexpr                { ($2, None) }
;

decreases:
  /* epsilon */   { None }
| DECREASES variant { Some $2 }
;

annot:
  annotation EOF   { $1 }
;

annotation:
| decl             { Adecl [$1] }
| ghost_decl       { Adecl $1 }
| spec             { Aspec $1 }
| loop_annot       { Aloop_annot $1 }
| ASSERT lexpr     { Acode_annot (Assert $2) }
| LABEL IDENTIFIER { Acode_annot (Label $2) }
| SET ghost_lvalue EQUAL lexpr
                   { Acode_annot(GhostSet($2,$4)) }
;

ghost_lvalue: lexpr { $1 }
;

effects:
  /* epsilon */ { None }
| ASSIGNS locations { Some $2 }
| ASSIGNS NOTHING { Some [] }
;

loop_effects:
  /* epsilon */ { None }
| ne_loop_effects { Some $1 }
;

ne_loop_effects:
| LOOP_ASSIGNS locations { $2 }
| LOOP_ASSIGNS NOTHING { [] }
;

locations:
| location { [$1] }
| location COMMA locations { $1 :: $3 }
;

location:
  lexpr { $1 }
;

decl:
  LOGIC logic_type IDENTIFIER LPAR parameters RPAR
    { LDlogic (Info.default_logic_info $3, $2, $5, []) }
| LOGIC logic_type IDENTIFIER LPAR parameters RPAR READS locations
    { LDlogic (Info.default_logic_info $3, $2, $5, $8) }
| LOGIC logic_type IDENTIFIER LPAR parameters RPAR LBRACE lexpr RBRACE
    { LDlogic_def (Info.default_logic_info $3, $2, $5, $8) }
| PREDICATE IDENTIFIER LPAR parameters RPAR
    { LDpredicate_reads (Info.default_logic_info $2, $4, []) }
| PREDICATE IDENTIFIER LPAR parameters RPAR READS locations
    { LDpredicate_reads (Info.default_logic_info $2, $4, $7) }
| PREDICATE IDENTIFIER LPAR parameters RPAR LBRACE lexpr RBRACE
    { LDpredicate_def (Info.default_logic_info $2, $4, $7) }
| AXIOM IDENTIFIER COLON lexpr { LDaxiom ($2, $4) }
| INVARIANT IDENTIFIER COLON lexpr { LDinvariant ($2, $4) }
;

ghost_decl:
| GHOST type_specifier init_declarator_list
      { List.map (ghost $2) $3 }
;

type_specifier:
| CHAR { Cast_misc.noattr (CTint (Unsigned, Char)) }
| INT { Cast_misc.noattr (CTint (Signed, Int)) }
| FLOAT { Cast_misc.noattr (CTfloat Float) }
| DOUBLE { Cast_misc.noattr (CTfloat Double) }
;

parameters:
  /* epsilon */  { [] }
| ne_parameters { $1 }
;

ne_parameters:
  parameter { [$1] }
| parameter COMMA ne_parameters { $1 :: $3 }
;

parameter:
  logic_type IDENTIFIER { ($1, $2) }
| logic_type IDENTIFIER LSQUARE RSQUARE { (LTarray $1, $2) }
;


/* ghost variables */

init_declarator_list
        : init_declarator { [$1] }
        | init_declarator_list COMMA init_declarator { $1 @ [$3] }
        ;

init_declarator
        : declarator
            { let (id,d) = $1 in (id,d, None) }
        | declarator EQUAL c_initializer
	    { let (id,d) = $1 in (id,d, Some $3) }
        ;

declarator
        : /*TODO pointer direct_declarator { let id,d = $2 in id, $1 d }
        | */direct_declarator { $1 }
        ;

direct_declarator
        : IDENTIFIER
            { $1, Dsimple }
        | direct_declarator LSQUARE lexpr RSQUARE
	    { let id,d = $1 in id, Darray (d, Some $3) }
        | direct_declarator LSQUARE RSQUARE
	    { let id,d = $1 in id, Darray (d, None) }
        ;

c_initializer
        : lexpr { Iexpr $1 }
        | LBRACE c_initializer_list RBRACE { Ilist $2 }
        | LBRACE c_initializer_list COMMA RBRACE { Ilist $2 }
        ;

c_initializer_list
        : c_initializer { [$1] }
        | c_initializer_list COMMA c_initializer { $1 @ [$3] }
        ;

%%
