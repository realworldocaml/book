(* Original file: lustre-v6.1.737/lustre-v6.1.737/src/lv6parser.mly *)
%{
open Lxm
open AstPredef
open AstV6
open AstCore
open Lv6parserUtils


(**********************************************************************************)
(**********************************************************************************)
(**********************************************************************************)
(*
   TODO:
   - reindent
 *)

%}

%token TK_EOF

%token <Lxm.t> TK_ERROR

%token <Lxm.t> TK_EXTERN
%token <Lxm.t> TK_UNSAFE

%token <Lxm.t> TK_AND
%token <Lxm.t> TK_ARROW
%token <Lxm.t> TK_ASSERT
%token <Lxm.t> TK_BAR
%token <Lxm.t> TK_BOOL
%token <Lxm.t> TK_CDOTS
%token <Lxm.t> TK_CLOSE_BRACE
%token <Lxm.t> TK_CLOSE_BRACKET
%token <Lxm.t> TK_CLOSE_PAR
%token <Lxm.t> TK_CLOSE_STATIC_PAR
%token <Lxm.t> TK_COLON
%token <Lxm.t> TK_COMA
%token <Lxm.t> TK_CONST
%token <Lxm.t> TK_CURRENT
%token <Lxm.t> TK_DIESE
%token <Lxm.t> TK_DIV
%token <Lxm.t> TK_DOT
%token <Lxm.t> TK_EQ
%token <Lxm.t> TK_ELSE
%token <Lxm.t> TK_ENUM
%token <Lxm.t> TK_FALSE
%token <Lxm.t> TK_FUNCTION
%token <Lxm.t> TK_GT
%token <Lxm.t> TK_GTE
%token <Lxm.t> TK_HAT
%token <Lxm.t> TK_ICONST
%token <Lxm.t> TK_IDENT
%token <Lxm.t> TK_LONGIDENT
%token <Lxm.t> TK_IF
%token <Lxm.t> TK_IMPL
%token <Lxm.t> TK_INT
%token <Lxm.t> TK_LET
%token <Lxm.t> TK_LT
%token <Lxm.t> TK_LTE
%token <Lxm.t> TK_MERGE
%token <Lxm.t> TK_MINUS
%token <Lxm.t> TK_MOD
%token <Lxm.t> TK_NEQ
%token <Lxm.t> TK_NODE
%token <Lxm.t> TK_NOR
%token <Lxm.t> TK_NOT
%token <Lxm.t> TK_OPEN_BRACE
%token <Lxm.t> TK_OPEN_BRACKET
%token <Lxm.t> TK_OPEN_PAR
%token <Lxm.t> TK_OPEN_STATIC_PAR
%token <Lxm.t> TK_OPERATOR
%token <Lxm.t> TK_OR
%token <Lxm.t> TK_PCENT
%token <Lxm.t> TK_PLUS
%token <Lxm.t> TK_POWER
%token <Lxm.t> TK_PRE
%token <Lxm.t> TK_FBY
%token <Lxm.t> TK_RCONST
%token <Lxm.t> TK_REAL
%token <Lxm.t> TK_RETURNS
%token <Lxm.t> TK_SEMICOL
%token <Lxm.t> TK_SLASH
%token <Lxm.t> TK_STAR
%token <Lxm.t> TK_STEP
%token <Lxm.t> TK_STRUCT
%token <Lxm.t> TK_TEL
%token <Lxm.t> TK_THEN
%token <Lxm.t> TK_TRUE
%token <Lxm.t> TK_TYPE
%token <Lxm.t> TK_VAR
%token <Lxm.t> TK_WHEN
%token <Lxm.t> TK_WITH
%token <Lxm.t> TK_XOR
%token <Lxm.t> TK_MODEL
%token <Lxm.t> TK_PACKAGE
%token <Lxm.t> TK_NEEDS
%token <Lxm.t> TK_PROVIDES
%token <Lxm.t> TK_USES
%token <Lxm.t> TK_IS
%token <Lxm.t> TK_BODY
%token <Lxm.t> TK_END
%token <Lxm.t> TK_INCLUDE
%token <Lxm.t> TK_STRING
/* %token <Lxm.t> TK_QUOTE */

/* Priorities */

%left TK_ELSE
%left TK_BAR
%left TK_ARROW
%nonassoc TK_STEP
%nonassoc TK_CDOTS
%right TK_IMPL
%left TK_OR TK_XOR
%left TK_AND
%nonassoc TK_LT TK_LTE TK_EQ TK_GTE TK_GT TK_NEQ
%nonassoc TK_NOT
%left TK_PLUS TK_MINUS
%left TK_STAR TK_SLASH TK_PCENT TK_MOD TK_DIV
%left TK_POWER
%left TK_WHEN
%nonassoc TK_INT TK_REAL
%nonassoc TK_UMINUS TK_PRE TK_CURRENT TK_DIESE TK_NOR /* TK_DIESE TK_NOR useless ? */
%left TK_HAT TK_DOT
%right TK_OPEN_BRACKET TK_OPEN_BRACE TK_SEMICOL
%right TK_COMA
%right TK_FBY

/* Entry point */
%start program
%type <AstV6.t> program

%%

/*-------------------------------------------------------
        GRAMMAR
---------------------------------------------------------
NOTES ON THE CODE:

- Nothing is "built" for the main list declarations :
each definition is directly inserted in the corresponding
hash table (i.e. each declaration makes a side effect)

- Any rule that produces a caml list builds it in THE REVERSE
ORDER (in order to save the yacc stack and to avoid
the use of the caml @ operator). This is why they are
named "TotoReverseList". This is why they must not be
used in other rules: onr have to use the corresponding
"TotoList" which finally reverse the list.
-------------------------------------------------------*/

/*
A lustre file is either an old-fashioned list of item decl,
or a list of pack/model declaration
*/

program: IncludeList PackBody TK_EOF
        {
                AstV6.PRPackBody(List.rev $1, $2)
        }
|       IncludeList PackList TK_EOF
        {
                AstV6.PRPack_or_models (List.rev $1, List.rev $2)
        }
;

PackList: OnePack
        { [$1] }
|       PackList OnePack
        { $2::$1 }
;

OnePack: /* ebnf:print=short */
        ModelDecl
                { AstV6.NSModel $1 }
|       PackDecl
                { AstV6.NSPack $1 }
|       PackEq
                { AstV6.NSPack $1 }
;

Include: TK_INCLUDE TK_STRING
        { (Lxm.str $2)  }
;

IncludeList: /* ebnf:print=expand */
		/* empty */
        { [] }
|       IncludeList Include
        { $2::$1 }
;

/*
Pour les provides, on rend des decls, bien
que syntaxiquement, on n'autorise pas n'importe quoi ...

*/
Provides:
        /* nada */
                { None }
/* |    TK_PROVIDES StaticParamList TK_SEMICOL */
|       TK_PROVIDES ProvideList
        { Some (List.rev $2) }
;

ProvideList: /* ebnf:print=expand */ Provide TK_SEMICOL
                { [$1]  }
        |   ProvideList Provide TK_SEMICOL
                { $2::$1 }
        ;



Provide:
        /* constante abstraite */
        TK_CONST Lv6Id TK_COLON Type ConstDefOpt
        {
                Lxm.flagit
                  (ConstInfo (ExternalConst (Lxm.id $2, $4, $5)))
                $2
        }
        /* noeud abstrait
         2015/07 -> les noeuds/fonctions parametriques n'étaient pas prévus
         manque :  StaticParams
         voir : Lv6parserUtils.treat_abstract_node
         */
|       TK_UNSAFE TK_NODE Lv6Id StaticParams Params TK_RETURNS Params
        {
                treat_abstract_node true true $3 $4 $5 $7
        }
        /* fonction abstraite */
|       TK_NODE Lv6Id StaticParams Params TK_RETURNS Params
        {
                treat_abstract_node false true $2 $3 $4 $6
        }
        /* fonction abstraite */
|       TK_UNSAFE TK_FUNCTION Lv6Id StaticParams Params TK_RETURNS Params
        {
                treat_abstract_node true false $3 $4 $5 $7
        }
|       TK_FUNCTION Lv6Id StaticParams Params TK_RETURNS Params
        {
                treat_abstract_node false false $2 $3 $4 $6
        }
        /* type abstrait... */
|       TK_TYPE OneTypeDecl
        { Lxm.flagit (TypeInfo (snd $2)) (fst $2) }
;

ConstDefOpt: /* ebnf:print=expand */
        { None}
|       TK_EQ Expression
        {
          Some $2
        }
;
ModelDecl:
        TK_MODEL Lv6Id
        Uses
        /* TK_NEEDS PackParamList TK_SEMICOL */
        TK_NEEDS StaticParamList TK_SEMICOL
        Provides
        TK_BODY
                PackBody
        TK_END
        {
                let mdecl = {
                        mo_name = (Lv6Id.pack_name_of_string (Lxm.str $2));
                        mo_uses = $3 ;
                        mo_needs = (List.rev $5) ;
                        mo_provides = $7 ;
                        mo_body = $9;
                } in
                {it = mdecl; src = $2 }
        }
;


PackDecl:
        TK_PACKAGE Lv6Id
        Uses
        Provides
        TK_BODY
                PackBody
        TK_END
        {
                let pdef = PackGiven {
                        pg_uses = $3 ;
                        pg_provides = $4 ;
                        pg_body = $6;
                } in
                let pdecl = {
                        pa_name = (Lv6Id.pack_name_of_string (Lxm.str $2));
                        pa_def = pdef;
                } in
                {it = pdecl; src = $2 }
        }
;

/* pack params are identical to node static Packparams (?) */
/*
PackParamList:
        StaticParamList
                { $1 }
;
*/


Uses:
                /* nada */
        { [] }
|       TK_USES Lv6IdList TK_SEMICOL
        {
                List.rev_map lexeme_to_pack_name_flagged $2
        }
;

/* */
Eq_or_Is: TK_EQ {}
        | TK_IS {}
        ;

/* I don't like by-pos notation, but keep it
        for backward compatibility
*/
PackEq:
        TK_PACKAGE Lv6Id Eq_or_Is Lv6Id TK_OPEN_PAR
                ByNameStaticArgList
        TK_CLOSE_PAR TK_SEMICOL
        {
                let pdef = PackInstance {
                        pi_model =  (Lxm.id $4);
                        pi_args = (List.rev $6);
                } in
                let pa = {
                        pa_name = (Lv6Id.pack_name_of_string (Lxm.str $2));
                        pa_def = pdef;
                } in
                {it = pa; src = $2 }
        }
;

/* PackBody :
        les informations collectées dans les tables
        sont figées, et on remet les tables à 0 ...
 */

PackBody:
        DeclList
        {
                let res = AstV6.make_packbody
                        const_table type_table node_table (List.rev !def_list) in
                (* clean all ... *)
                Hashtbl.clear const_table ;
                Hashtbl.clear type_table ;
                Hashtbl.clear node_table ;
                def_list := [] ;
                res
        }
;

/* Declarations */

DeclList: /* ebnf:print=expand */
				OneDecl
                        { }
        |   DeclList OneDecl
                        { }
        ;

OneDecl: /* ebnf:print=short */
                ConstDecl
                        { }
        |       TypeDecl
                        { }
        |       ExtNodeDecl
                        { }
        |       NodeDecl
                        { }
        ;

/* Lv6Idifiers and lists */

Lv6IdRef: /* ebnf:print=ignore */
        /* simple or long ... */
        TK_IDENT
        { idref_of_lxm $1 }
	|    TK_LONGIDENT
        { idref_of_lxm $1 }
;

/* Lv6Idifiers and lists */

Lv6Id: /* ebnf:print=ignore */
		TK_IDENT Pragma
        { (Lv6parserUtils.make_ident $1 $2) }
        ;


Lv6IdList: /* ebnf:print=expand */
				Lv6Id
                        { [$1] }
        |   Lv6IdList TK_COMA Lv6Id
                        { $3::$1 }
        ;

TypedLv6IdsList: TypedLv6Ids
                        { [ $1 ] }
        |   TypedLv6IdsList TK_SEMICOL TypedLv6Ids
                        { $3::$1 }
        ;

TypedLv6Ids:   Lv6IdList TK_COLON Type
        /* WARNING ! il faut remettre la liste à l'endroit */
                        { ((List.rev $1), $3 ) }
        ;

TypedValuedLv6Ids: TypedValuedLv6Id
                        { $1 }
        |   TypedValuedLv6Ids TK_SEMICOL TypedValuedLv6Id
                        { List.append $1 $3 }
        ;

TypedValuedLv6Id :
        /* Les listes d'idents en partie gauche sont
           acceptées pour les idents SANS valeur
         */
                Lv6Id TK_COLON Type
                        { (id_valopt_list_of_id_list [$1] $3 ) }
        |       Lv6Id TK_COMA Lv6IdList TK_COLON Type
                        { (id_valopt_list_of_id_list ($1::(List.rev $3)) $5) }
        /* Mais pas pour les constantes définies :
        */
        |  Lv6Id TK_COLON Type TK_EQ Expression
                        { [id_valopt_of_id_val $1 $3 $5]  }
;


/* constantes */

ConstDecl: TK_CONST ConstDeclList
					{ treat_const_decl_list $2 }
		;

ConstDeclList:
			OneConstDecl TK_SEMICOL
				{ $1 }
		|	ConstDeclList OneConstDecl TK_SEMICOL
				{ $1 @ $2 }
		;

/* Retourne une liste (lxm, const_info) */
OneConstDecl:
/* Les listes d'idents en partie gauche sont acceptÃ©es
	pour les constantes externes : */
			Lv6Id TK_COLON Type
				{ (make_external_const_list [$1] $3 ) }
		|	Lv6Id TK_COMA Lv6IdList TK_COLON Type
				{ (make_external_const_list ($1::(List.rev $3)) $5) }
/* Mais pas pour les constantes définies : */
		|  Lv6Id TK_COLON Type TK_EQ Expression
				{ [ (make_defined_const $1 (Some $3) $5) ] }
		|  Lv6Id TK_EQ Expression
				{ [ (make_defined_const $1 (None) $3 ) ] }
		;


/* types */

TypeDecl:   TK_TYPE TypeDeclList
                { List.iter treat_type_decl (List.rev $2) }
        ;

TypeDeclList:   OneTypeDecl TK_SEMICOL
                        { [$1] }
        |   TypeDeclList OneTypeDecl TK_SEMICOL
                        { $2::$1 }
        ;

/* returns a couple (lxm, type_info) */
OneTypeDecl:
        /* type abstrait (externes) */
                Lv6Id
                { ($1, ExternalType (Lxm.id $1)) }
        /* un alias sur type immédiat */
        |       Lv6Id TK_EQ Type
                { ($1, AliasedType ((Lxm.id $1), $3)) }
        /* type énuméré */
        /* WARNING ! il faut remettre la liste à l'endroit */
        |       Lv6Id TK_EQ TK_ENUM TK_OPEN_BRACE Lv6IdList TK_CLOSE_BRACE
                {
							let cstnamelist = List.rev_map lexeme_to_ident_flagged $5 in
							($1, EnumType ((Lxm.id $1), cstnamelist))
                }
        /* type structure à champs nommés */
        /* WARNING ! la liste est déjà à l'endroit */
        |       Lv6Id TK_EQ OptStruct TK_OPEN_BRACE TypedValuedLv6Ids OptSemicol TK_CLOSE_BRACE
                {
						let typinfo = StructType (make_struct_type_info $1 $5) in
						($1, typinfo)
					 }
        ;


/* COMPATIBILITY : "struct" keyword is optional */
OptStruct: /* ebnf:print=expand */
        /* nothing */ {}
|       TK_STRUCT     {}
;


/* Notation de type "immédiat" */
Type:
                /* prédéfini */
            TK_BOOL  { {src=$1; it=Bool_type_exp } }
        |   TK_INT   { {src=$1; it=Int_type_exp } }
        |   TK_REAL  { {src=$1; it=Real_type_exp } }
                /* ref à un type nommé */
        |       Lv6IdRef  { {src=$1.src; it= Named_type_exp $1.it } }
                /* ou tableau immédiat */
        |       Type TK_HAT Expression
                        { {src=$2; it=Array_type_exp ($1 , $3) } }
        ;


/* extern nodes */

ExtNodeDecl:
  TK_EXTERN TK_FUNCTION Lv6Id Params TK_RETURNS Params OptSemicol
          { treat_external_node false false $3 $4 $6 }
| TK_UNSAFE TK_EXTERN TK_FUNCTION Lv6Id Params TK_RETURNS Params OptSemicol
          { treat_external_node true false $4 $5 $7 }
| TK_EXTERN TK_NODE     Lv6Id Params TK_RETURNS Params OptSemicol
          { treat_external_node false true $3 $4 $6 }
| TK_UNSAFE TK_EXTERN TK_NODE     Lv6Id Params TK_RETURNS Params OptSemicol
          { treat_external_node true true  $4 $5 $7 }
;

/* noeuds */

NodeDecl: LocalNode {};

LocalNode:
		TK_NODE Lv6Id StaticParams Params TK_RETURNS Params OptSemicol
                 LocalDecls Body OptEndNode
        { treat_node_decl false true $2 $3 $4 $6 $8 (fst $9) (snd $9) }
	|	TK_FUNCTION Lv6Id StaticParams Params TK_RETURNS Params OptSemicol
                 LocalDecls Body OptEndNode
        { treat_node_decl false false $2 $3 $4 $6 $8 (fst $9) (snd $9) }
	|	TK_NODE Lv6Id StaticParams NodeProfileOpt TK_EQ EffectiveNode OptSemicol
        { treat_node_alias false true $2 $3 $4 $6 }
	|	TK_FUNCTION Lv6Id StaticParams NodeProfileOpt TK_EQ EffectiveNode OptSemicol
        { treat_node_alias false false $2 $3 $4 $6 }
   |	TK_UNSAFE TK_NODE Lv6Id StaticParams Params TK_RETURNS Params OptSemicol
                 LocalDecls Body OptEndNode
        { treat_node_decl true true $3 $4 $5 $7 $9 (fst $10) (snd $10) }
	|	TK_UNSAFE TK_FUNCTION Lv6Id StaticParams Params TK_RETURNS Params OptSemicol
                 LocalDecls Body OptEndNode
        { treat_node_decl true false  $3 $4 $5 $7 $9 (fst $10) (snd $10) }
	|	TK_UNSAFE TK_NODE Lv6Id StaticParams NodeProfileOpt TK_EQ EffectiveNode OptSemicol
        { treat_node_alias true true $3 $4 $5 $7 }
	|	TK_UNSAFE TK_FUNCTION Lv6Id StaticParams NodeProfileOpt TK_EQ EffectiveNode OptSemicol
        { treat_node_alias true false $3 $4 $5 $7 }
	;

NodeProfileOpt :
        /* nada */
        { None }
	|	Params TK_RETURNS Params
        {
                let invars = clocked_ids_to_var_infos VarInput $1 in
                let outvars = clocked_ids_to_var_infos VarOutput $3 in
                Some (invars, outvars)
        }
	;

StaticParams: /*rien*/
                { [] }
		|  TK_OPEN_STATIC_PAR StaticParamList TK_CLOSE_STATIC_PAR
                { (List.rev $2) }
		;
StaticParamList:
                StaticParam
                        { [$1] }
        |       StaticParamList TK_SEMICOL StaticParam
                        { $3::$1 }
        ;
StaticParam:
                TK_TYPE Lv6Id
                        { {it=(StaticParamType (Lxm.id $2)); src=$2} }
        |       TK_CONST Lv6Id TK_COLON Type
                        { {it=(StaticParamConst (Lxm.id $2 , $4)); src=$2} }
        |       TK_NODE Lv6Id Params TK_RETURNS Params
        				{
                		let invars = clocked_ids_to_var_infos VarInput $3 in
                		let outvars = clocked_ids_to_var_infos VarOutput $5 in
                		let xn = StaticParamNode (
                        		Lxm.id $2,
                        		invars,
                        		outvars,
                        		true,
                              true
                		) in
                		Lxm.flagit xn $2
        				}
        |       TK_FUNCTION Lv6Id Params TK_RETURNS Params
        {
                let invars = clocked_ids_to_var_infos VarInput $3 in
                let outvars = clocked_ids_to_var_infos VarOutput $5 in
                let xn = StaticParamNode (
                        Lxm.id $2,
                        invars,
                        outvars,
                        false,
                        true
                ) in
                Lxm.flagit xn $2
        }
        |       TK_UNSAFE TK_NODE Lv6Id Params TK_RETURNS Params
        				{
                		let invars = clocked_ids_to_var_infos VarInput $4 in
                		let outvars = clocked_ids_to_var_infos VarOutput $6 in
                		let xn = StaticParamNode (
                        		Lxm.id $3,
                        		invars,
                        		outvars,
                        		true,
                              false
                		) in
                		Lxm.flagit xn $3
        				}
        |       TK_UNSAFE TK_FUNCTION Lv6Id Params TK_RETURNS Params
        {
                let invars = clocked_ids_to_var_infos VarInput $4 in
                let outvars = clocked_ids_to_var_infos VarOutput $6 in
                let xn = StaticParamNode (
                        Lxm.id $3,
                        invars,
                        outvars,
                        false,
                        false
                ) in
                Lxm.flagit xn $3
        }
		;
/* Le "." à la fin des noeuds est une fioriture historique,
        On accepte donc '.' ';' ou rien du tout !
*/
OptEndNode: /* ebnf:print=expand */
                TK_DOT
                        {}
        |       OptSemicol
                        {}
        ;

/* Aucune difference entre params d'entrée et les autres */
/* params de sortie :
type: sx_Param list = (((Lxm.t list) * type_exp) list * AstCore.clock_exp) list
*/

Params:
                /* rien */
                TK_OPEN_PAR TK_CLOSE_PAR
                        { [] }
        |
                TK_OPEN_PAR VarDeclList OptSemicol TK_CLOSE_PAR
                  /* WARNING ! il faut remettre la liste à l'endroit */
                        { (List.rev $2) }
        ;

/* DÃ©clarations ocales(2010/07/02)
concret: liste de var(s) ou const
abstrait: couple liste de vars * list de consts
*/
LocalDecls:
	/* nada */
	{ ([],[]) }
|	LocalDeclList
	{ $1 }
;

LocalDeclList:
	OneLocalDecl
		{ $1 }
|  LocalDeclList OneLocalDecl
		{
			match ($1,$2) with
			|	( (vl1, cl1) , (vl2, cl2)) ->
				( vl2 @ vl1, cl2 @ cl1)
		}
;

OneLocalDecl:
	LocalVars
		{ ($1, []) }
|	LocalConsts
		{ ([], $1) }
;

/* DÃ©claration de constantes locale (2010/07/02)
uniquement des constantes dÃ©finies
*/
LocalConsts:
TK_CONST ConstDeclList
	{ $2 }
;

/* variables locales */
LocalVars:
	TK_VAR VarDeclList TK_SEMICOL
	/* WARNING ! il faut remettre la liste à l'endroit */
	{ (List.rev $2) }
;


/* liste de déclarations de vars typées et clockées */
VarDeclList: VarDecl
                        { [$1] }
        |    VarDeclList TK_SEMICOL VarDecl
                        { $3::$1 }
        ;

/* déclaration de vars éventuellement clockées */
VarDecl:
        /*
                Pas de clock : sous-entendu sur la base
                exemple: x, ..., z : type
        */
                TypedLv6Ids
                { ([$1], Base) }
        |
        /*
                Clock explicite sur UNE seule liste d'idents typés
                exemple: x, ..., z : type when clock
        */
                TypedLv6Ids TK_WHEN ClockExpr
                { ([$1], $3) }
        |
        /*
                Clock explicite sur PLUSIEURS listes d'idents typés
                exemple: (x,..,z : t1 ; a,...,b : t2) when clock
        */
                TK_OPEN_PAR TypedLv6IdsList TK_CLOSE_PAR TK_WHEN ClockExpr
                /* WARNING ! il faut remettre la liste à l'endroit */
                { ( (List.rev $2), $5 ) }
        ;

/* Corps d'un noeud */
/*
Retourne un couple (assertions list, equations list)
*/
Body:
      TK_LET TK_TEL
                { ([], []) }
        |   TK_LET EquationList TK_TEL
        /* WARNING ! il faut remettre les listes à l'endroit */
                { (List.rev (fst $2) , List.rev (snd $2)) }
        ;

/* Equations */

EquationList:   Equation
        		{ $1 }
        |   EquationList Equation
        		{ ( (fst $2) @ (fst $1) , (snd $2) @ (snd $1) ) }
        ;
Equation:   TK_ASSERT Expression TK_SEMICOL
        		{ ( [ {src = $1; it = $2} ] , [] ) }
        |   Left TK_EQ Expression TK_SEMICOL
        		{ ( [] , [ {src = $2; it = ($1, $3) } ] ) }
        ;


/* partie gauche d'equation */

Left:   LeftItemList
        /* WARNING ! il faut remettre la liste à l'endroit */
        		{ (List.rev $1) }
        |   TK_OPEN_PAR LeftItemList TK_CLOSE_PAR
        /* WARNING ! il faut remettre la liste à l'endroit */
        		{ (List.rev $2) }
        ;

LeftItemList:   LeftItem
        		{ [$1] }
        |   LeftItemList TK_COMA LeftItem
        		{ $3::$1 }
        ;

LeftItem: Lv6Id
        		{ LeftVar ( {src = $1; it = Lxm.id $1} ) }
        |   FieldLeftItem
        		{ $1 }
        |   TableLeftItem
        		{ $1 }
        ;


FieldLeftItem: LeftItem TK_DOT Lv6Id
        { LeftField ($1 , {src = $3; it = Lxm.id $3} ) }
        ;

TableLeftItem:
            LeftItem TK_OPEN_BRACKET Expression TK_CLOSE_BRACKET
        		{ LeftArray ($1 , {src = $2; it = $3})  }
        |   LeftItem TK_OPEN_BRACKET Select TK_CLOSE_BRACKET
        		{ LeftSlice ($1, $3 ) }
        ;



/* partie droite d'equation (expression) */
Expression:
        /* zéroaires */
            Constant { $1 }
        |   Lv6IdRef    { leafexp $1.src (IDENT_n $1.it) }
        /* unaires */
        |   TK_NOT Expression      { unexp_predef $1 NOT_n $2 }
        |   TK_MINUS Expression %prec TK_UMINUS
                                     { unexp_predef $1 UMINUS_n $2 }
        |   TK_PRE Expression      { unexp $1 PRE_n $2 }
        |   TK_CURRENT Expression  { unexp $1 CURRENT_n $2 }
        |   TK_INT Expression      { unexp_predef $1 REAL2INT_n $2 }
        |   TK_REAL Expression     { unexp_predef $1 INT2REAL_n $2 }
         /* binaires */
        |  Expression TK_WHEN ClockExpr
				 { unexp $2 (WHEN_n $3) $1 }
        |  Expression TK_FBY  Expression  { binexp $2 FBY_n $1 $3 }
        |  Expression TK_ARROW Expression { binexp $2 ARROW_n $1 $3 }
        |  Expression TK_AND   Expression { binexp_predef $2 AND_n  $1 $3 }
        |  Expression TK_OR    Expression { binexp_predef $2 OR_n  $1 $3 }
        |  Expression TK_XOR   Expression { binexp_predef $2 XOR_n  $1 $3 }
        |  Expression TK_IMPL  Expression { binexp_predef $2 IMPL_n  $1 $3 }
        |  Expression TK_EQ    Expression { binexp_predef $2 EQ_n  $1 $3 }
        |  Expression TK_NEQ   Expression { binexp_predef $2 NEQ_n  $1 $3 }
        |  Expression TK_LT    Expression { binexp_predef $2 LT_n  $1 $3 }
        |  Expression TK_LTE   Expression { binexp_predef $2 LTE_n  $1 $3 }
        |  Expression TK_GT    Expression { binexp_predef $2 GT_n  $1 $3 }
        |  Expression TK_GTE   Expression { binexp_predef $2 GTE_n  $1 $3 }
        |  Expression TK_DIV   Expression { binexp_predef $2 DIV_n  $1 $3 }
        |  Expression TK_MOD   Expression { binexp_predef $2 MOD_n  $1 $3 }
        |  Expression TK_MINUS Expression { binexp_predef $2 MINUS_n  $1 $3 }
        |  Expression TK_PLUS  Expression { binexp_predef $2 PLUS_n  $1 $3 }
        |  Expression TK_SLASH Expression { binexp_predef $2 SLASH_n  $1 $3 }
        |  Expression TK_STAR  Expression { binexp_predef $2 TIMES_n  $1 $3 }
        /* ternaires */
        |  TK_IF Expression TK_THEN Expression TK_ELSE Expression
                        { ternexp_predef $1 IF_n $2 $4 $6 }
        |  TK_WITH Expression TK_THEN Expression TK_ELSE Expression
            { CallByPos( {src = $1 ; it = WITH_n($2, $4, $6) }, Oper [] ) }
        /* n-aires */
        /* WARNING ! il faut remettre la liste à l'endroit */
        |  TK_DIESE TK_OPEN_PAR ExpressionList TK_CLOSE_PAR
                        { naryexp_predef $1 DIESE_n (List.rev $3) }
        |  TK_NOR TK_OPEN_PAR ExpressionList TK_CLOSE_PAR
                        { naryexp_predef $1 NOR_n (List.rev $3) }
        | CallByPosExpression
                        { $1 }
        /* Opérations sur les tableaux */
        /* -> création à partir d'une liste */
        |  TK_OPEN_BRACKET ExpressionList TK_CLOSE_BRACKET
                        { naryexp $1 ARRAY_n (List.rev $2) }
        /* -> création par exponentiation */
        |  Expression TK_HAT Expression { binexp $2 HAT_n $1 $3 }
        /* -> concaténation */
        |  Expression TK_BAR Expression { binexp $2 CONCAT_n $1 $3 }
        /* -> accès à un élément */
        |  Expression TK_OPEN_BRACKET Expression TK_CLOSE_BRACKET
                        { unexp $2 (ARRAY_ACCES_n $3) $1 }
        /* -> accès à une tranche */
        |  Expression TK_OPEN_BRACKET Select TK_CLOSE_BRACKET
                        { unexp $3.src (ARRAY_SLICE_n $3.it) $1 }
        /* Acces aux structures */
        |  Expression TK_DOT Lv6Id
                        { unexp $2 (STRUCT_ACCESS_n (Lxm.id $3)) $1 }
        /* Appels par noms */
        |       CallByNameExpression
                        { $1 }
        /* Parenthesis or tuple */
        |  TK_OPEN_PAR ExpressionList TK_CLOSE_PAR
				{ if (List.length $2 = 1) then (List.hd $2) else naryexp $1 TUPLE_n (List.rev $2) }
        /* merge */
        |  TK_MERGE Lv6Id MergeCaseList
                { make_merge_op $2 $3 }
        ;

MergeCaseList:
        |        MergeCase
                { [$1] }
        |       MergeCaseList MergeCase
                { $2::$1 }
        ;

MergeCase:
        |  TK_OPEN_PAR Lv6IdRef TK_ARROW Expression TK_CLOSE_PAR
           { (Idref $2.it,$2.src,$4) }
        |  TK_OPEN_PAR TK_TRUE TK_ARROW Expression TK_CLOSE_PAR
           { (Bool true, $2,$4) }
        |  TK_OPEN_PAR TK_FALSE TK_ARROW Expression TK_CLOSE_PAR
           { (Bool false, $2,$4) }
        ;

ClockExpr:
    Lv6IdRef TK_OPEN_PAR Lv6Id TK_CLOSE_PAR
                   { (make_clock_exp $1.it $3) }
  |        Lv6Id { (make_clock_exp (Lv6Id.idref_of_string "Lustre::true")  $1) }
  | TK_NOT Lv6Id { (make_clock_exp (Lv6Id.idref_of_string "Lustre::false") $2) }
  | TK_NOT TK_OPEN_PAR Lv6Id TK_CLOSE_PAR
                   { (make_clock_exp (Lv6Id.idref_of_string "Lustre::false") $3) }
  ;

PredefOp: /* ebnf:print=short */
            TK_NOT    { make_predef_posop $1 NOT_n }
        |   TK_FBY    { {src=$1; it=FBY_n} }
        |   TK_PRE    { {src=$1; it=PRE_n} }
        |   TK_CURRENT{ {src=$1; it=CURRENT_n} }
        |   TK_ARROW  { {src=$1; it=ARROW_n} }

        |   TK_AND    { make_predef_posop $1 AND_n }
        |   TK_OR     { make_predef_posop $1 OR_n }
        |   TK_XOR    { make_predef_posop $1 XOR_n }
        |   TK_IMPL   { make_predef_posop $1 IMPL_n }
        |   TK_EQ     { make_predef_posop $1 EQ_n }
        |   TK_NEQ    { make_predef_posop $1 NEQ_n }
        |   TK_LT     { make_predef_posop $1 LT_n }
        |   TK_LTE    { make_predef_posop $1 LTE_n }
        |   TK_GT     { make_predef_posop $1 GT_n }
        |   TK_GTE    { make_predef_posop $1 GTE_n }
        |   TK_DIV    { make_predef_posop $1 DIV_n }
        |   TK_MOD    { make_predef_posop $1 MOD_n }
        |   TK_MINUS  { make_predef_posop $1 MINUS_n }
        |   TK_PLUS   { make_predef_posop $1 PLUS_n }
        |   TK_SLASH  { make_predef_posop $1 SLASH_n }
        |   TK_STAR   { make_predef_posop $1 TIMES_n }
        |   TK_IF     { make_predef_posop $1 IF_n }
;

/* nothing to do here !!!
        |   TK_WHEN ClockExpr  { {src=$1; it=(WHEN_n $2)} }
        |   TK_WHEN   { {src=$1; it=(WHEN_n Base)} }
*/

/* Appel fonctionnel par position (classique) */
/* NB
        On a 2 règles à cause des appels échantillonné
*/
CallByPosExpression:
/*
            EffectiveNode TK_OPEN_PAR Expression TK_CLOSE_PAR
                { naryexp $1.src (CALL_n $1) [$3] }
        |
*/
				EffectiveNode TK_OPEN_PAR ExpressionList TK_CLOSE_PAR
                { naryexp $1.src (CALL_n $1) (List.rev $3) }
        ;

/* Effective node : une constrcution qui designe un noeud */

EffectiveNode:
        /* Juste un nom */
                Lv6IdRef
                        { {src=$1.src; it=(($1.it, [])) } }
        /* Un nom + des params statiques */
        |       Lv6IdRef TK_OPEN_STATIC_PAR StaticArgList TK_CLOSE_STATIC_PAR
                        { {src=$1.src; it=(($1.it, List.rev $3)) }      }
        /* Un operateur prédéfini
        |       TK_OPERATOR PredefOp,[]
                        { {src=$; it=($2.it, []) } }
XXX pour l'instant, j'enleve la possibilité d'avoir
(operator +(1,2)). On verra ca plus tard
*/
        ;

StaticArgList:
                StaticArg
                { [$1] }
        |       StaticArgList TK_COMA StaticArg
                { $3::$1 }
        /* let's be permissive... */
        |       StaticArgList TK_SEMICOL StaticArg
                { $3::$1 }
        ;

/* Faut se tordre l'esprit ici !
- la nature est explicite,
          - la nature est immediate (type, const ou node predefini)
          - la nature est sans ambiguite const (expressions simples)
          - la nature est compile-time (juste un ident, a résoudre)
          */

StaticArg:
          /* nature explicite */
          TK_TYPE Type
            { {src=$1 ; it=StaticArgType $2 } }
        | TK_CONST Expression
            { {src=$1 ; it=StaticArgConst $2 } }
        | TK_NODE EffectiveNode
            { {src=$1 ; it=StaticArgNode (CALL_n $2) } }
        | TK_FUNCTION EffectiveNode
            { {src=$1 ; it=StaticArgNode (CALL_n $2) } }
        | PredefOp
                        { {src=$1.src; it=StaticArgNode $1.it } }
          /* un ident OU une expression simple (à résoudre) */
          /* c'est au retour qu'on choisit */
        | SimpleExp
            {
              match $1 with
                | CallByPos (op, x) -> (
                    match op.it with
                      | IDENT_n idref -> {src=op.src ; it = StaticArgLv6Id idref }
                      | _ -> {src=op.src ; it= StaticArgConst $1}
                  )
                | Merge_bool_n _
                | Merge_n _
                | CallByName _ ->
                    print_string "*** unexpected static argument
";
                    assert false
            }
                /* un type sans ambiguite */
        |       SurelyType
                        { {src=$1.src; it=StaticArgType $1} }
                /* un node sans ambiguite */
        |       SurelyNode
                        { {src=$1.src; it=StaticArgNode (CALL_n $1)} }
        ;

/* for model arguments (copy-pasted from call by position StaticArg */
ByNameStaticArgList:
                ByNameStaticArg
                { [$1] }
        |       ByNameStaticArgList TK_COMA ByNameStaticArg
                { $3::$1 }
        /* let's be permissive... */
        |       ByNameStaticArgList TK_SEMICOL ByNameStaticArg
                { $3::$1 }
        ;

/* Faut se tordre l'esprit ici !
- la nature est explicite,
          - la nature est immediate (type, const ou node predefini)
          - la nature est sans ambiguite const (expressions simples)
          - la nature est compile-time (juste un ident, a résoudre)
          */

ByNameStaticArg:
          /* nature explicite */
          TK_TYPE Lv6Id TK_EQ Type
            {  (Lxm.id $2, {src=$1 ; it= StaticArgType $4 }) }
        | TK_CONST Lv6Id TK_EQ Expression
            {  (Lxm.id $2, {src=$1 ; it= StaticArgConst $4 }) }
        | TK_NODE Lv6Id TK_EQ EffectiveNode
            {  (Lxm.id $2, {src=$1 ; it= StaticArgNode (CALL_n $4) }) }
        | TK_FUNCTION Lv6Id TK_EQ EffectiveNode
            {  (Lxm.id $2, {src=$1 ; it= StaticArgNode (CALL_n $4) }) }



        | Lv6Id TK_EQ PredefOp
            { Lxm.id $1, {src=$3.src; it=StaticArgNode $3.it } }
          /* un ident OU une expression simple (à résoudre) */
          /* c'est au retour qu'on choisit */
        | Lv6Id TK_EQ SimpleExp
            { Lxm.id $1,
              match $3 with
                | CallByPos (op, x) -> (
                    match op.it with
                      | IDENT_n idref -> {src=op.src ; it = StaticArgLv6Id idref }
                      | _ -> {src=op.src ; it= StaticArgConst $3}
                  )
                | Merge_bool_n _
                | Merge_n _
                | CallByName _ ->
                    print_string "*** unexpected static argument
";
                    assert false
            }
                /* un type sans ambiguite */
        | Lv6Id TK_EQ SurelyType
                        { Lxm.id $1, {src=$3.src; it=StaticArgType $3} }
                /* un node sans ambiguite */
        | Lv6Id TK_EQ SurelyNode
                        { Lxm.id $1, {src=$3.src; it=StaticArgNode (CALL_n $3)} }
        ;

SurelyNode: Lv6IdRef TK_OPEN_STATIC_PAR StaticArgList TK_CLOSE_STATIC_PAR
                 { {src=$1.src; it=($1.it, List.rev $3) }        }
        ;

SurelyType:
                /* prédéfini */
            TK_BOOL  { {src=$1; it=Bool_type_exp} }
        |   TK_INT   { {src=$1; it=Int_type_exp} }
        |   TK_REAL  { {src=$1; it=Real_type_exp} }
                /* ou tableau immédiat */
        |       SurelyType TK_HAT Expression
                        { {src=$1.src; it = Array_type_exp ($1 , $3) } }
        ;

/* SimpleExp = (hopefuly) statically evaluable exp */
SimpleExp:
           Constant { $1 }
        |  Lv6IdRef    { leafexp $1.src (IDENT_n $1.it) }
        |  SimpleTuple { $1 }
        |  TK_NOT SimpleExp      { unexp_predef $1 NOT_n $2 }
        |  TK_MINUS SimpleExp %prec TK_UMINUS { unexp_predef $1 UMINUS_n $2 }

        |  SimpleExp TK_AND   SimpleExp { binexp_predef $2 AND_n  $1 $3 }
        |  SimpleExp TK_OR    SimpleExp { binexp_predef $2 OR_n  $1 $3 }
        |  SimpleExp TK_XOR   SimpleExp { binexp_predef $2 XOR_n  $1 $3 }
        |  SimpleExp TK_IMPL  SimpleExp { binexp_predef $2 IMPL_n  $1 $3 }
        |  SimpleExp TK_EQ    SimpleExp { binexp_predef $2 EQ_n  $1 $3 }
        |  SimpleExp TK_NEQ   SimpleExp { binexp_predef $2 NEQ_n  $1 $3 }
        |  SimpleExp TK_LT    SimpleExp { binexp_predef $2 LT_n  $1 $3 }
        |  SimpleExp TK_LTE   SimpleExp { binexp_predef $2 LTE_n  $1 $3 }
        |  SimpleExp TK_GT    SimpleExp { binexp_predef $2 GT_n  $1 $3 }
        |  SimpleExp TK_GTE   SimpleExp { binexp_predef $2 GTE_n  $1 $3 }
        |  SimpleExp TK_DIV   SimpleExp { binexp_predef $2 DIV_n  $1 $3 }
        |  SimpleExp TK_MOD   SimpleExp { binexp_predef $2 MOD_n  $1 $3 }
        |  SimpleExp TK_MINUS SimpleExp { binexp_predef $2 MINUS_n  $1 $3 }
        |  SimpleExp TK_PLUS  SimpleExp { binexp_predef $2 PLUS_n  $1 $3 }
        |  SimpleExp TK_SLASH SimpleExp { binexp_predef $2 SLASH_n  $1 $3 }
        |  SimpleExp TK_STAR  SimpleExp { binexp_predef $2 TIMES_n  $1 $3 }
        /* ternaires */
        |  TK_IF SimpleExp TK_THEN SimpleExp TK_ELSE SimpleExp
                        { ternexp_predef $1 IF_n $2 $4 $6 }
        ;

SimpleTuple:
	| TK_OPEN_PAR SimpleExpList  TK_CLOSE_PAR
	{
		(match $2 with
		| [x] -> x
		| l  -> naryexp $1 TUPLE_n (List.rev l) )
	}
   ;
SimpleExpList: SimpleExp
	 	{ [$1] }
	|   SimpleExpList TK_COMA SimpleExp
		{ $3::$1 }
	;

/* Appel fonctionnel par nom */
/* NB
Actuellement, uniquement pour les structures,
donc pas de soucis d'échantillonnage
*/
CallByNameExpression:
        /* WARNING ! il faut remettre la liste à l'endroit */
        | Lv6IdRef TK_OPEN_BRACE CallByNameParamList OptSemicol TK_CLOSE_BRACE
                { bynameexp $1.src (STRUCT_n $1.it) (List.rev $3) }
        | Lv6IdRef TK_OPEN_BRACE Lv6IdRef TK_WITH CallByNameParamList OptSemicol TK_CLOSE_BRACE
                { bynameexp $1.src (STRUCT_WITH_n ($1.it,$3.it)) (List.rev $5) }
        /* on peut avoir une liste vide */
        | Lv6IdRef TK_OPEN_BRACE TK_CLOSE_BRACE
                { bynameexp $1.src (STRUCT_n $1.it) ([]) }
        /* COMPATIBILITY : immediate "struct" without the type name
        | TK_OPEN_BRACE CallByNameParamList OptSemicol TK_CLOSE_BRACE
                { bynameexp $1 STRUCT_anonymous_n (List.rev $2) } */
        ;

CallByNameParamList:
                CallByNameParam
                        { [$1] }
        |
                CallByNameParamList sepVariant CallByNameParam
                        { $3::$1 }
        ;

/* COMPATIBILITY : ',' or ';'  */
sepVariant: /* ebnf:print=expand */
        TK_SEMICOL
                {}
|       TK_COMA
                { Lv6errors.warning $1 "separator mismatch, ';' expected"}
;


CallByNameParam:
                Lv6Id TK_EQ Expression
                        { ({it=Lxm.id $1;src=$1} , $3) }
        ;

/* WARNING ! : les listes sont créées à l'envers */
ExpressionList:
		/* empty */
        { [] }
        |   Expression
                        { [$1] }
        |   ExpressionList TK_COMA Expression
                        { $3::$1 }
        ;

Constant: /* ebnf:print=short */
				TK_TRUE
                        { (leafexp_predef $1 TRUE_n) }
        |   TK_FALSE
                        { (leafexp_predef $1 FALSE_n) }
        |   IntConst
                        { (leafexp_predef $1 (ICONST_n (Lxm.id $1))) }
        |   RealConst
                        { (leafexp_predef $1 ((RCONST_n (Lxm.id $1)))) }
        ;

IntConst: /* ebnf:print=ignore */
	TK_ICONST
		{ $1 }
	;

RealConst: /* ebnf:print=ignore */
	TK_RCONST
		{ $1 }
	;

Select:
           Expression TK_CDOTS Expression Step
                { {it={si_first = $1; si_last = $3 ; si_step = $4 }; src = $2} }
        ;

Step: /* empty */
                        { None }
        |   TK_STEP Expression
                        { Some $2 }
        ;
/* NB
AstV6 laxiste des listes :
quand il n'y a pas d'ambiguité,
les ";" sont vus indifferemment commme
des séparateurs ou des terminateurs
*/
OptSemicol : /* ebnf:print=expand */
                /* empty */
                {}
        |   TK_SEMICOL
                {}
        ;

Pragma: /* e.g., %ASSUME:toto% */
        { [] } /* produces 3 shift reduce conflicts! */
|       TK_PCENT TK_IDENT TK_COLON TK_IDENT TK_PCENT  Pragma
                { (Pragma(Lxm.str $2, Lxm.str $4))::$6 }

;

%%
