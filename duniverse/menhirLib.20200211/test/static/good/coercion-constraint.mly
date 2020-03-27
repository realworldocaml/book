/* $Header: /home/pauillac/cristal2/regisgia/cvsroot/gadt/src/miniParser.mly,v 1.9 2004/10/07 14:17:35 regisgia Exp $ */

%{

open Sig
open Positions
open AstPositions
open MiniAst

let fold_pair f ts =
  match ts with
    | a :: b :: q -> List.fold_left f (f a b) q
    | _ -> assert false

let tuple2 pos t1 t2 =
  EDCon (pos, "_Tuple", [ t1; t2 ])

let tuple pos =
  fold_pair (tuple2 pos)

let arrow_type pos t1 t2 =
  TypApp (pos, TypVar (pos, "->"), [ t1; t2 ])

let tuple_type2 pos t1 t2 =
  TypApp (pos, TypVar (pos, "*"), [ t1; t2 ])

let tuple_type pos =
  fold_pair (tuple_type2 pos)

let unclosed b e l1 l2 =
  let l1 = lex_join (Parsing.rhs_start_pos l1) (Parsing.rhs_end_pos l1)
  and l2 = lex_join (Parsing.rhs_start_pos l2) (Parsing.rhs_end_pos l2)
  in
    raise (ParsingExceptions.Unclosed (b, e, l1, l2))

let clet envs body =
  fun (tenv, pool) ->
    CLet (envs (tenv, pool), (body (tenv, pool)))

let cexists pos vars c =
  fun (tenv, pool) ->
    let vars = snd (List.split vars) in
    let rqs, fqs, tenv = MiniTypes.intern_let_env pos tenv [] vars in
    CLet ([
      Scheme (pos, rqs, fqs, c (tenv, pool),
	      Misc.StringMap.empty
	     )
	  ], CTrue pos)

let cequation t1 t2 =
  fun (tenv, pool) ->
    let p = tjoin t1 t2
    and it1 = MiniTypes.intern (tposition t1) tenv t1
    and it2 = MiniTypes.intern (tposition t2) tenv t2 in
      CEquation (p, it1, it2)

let cinstance (p1, id) t =
  fun (tenv, _) ->
    let p = join p1 (tposition t) in
    CInstance (p, id, MiniTypes.intern (tposition t) tenv t)

let scheme pos (rvs, fvs) c g =
  fun (tenv, pool) ->
    let rqs, fqs, tenv = MiniTypes.intern_let_env pos tenv rvs fvs in
      Scheme (pos, rqs, fqs, c (tenv, pool),
              List.fold_left
                (fun m (n, ty) ->
                   let t = MiniTypes.intern pos tenv ty in
                     Misc.StringMap.add n (t, pos) m)
                Misc.StringMap.empty
                g
             )

let appl y =
  List.map (fun x -> x y)

let conjunction cs =
  fun p -> CConjunction (appl p cs)

let mkArrow (p1, t1) (p2, t2) =
  let p = join p1 p2 in
  (p, TypApp (p, TypVar (p, "->"), [t1; t2]))

let mkApp (p, t) ts =
  let p' = join p (ljoinf fst ts) in
  let _, ts = List.split ts in
    (p', TypApp (p', t, ts))

let mkRow r =
  assert false

let typeid (p, id) =
    (p, TypVar (p, id))

type row =
    Partial of MiniAst.typ
  | Row of (string * MiniAst.typ)

%}

%token <Positions.position> EOF
%token <Positions.position * string> LID
%token <Positions.position> LET
%token <Positions.position> IN
%token <Positions.position> DOT
%token <Positions.position> LBRACE
%token <Positions.position> RBRACE
%token <Positions.position> LBRACKET
%token <Positions.position> RBRACKET
%token <Positions.position> LESS
%token <Positions.position> EXISTS
%token <Positions.position> FORALL
%token <Positions.position> ARROW
%token <Positions.position> TIMES
%token <Positions.position> LPAREN
%token <Positions.position> RPAREN
%token <Positions.position> SEMI
%token <Positions.position> COMMA
%token <Positions.position> AND
%token <Positions.position> COLON
%token <Positions.position> END
%token <Positions.position> TRUE
%token <Positions.position> FALSE
%token <Positions.position> DUMP
%token <Positions.position> EQ
%token <Positions.position> BACKSLASH

%nonassoc EOF
%nonassoc AND
%right LEFT
%left APP
%nonassoc ID

%start tconstraint
%type <MiniTypingEnvironment.environment * MiniMultiEquation.pool-> MiniConstraint.tconstraint> tconstraint

%%

tconstraint: constraint_exp EOF { $1 }
;

constraint_exp:
  LET let_envs IN constraint_exp        { clet $2 $4 }
| EXISTS vars DOT constraint_exp	{ cexists $1 $2 $4 }
| constraint_exp1 { $1 }
;

constraint_exp1:
 conjunction				{ conjunction $1 }
| constraint_exp0 { $1 }
;

constraint_exp0:
| TRUE				        { fun _ -> CTrue $1 }
| FALSE				        { fun _ -> CFalse $1 }
| DUMP					{ fun _ -> CDump $1 }
| typ EQ typ				{ cequation $1 $3 }
| LID LESS typ				{ cinstance $1 $3 }
| LPAREN constraint_exp RPAREN		{ $2 }
;

let_env:
  opt_env_vars opt_constraint opt_env_ids {
    (* FIXME: Fix positions. *)
    scheme undefined_position $1 $2 $3
  }
;

let_envs:
let_env					 { fun p -> [ $1 p ] }
| let_env AND let_envs			 { fun p -> ($1 p) :: ($3 p) }
;

opt_env_vars:
  /* empty */				 { ([], []) }
| FORALL LBRACE vars RBRACE opt_vars		 { (snd (List.split $3),
					     (snd (List.split $5))) }
| FORALL vars		 { ([], (snd (List.split $2))) }
;

opt_vars: /* empty */ { [] }
| vars { $1 }
;

vars: var				 { [ $1 ] }
| var vars				 { $1 :: $2 }
;

var: LID { $1 }
| ARROW { ($1, "->") }
| TIMES { ($1, "*") }
;

opt_constraint:
  /* empty */
  { fun pool -> CTrue undefined_position }
| LBRACKET constraint_exp RBRACKET       { $2 }
;

opt_env_ids:
  /* empty */				 { [] }
| LPAREN env_ids RPAREN			 { $2 }
;

env_id: LID COLON typ			 { (snd $1, $3) }
;

env_ids: env_id
  { [ $1 ] }
| env_id SEMI env_ids
      { $1 :: $3 }
;

conjunction: constraint_exp0 AND constraint_exp0 { [ $1; $3  ] }
| constraint_exp0 AND conjunction { $1 :: $3 }
;

attributes:
  typ						    { [], $1 }
| attribute SEMI attributes			    { $1 :: (fst $3), snd $3 }
;

attribute:
  LID COLON typ					    { snd $1, $3 }
;

typ:
  type2                                             { $1 }
;

type2:
  type10 ARROW type2
  { arrow_type (tjoin $1 $3) $1 $3  }
| type10                                             { $1 }
;

type10:
 star_types
{ match $1 with
    | [] -> assert false
    | [ a ] -> a
    | l -> tuple_type (tlposition $1) l
}
;

star_types:
  type1 TIMES star_types                             { $1 :: $3 }
| type1						    { [ $1 ] }
;

type1:
  type0 { $1 }
| BACKSLASH type0				    { TypRowUniform ($1, $2) }
;

type0:
 type00s
  {
    match $1 with
	[] -> assert false
      | [ t ] -> t
      | t :: q ->
	  TypApp (join (tposition t)
		    (tlposition q),
		    t,
		    q)
  }
;

type00:
  LID                                               { TypVar (fst $1, snd $1) }
| LBRACE attributes RBRACE			    { TypRowCons
							(join $1 $3,
							 fst $2,
							 snd $2)
						    }
| LPAREN typ RPAREN                                 { $2 }
| LPAREN typ COMMA types RPAREN                     { tuple_type (join $1 $5)
							($2 :: $4) }

;
/* TEMPORARY autoriser les 'equations inline dans les types */

type00s:
type00				            { [ $1 ] }
| type00 type00s			    { $1 :: $2 }
;

types:
  typ                                               { [ $1 ] }
| typ COMMA types                                   { $1 :: $3 }
;

