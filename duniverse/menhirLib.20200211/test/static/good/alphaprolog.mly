/* Yet another attempt to parse nicely.
   This parses to tree-structured preterms with remaining ambiguity as follows:
   1. Commas might be either pair constructors
      (within parens inside terms) or and-constructors
      (outside of propositions).
   2. Identifiers are not resolved to syntactic classes
      (e.g. atom, variable, term symbol).
   These preterms are the input to typechecking,
   Typechecking resolves identifiers to variables, symbols, or atoms
   and translates propositions to goals/program clauses
   and translates terms to internal terms.
*/
%{
open Absyn;;
open Fixity;;
open Nstbl;;

type quantifier = QForall | QExists | QNew;;

let do_quantify q tvs e =
  let do_quantifier q (tv,st) e =
    match q with
      QForall -> Forall(tv,st,e)
    | QExists -> Exists(tv,st,e)
    | QNew -> New(tv,st,e)
  in
  List.fold_right (do_quantifier q) tvs e
;;

let do_literal' s t =
  let n = String.length s in
  let rec go i =
    if i = n then t
    else Cons(CharC(String.get s i), go (i+1))
  in go 0
;;

let do_literal s = do_literal' s Nil;;


type dcg_term = Nonterm of atomic | Char of char | Seq of dcg_term * dcg_term
              | Alt of dcg_term * dcg_term | Goal of term | Literal of string
;;


let translate_dcg (hd,tl) t =
  let rec tr t x y =
    match t with
      Char c -> Eq(Var x, Cons(CharC c, Var y))
    | Literal s -> Eq (Var x, do_literal' s (Var y))
    | Seq (t1,t2) ->
	let w = Var.mkvar "W" in
	And(tr t1 x w, tr t2 w y)
    | Alt(t1,t2) ->
	Or(tr t1 x y, tr t2 x y)
    | Goal(t) ->
	And(Eq(Var x,Var y), t)
    | Nonterm (hd,tl) ->
	Atomic(hd,tl@[Var x; Var y])
  in
  let x = Var.mkvar "X" in
  let y = Var.mkvar "Y" in
  Implies(tr t x y, Atomic(hd,tl@[Var x; Var y]))


let mk_toplevel_decl rdecl =
  {pos=None; rdecl = rdecl}
;;


let mk_decl rdecl =
  {pos=Some (Pos.mk_pos (Lineno.filename()) (Lineno.lineno())); rdecl=rdecl}
;;

exception Eof;;


let rec kd_app ks k =
  match ks with
    [] -> k
  | k'::ks -> ArrowK (k',kd_app ks k)
;;

let rec ty_app ts t =
  match ts with
    [] -> t
  | t'::ts -> ArrowTy (t',ty_app ts t)

%}

%token <int> INT
%token <string> ID LITERAL
%token <Nstbl.path> QUAL_ID
%token <char> CHAR
%token USE TRACE QUIT OPEN TYPEQ HELP
%token NAME_TYPE TYPE INFIXL INFIXR INFIXN NAMESPACE FUNC PRED CNST
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token QUESTION DOT COLON UNDERSCORE TILDE
%token ARROW DCG_ARROW LARROW DARROW COMMA SEMI
%token TRUE NOT CUT BAR HASH SLASH BACKSLASH EQ IS
%token FORALL EXISTS NEW
%token CONS
%token <string> INFIXL1 INFIXL2 INFIXL3 INFIXL4 INFIXL5
%token <string> INFIXL6 INFIXL7 INFIXL8 INFIXL9
%token <string> INFIXR1 INFIXR2 INFIXR3 INFIXR4 INFIXR5
%token <string> INFIXR6 INFIXR7 INFIXR8 INFIXR9
%token <string> INFIXN1 INFIXN2 INFIXN3 INFIXN4 INFIXN5
%token <string> INFIXN6 INFIXN7 INFIXN8 INFIXN9
%token EOF

%type <Absyn.decl list> parse parse_input_line

/* Precedences */
%right DOT
/* 1 */
%left INFIXL1
%right INFIXR1
%nonassoc INFIXN1
%right ARROW DARROW
%left LARROW
%nonassoc SLASH
%nonassoc BAR
/* 2 */
%left INFIXL2
%right INFIXR2
%nonassoc INFIXN2
%right SEMI
/* 3 */
%left INFIXL3
%right INFIXR3
%nonassoc INFIXN3
%right COMMA
/* 4 */
%left INFIXL4
%right INFIXR4
%nonassoc INFIXN4
%nonassoc EQ HASH IS
/* 5 */
%left INFIXL5
%right INFIXR5
%nonassoc INFIXN5
/* 6 */
%left INFIXL6
%right INFIXR6
%nonassoc INFIXN6
%right CONS
/* 7 */
%left INFIXL7
%right INFIXR7
%nonassoc INFIXN7
/* 8 */
%left INFIXL8
%right INFIXR8
%nonassoc INFIXN8
/* 9 */
%left INFIXL9
%right INFIXR9
%nonassoc INFIXN9
/* 10 */
%right BACKSLASH

%start parse
%start parse_input_line

%%

parse: decls EOF                        { List.rev $1 }
;

parse_input_line : goal_decl DOT        { [mk_toplevel_decl $1] }
  | EOF                                 { raise End_of_file }
;

goal_decl  : goal                       { Query($1) }
      | directive                       { $1 }
      | infix_decl                      { $1 }
      ;


decls : decls decl DOT                  { $2::$1 }
      |                                 { [] }
;

decl  : sig_decl                        { mk_decl $1}
      | prog                            { mk_decl (ClauseDecl($1))}
      | QUESTION goal                   { mk_decl(Query($2))}
      | directive                       { mk_decl $1}
      | infix_decl                      { mk_decl $1}
      | NAMESPACE ID LPAREN decls RPAREN
                                        { mk_decl
					     (NamespaceDecl($2,
							    List.rev $4))
					}
;

sig_decl :
        ID COLON kind                   { KindDecl($1,$3) }
      |	ID COLON ty                     { SymDecl($1,$3) }
      | PRED ID ty0s                    { PredDecl($2,List.rev $3) }
      |	CNST ID EQ ty                   { FuncDecl($2,[],$4) }
      | FUNC ID ty0s EQ ty              { FuncDecl($2,List.rev $3, $5) }
      | TYPE ID vars EQ ty              { let rvs = $3 in
                                          TypeDefn($2,List.rev rvs,$5) }
;


vars :                                  { [] }
      | vars ID                         { Var.mkvar' $2::$1 }
;

kind :
       TYPE                             { TypeK }
     | NAME_TYPE                        { NameK }
     | kind ARROW kind                  { ArrowK($1,$3) }

;

directive : USE LITERAL                 { UseDirective($2) }
          | TRACE INT                   { TraceDirective ($2) }
          | TYPEQ preterm               { TypeQuery($2) }
	  | QUIT                        { QuitDirective }
	  | OPEN qual_id                { OpenDirective $2 }
	  | HELP                        { HelpDirective(None) }
;


infix_decl : INFIXN ID INT              { add_sym $2 $3 Non;
			                  InfixDecl(Non,$2,$3) }
	  | INFIXL ID INT               { add_sym $2 $3 Left;
			                  InfixDecl(Left,$2,$3) }
	  | INFIXR ID INT               { add_sym $2 $3 Right;
			                  InfixDecl(Right,$2,$3) }
;

prog: preterm                          { $1 }
	  |  dcg_rule                  { $1 }
;

dcg_rule :  app_or_qid DCG_ARROW dcg_term
                                       { translate_dcg $1 $3 }
	  | app_or_qid DCG_ARROW dcg_term SLASH constr
                                       { Constr(translate_dcg $1 $3,$5) }
;

dcg_term : LBRACE preterm RBRACE       { Goal $2 }
	  | dcg_term COMMA dcg_term    { Seq ($1,$3) }
	  | dcg_term SEMI dcg_term     { Alt($1,$3) }
	  | app_or_qid                 { Nonterm($1) }
	  | CHAR                       { Char $1 }
	  | LITERAL                    { Literal $1 }
;

goal: preterm                          { $1 }
;

quantifier : FORALL                    { QForall }
      | EXISTS                         { QExists }
      | NEW                            { QNew }
;


qv : ID                                 { (Var.mkvar' $1,UnderscoreTy) }
   | ID COLON ty                        { (Var.mkvar' $1,$3) }
;

qlist : qv                              { [$1] }
   | qlist COMMA qv                     { $3::$1 }
;

const : UNDERSCORE                      { Underscore }
   | CUT                                { Cut }
   | TRUE                               { True }
   | LBRACK RBRACK                      { Nil }
   | LPAREN RPAREN                      { Unit }
   | INT                                { IntC $1 }
   | LITERAL                            { do_literal $1 }
   | CHAR                               { CharC $1 }
;

qual_id : QUAL_ID                       { $1 }
   | ID                                 { Rel(Base($1)) }
;

preterm0 : LPAREN preterm RPAREN        { $2 }
   | qual_id                            { Atomic($1,[]) }
   | const                              { $1 }
   | LBRACK preterms_brack RBRACK       { List.fold_right
					    (fun x y ->
					      Cons(x,y))
					    (List.rev $2) Nil }
   | LBRACK preterms_brack BAR preterm_brack RBRACK
                                        { List.fold_right
					    (fun x y ->
					      Cons(x,y))
					    (List.rev $2) $4 }
   | LPAREN ID TILDE ID RPAREN preterm0 { Transpose(Var.mkvar' $2,
						    Var.mkvar' $4,$6) }
;


constr : ID HASH ID                     { [(Var.mkvar' $1,Var.mkvar' $3)] }
   | constr COMMA ID HASH ID            { (Var.mkvar' $3,Var.mkvar' $5)::$1 }
;

preterm : quantifier qlist DOT preterm  { do_quantify $1 (List.rev $2) $4 }
   | ID BACKSLASH preterm               { Abs (Var.mkvar' $1,$3) }
   | preterm ARROW preterm BAR preterm  { Guard($1,$3,$5) }
   | preterm LARROW preterm             { Implies ($3,$1) }
/* Ambiguous betweem "pair" and "and" commas */
   | preterm COMMA preterm              { Pair($1,$3) }
   | preterm SEMI preterm               { Or($1,$3) }
   | preterm SLASH constr               { Constr($1,$3) }
   | preterm HASH preterm               { Fresh($1,$3) }
   | preterm EQ preterm                 { Eq($1,$3) }
   | preterm IS preterm                 { Is($1,$3) }
   | preterm DARROW preterm             { Implies($1,$3) }
   | preterm CONS preterm               { Cons($1,$3) }
   | NOT preterm0                       { Not($2) }
   | infixl_preterm                     { $1 }
   | infixr_preterm                     { $1 }
   | infixn_preterm                     { $1 }
   | app                                { let (h,tl) = $1 in
                                          Atomic(h,List.rev tl) }
   | preterm0                           {$1}
;

preterm_brack :
     ID BACKSLASH preterm_brack         { Abs (Var.mkvar' $1,$3)}
   | preterm_brack CONS preterm_brack   { Cons($1,$3) }
   | app                                { let (h,tl) = $1 in
                                          Atomic(h,List.rev tl) }
   | preterm0                           { $1 }
;

preterms_brack : preterm_brack          { [$1] }
   | preterms_brack COMMA preterm_brack { $3::$1 }
;

infixl_preterm:
     preterm INFIXL1 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXL2 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXL3 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXL4 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXL5 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXL6 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXL7 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXL8 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXL9 preterm            { Atomic(Rel(Base $2),[$1;$3]) }

infixr_preterm :
     preterm INFIXR1 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXR2 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXR3 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXR4 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXR5 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXR6 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXR7 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXR8 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXR9 preterm            { Atomic(Rel(Base $2),[$1;$3]) }

infixn_preterm:
     preterm INFIXN1 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXN2 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXN3 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXN4 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXN5 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXN6 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXN7 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXN8 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
   | preterm INFIXN9 preterm            { Atomic(Rel(Base $2),[$1;$3]) }
;

app : qual_id preterm0                  { ($1,[$2]) }
    | app preterm0                      { let (h,tl) = $1 in
                                          (h,$2::tl) }
;

app_or_qid : app                        { let (hd,tl) = $1 in
	                                  (hd,List.rev tl) }
    | qual_id                           { ($1,[]) }
;

tyapp : qual_id ty0                     { ($1,[$2]) }
      | tyapp ty0                       { let (ds,ss) = $1 in
                                          (ds,$2::ss) }
;

ty :
       ty ARROW ty                      { ArrowTy($1,$3) }
     | tyapp                            { let ds,ss = $1 in
                                          DataTy(ds,List.rev ss) }
     | ty BACKSLASH ty                  { AbsTy($1,$3) }
     | ty0                              { $1 }
;

/* could be more efficient... */
comma_tys : ty                          { $1 }
     | ty COMMA comma_tys               { PairTy($1,$3) }

ty0 : LPAREN comma_tys RPAREN           { $2 }
     | UNDERSCORE                       { UnderscoreTy }
     | qual_id                          { IdTy($1) }
     | LBRACK ty RBRACK                 { ListTy $2 }
     | LPAREN RPAREN                    { UnitTy }
;

ty0s : ty0s ty0                         { $2 :: $1 }
     |                                  { [] }
;
