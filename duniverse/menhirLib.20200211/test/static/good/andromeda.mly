(* Infix operations *)
%token <Name.t * Location.t> QQMARK PREFIXOP EQ INFIXOP0 INFIXOP1 INFIXCONS INFIXOP2 STAR INFIXOP3 INFIXOP4

(* Names and numerals *)
%token UNDERSCORE
%token <Name.t> NAME
%token <int> NUMERAL

(* Parentheses & punctuations *)
%token LPAREN RPAREN
%token LBRACK RBRACK
%token LBRACE RBRACE
%token COLON COMMA PERIOD COLONGT COLONQT
%token ARROW

(* Modules *)
%token MODULE STRUCT

(* Things specific to toplevel *)
%token SEMISEMI
%token RULE

(* Let binding *)
%token LET REC AND IN

(* Meta-level programming *)
%token OPERATION
%token MATCH WHEN END
%token AS TYPE BY
%token EQEQ

%token EXCEPTION RAISE
%token TRY WITH HANDLER BAR VAL
%token SEMI

%token NATURAL

%token EXTERNAL

%token UATOM

(* Meta types *)
%token MLUNIT MLSTRING
%token MLJUDGEMENT MLBOUNDARY MLDERIVATION
%token MLTYPE DARROW
%token MLFORALL
%token OF

(* References *)
%token BANG COLONEQ REF

(* Functions *)
%token FUN

(* TT commands *)
%token FRESH CONVERT CONGRUENCE CONTEXT OCCURS DERIVE ABSTRACT

(* Toplevel directives *)
%token VERBOSITY
%token <string> QUOTED_STRING
%token REQUIRE INCLUDE OPEN

%token EOF

(* Precedence and fixity of infix operators *)
%nonassoc COLONEQ
%left     EQ INFIXOP0
%right    INFIXOP1
%right    INFIXCONS
%left     INFIXOP2
%left     STAR INFIXOP3
%right    INFIXOP4

%start <Sugared.toplevel list> file
%start <Sugared.toplevel> commandline

%%

(* Toplevel syntax *)

file:
  | ds=ml_module EOF
    { ds }

ml_module:
  |
    { [] }

  | c=top_term
    { [c] }

  | c=top_term SEMISEMI cs=ml_module
    { c :: cs }

  | c=top_command SEMISEMI cs=ml_module
    { c :: cs }

  | c=top_command cs=ml_module_top
    { c :: cs }

ml_module_top:
  |
    { [] }

  | c=top_command SEMISEMI cs=ml_module
    { c :: cs }

  | c=top_command cs=ml_module_top
    { c :: cs }

commandline:
  | top_command SEMISEMI? EOF
    { $1 }

  | top_term SEMISEMI? EOF
    { $1 }

(* Toplevel term *)
top_term: mark_location(top_term_) { $1 }
top_term_:
  | t=term
    { Sugared.TopComputation t }

(* Toplevel commands that need not be preceeded by double semicolon. *)
top_command: mark_location(top_command_) { $1 }
top_command_:
  | REQUIRE mdls=separated_nonempty_list(COMMA, module_name)
    { Sugared.Require mdls }

  | INCLUDE mdl=long(module_name)
    { Sugared.Include mdl }

  | OPEN mdl=long(module_name)
    { Sugared.Open mdl }

  | MODULE mdl=module_name EQ STRUCT cmds=ml_module END
    { Sugared.TopModule (mdl, cmds) }

  | LET lst=separated_nonempty_list(AND, let_clause)
    { Sugared.TopLet lst }

  | LET REC lst=separated_nonempty_list(AND, recursive_clause)
    { Sugared.TopLetRec lst }

  | WITH lst=top_operation_cases END
    { Sugared.TopWith lst }

  | MLTYPE t=ml_name xs=list(opt_name(ml_name))
    { Sugared.DefMLTypeAbstract (t, xs) }

  | MLTYPE lst=mlty_defs
    { Sugared.DefMLType lst }

  | MLTYPE REC lst=mlty_defs
    { Sugared.DefMLTypeRec lst }

  | OPERATION op=op_name COLON opsig=op_mlsig
    { Sugared.DeclOperation (op, opsig) }

  | EXCEPTION exc=exc_name
    { Sugared.DeclException (exc, None) }

  | EXCEPTION exc=exc_name OF t=mlty
    { Sugared.DeclException (exc, Some t) }

  | VERBOSITY n=NUMERAL
    { Sugared.Verbosity n }

  | EXTERNAL n=ml_name COLON sch=ml_schema EQ s=QUOTED_STRING
    { Sugared.DeclExternal (n, sch, s) }

  | RULE r=rule_
    { r }


rule_:
  | c=tt_name ps=list(premise) TYPE
    { Sugared.RuleIsType (c, ps) }

  | c=tt_name ps=list(premise) COLON ty=term
    { Sugared.RuleIsTerm (c, ps, ty) }

  | c=tt_name ps=list(premise) COLON l=app_term EQEQ r=ty_term
    { Sugared.RuleEqType (c, ps, (l, r)) }

  | c=tt_name ps=list(premise) COLON l=app_term EQEQ r=app_term COLON ty=term
    { Sugared.RuleEqTerm (c, ps, (l, r, ty)) }

premise: mark_location(premise_) { $1 }
premise_:
  | LPAREN lctx=local_context mv=opt_name(tt_name) TYPE RPAREN
    { Sugared.PremiseIsType (mv, lctx) }

  | LPAREN lctx=local_context mv=opt_name(tt_name) COLON ty=term RPAREN
    { Sugared.PremiseIsTerm (mv, lctx, ty) }

  | LPAREN lctx=local_context l=app_term EQEQ r=ty_term mv=equality_premise_name RPAREN
    { Sugared.PremiseEqType (mv, lctx, (l, r)) }

  | LPAREN lctx=local_context l=app_term EQEQ r=app_term COLON ty=term mv=equality_premise_name RPAREN
    { Sugared.PremiseEqTerm (mv, lctx, (l, r, ty)) }


equality_premise_name:
  |
    { None }

  | BY x=opt_name(tt_name)
    { x }

local_context:
  | lst=list(typed_binder)
    { List.concat lst }


(* Main syntax tree *)

term: mark_location(term_) { $1 }
term_:
  | e=ty_term_
    { e }

  | LET a=separated_nonempty_list(AND,let_clause) IN c=term
    { Sugared.Let (a, c) }

  | LET REC lst=separated_nonempty_list(AND, recursive_clause) IN c=term
    { Sugared.LetRec (lst, c) }

  | MATCH e=term WITH lst=match_cases END
    { Sugared.Match (e, lst) }

  | TRY c=term WITH hcs=handler_cases END
    { Sugared.Try (c, hcs) }

  | FUN xs=ml_arg+ ARROW e=term
    { Sugared.Function (xs, e) }

  | DERIVE ps=nonempty_list(premise) ARROW e=term
    { Sugared.Derive (ps, e) }

  | WITH h=term TRY c=term
    { Sugared.With (h, c) }

  | HANDLER hcs=handler_cases END
    { Sugared.Handler (hcs) }

  | FRESH x=opt_name(ml_name) COLON t=ty_term
    { Sugared.Fresh (x, t) }

  | e=app_term COLONQT bdry=ty_term
    { Sugared.BoundaryAscribe (e, bdry) }

  | e=app_term COLON ty=ty_term
    { Sugared.TypeAscribe (e, ty) }

  | e1=binop_term SEMI e2=term
    { Sugared.Sequence (e1, e2) }

ty_term: mark_location(ty_term_) { $1 }
ty_term_:
  | e=binop_term_
    { e }

  | a=abstraction e=binop_term
    { Sugared.Abstract (a, e) }


binop_term: mark_location(binop_term_) { $1 }
binop_term_:
  | e=app_term_
    { e }

  | e1=app_term COLONEQ e2=binop_term
    { Sugared.Update (e1, e2) }

  | QQMARK TYPE
    { Sugared.MLBoundaryIsType }

  | QQMARK COLON t=app_term
    { Sugared.MLBoundaryIsTerm t }

  | l=app_term EQEQ r=app_term BY p=app_term
    { Sugared.EqTypeAscribe (l, r, p) }

  | l=app_term EQEQ r=app_term COLON ty=app_term BY p=app_term
    { Sugared.EqTermAscribe (l, r, ty, p) }

  | l=app_term EQEQ r=app_term BY QQMARK
    { Sugared.MLBoundaryEqType (l, r) }

  | l=app_term EQEQ r=app_term COLON ty=app_term BY QQMARK
    { Sugared.MLBoundaryEqTerm (l, r, ty) }

  | e1=binop_term oploc=infix e2=binop_term
    { let (op, at) = oploc in
      let op = Location.mark ~at (Sugared.Name (Name.PName op)) in
      Sugared.Spine (op, [e1; e2])
    }

app_term: mark_location(app_term_) { $1 }
app_term_:
  | e=substitution_term_
    { e }

  | RAISE c=substitution_term
    { Sugared.Raise c }

  | CONGRUENCE e1=substitution_term e2=substitution_term es=list(substitution_term)
    { Sugared.Congruence (e1, e2, es) }

  | CONTEXT c=substitution_term
    { Sugared.Context c }

  | CONVERT c1=substitution_term c2=substitution_term
    { Sugared.Convert (c1, c2) }

  | ABSTRACT c1=prefix_term c2=prefix_term
    { Sugared.AbstractAtom (c1, c2) }

  | OCCURS c1=substitution_term c2=substitution_term
    { Sugared.Occurs (c1, c2) }

  | e=substitution_term es=nonempty_list(substitution_term)
    { Sugared.Spine (e, es) }

substitution_term: mark_location(substitution_term_) { $1 }
substitution_term_:
  | e=prefix_term_
    { e }

  | e=substitution_term s=substitution
    { Sugared.Substitute (e, s) }

prefix_term: mark_location(prefix_term_) { $1 }
prefix_term_:
  | e=simple_term_
    { e }

  | REF e=prefix_term
    { Sugared.Ref e }

  | BANG e=prefix_term
    { Sugared.Lookup e }

  | oploc=prefix e2=prefix_term
    { let (op, at) = oploc in
      let op = Location.mark ~at (Sugared.Name (Name.PName op)) in
      Sugared.Spine (op, [e2])
    }

  | NATURAL t=prefix_term
    { Sugared.Natural t }

(* simple_term: mark_location(simple_term_) { $1 } *)
simple_term_:
  | x=long(any_name)
    { Sugared.Name x }

  | s=QUOTED_STRING
    { Sugared.String s }

  | LBRACK lst=list_contents RBRACK
    { Sugared.List lst }

  | LBRACK RBRACK
    { Sugared.List [] }

  | LPAREN c=term COLONGT t=ml_schema RPAREN
    { Sugared.MLAscribe (c, t) }

  | LPAREN lst=separated_list(COMMA, term) RPAREN
    { match lst with
      | [{Location.it=e;_}] -> e
      | _ -> Sugared.Tuple lst }

list_contents:
  | t=binop_term SEMI?
    { [t] }

  | t=binop_term SEMI lst=list_contents
    { t::lst }


(* ML variable name *)
ml_name:
  | NAME
    { $1 }

  | LPAREN op=infix RPAREN
    { fst op }

  | LPAREN op=prefix RPAREN
    { fst op }


(* ML operation name *)
op_name:
  | NAME
    { $1 }

(* ML exception name *)
exc_name:
  | NAME
    { $1 }


(* ML module name *)
module_name:
  | NAME
    { $1 }


(* Type theory variable name *)
tt_name:
  | NAME
    { $1 }

  | LPAREN op=infix RPAREN
    { fst op }

  | LPAREN op=prefix RPAREN
    { fst op }


(* ML or type theory name *)
any_name:
  | tt_name
    { $1 }


(* ML constructor *)
constr_name:
  | NAME
    { $1 }

  | LPAREN op=infix RPAREN
    { fst op }

  | LPAREN op=prefix RPAREN
    { fst op }


module_path:
  | mdl=module_name
    { Name.PName mdl }

  | pth=module_path PERIOD mdl=module_name
    { Name.PModule (pth, mdl) }


(* Possibly a name qualified with a module *)
%inline long(N):
  | x=N
    { Name.PName x }

  | pth=module_path PERIOD x=N
    { Name.PModule (pth, x) }


(* Infix operators *)
%inline infix:
  | op=INFIXCONS
    { op }

  | op=EQ
    { op }

  | op=INFIXOP0
    { op }

  | op=INFIXOP1
    { op }

  | op=INFIXOP2
    { op }

  | op=INFIXOP3
    { op }

  | op=STAR
    { op }

  | op=INFIXOP4
    { op }

(* Prefix operators *)
%inline prefix:
  | op=PREFIXOP
    { op }


(* A name or optionally _ *)
opt_name(X):
  | x=X
    { Some x }

  | UNDERSCORE
    { None }

(* A name or _, where _ is represented as an anonymous name *)
anon_name(X):
  | x=opt_name(X)
    { match x with Some x -> x | None -> Name.anonymous () }

recursive_clause:
  | f=ml_name y=ml_arg ys=ml_arg* u=let_annotation EQ c=term
    { (f, y, ys, u, c) }

let_clause:
  | x=ml_name ys=ml_arg* u=let_annotation EQ c=term
    { Sugared.Let_clause_ML (Some (x, ys), u, c) }

  | UNDERSCORE u=let_annotation EQ c=term
    { Sugared.Let_clause_ML (None, u, c) }

  | x=ml_name COLON t=app_term EQ c=term
    { Sugared.Let_clause_tt (Some x, t, c) }

  | UNDERSCORE COLON t=app_term EQ c=term
    { Sugared.Let_clause_tt (None, t, c) }

  | pt=let_pattern u=let_annotation EQ c=term
    { Sugared.Let_clause_patt (pt, u, c) }


(* A possibly annotated argument of an ML function *)
ml_arg:
  | x=ml_name
    { (x, Sugared.Arg_annot_none) }

  | LPAREN x=ml_name COLONGT t=mlty RPAREN
    { (x, Sugared.Arg_annot_ty t) }

let_annotation:
  |
    { Sugared.Let_annot_none }

  | COLONGT sch=ml_schema
    { Sugared.Let_annot_schema sch }

maybe_typed_binder:
  | LBRACE xs=anon_name(tt_name)+ RBRACE
    { List.map (fun x -> (x, None)) xs }

  | LBRACE xs=anon_name(tt_name)+ COLON t=ty_term RBRACE
    { List.map (fun x -> (x, Some t)) xs }

typed_binder:
  | LBRACE xs=anon_name(tt_name)+ COLON t=ty_term RBRACE
    { List.map (fun x -> (x, t)) xs }

abstraction:
  | lst=nonempty_list(maybe_typed_binder)
    { List.concat lst }

substitution:
  | LBRACE subst=separated_nonempty_list(COMMA, term) RBRACE
    { subst }

handler_cases:
  | BAR lst=separated_nonempty_list(BAR, handler_case)
    { lst }

  | lst=separated_list(BAR, handler_case)
    { lst }

handler_case:
  | VAL c=match_case
    { Sugared.CaseVal c }

  | RAISE c=match_case
    { Sugared.CaseExc c }

  | op=long(op_name) ps=prefix_pattern* pt=handler_checking ARROW t=term
    { Sugared.CaseOp (op, (ps, pt, t)) }

top_operation_case:
  | OPERATION op=long(op_name) ps=prefix_pattern* pt=handler_checking ARROW t=term
    { (op, (ps, pt, t)) }

top_operation_cases:
  | BAR lst=separated_nonempty_list(BAR, top_operation_case)
    { lst }

  | lst=separated_list(BAR, top_operation_case)
    { lst }

handler_checking:
  |
    { None }

  | COLON pt=pattern
    { Some pt }

match_cases:
  | BAR lst=separated_nonempty_list(BAR, match_case)
    { lst }

  | lst=separated_list(BAR, match_case)
    { lst }

match_case:
  | p=pattern g=when_guard ARROW c=term
    { (p, g, c) }


when_guard:
  |
    { None }

  | WHEN c=binop_term
    { Some c }

(** Pattern matching *)

pattern: mark_location(pattern_) { $1 }
pattern_:
  | p=binop_pattern_
    { p }

  | p1=binop_pattern AS p2=binop_pattern
    { Sugared.Patt_As (p1, p2) }

  | p=binop_pattern TYPE
    { Sugared.Patt_IsType p }

  | p1=binop_pattern COLON p2=binop_pattern
    { Sugared.Patt_IsTerm (p1, p2) }

  | p=binop_pattern COLONGT t=prod_mlty
    { Sugared.Patt_MLAscribe (p, t) }

  | p1=binop_pattern EQEQ  p2=binop_pattern
    { Sugared.Patt_EqType (p1, p2) }

  | p1=binop_pattern EQEQ  p2=binop_pattern COLON p3=pattern
    { Sugared.Patt_EqTerm (p1, p2, p3) }

  | QQMARK COLON p=binop_pattern
    { Sugared.Patt_BoundaryIsTerm p }

  | p1=binop_pattern EQEQ p2=binop_pattern BY QQMARK
    { Sugared.Patt_BoundaryEqType (p1, p2) }

  | p1=binop_pattern EQEQ p2=binop_pattern COLON p3=binop_pattern BY QQMARK
    { Sugared.Patt_BoundaryEqTerm (p1, p2, p3) }

  | abstr=tt_maybe_typed_binder p=pattern
    { Sugared.Patt_Abstraction (abstr, p) }

binop_pattern: mark_location(binop_pattern_) { $1 }
binop_pattern_:
  | e=app_pattern_
    { e }

  | e1=binop_pattern oploc=infix e2=binop_pattern
    { let (op, _) = oploc in
      Sugared.Patt_Constructor (Name.PName op, [e1; e2])
    }

(* app_pattern: mark_location(app_pattern_) { $1 } *)
app_pattern_:
  | e=prefix_pattern_
    { e }

  | t=long(any_name) ps=prefix_pattern+
    { Sugared.Patt_Constructor (t, ps) }

prefix_pattern: mark_location(prefix_pattern_) { $1 }
prefix_pattern_:
  | e=simple_pattern_
    { e }

  | UATOM p=prefix_pattern
    { Sugared.Patt_GenAtom p }

  | oploc=prefix e=prefix_pattern
    { let (op, _) = oploc in
      Sugared.Patt_Constructor (Name.PName op, [e])
    }

(* simple_pattern: mark_location(simple_pattern_) { $1 } *)
simple_pattern_:
  | UNDERSCORE
    { Sugared.Patt_Anonymous }

  | QQMARK TYPE
    { Sugared.Patt_BoundaryIsType }

  | x=long(ml_name)
    { Sugared.Patt_Path x }

  | LPAREN ps=separated_list(COMMA, pattern) RPAREN
    { match ps with
      | [{Location.it=p;_}] -> p
      | _ -> Sugared.Patt_Tuple ps
    }

  | LBRACK ps=separated_list(SEMI, pattern) RBRACK
    { Sugared.Patt_List ps }

  | s=QUOTED_STRING
    { Sugared.Patt_String s }

let_pattern: mark_location(let_pattern_) { $1 }
let_pattern_:
  | LPAREN ps=separated_list(COMMA, pattern) RPAREN
    { match ps with
      | [{Location.it=p;_}] -> p
      | _ -> Sugared.Patt_Tuple ps
    }

  | LBRACK ps=separated_list(SEMI, pattern) RBRACK
    { Sugared.Patt_List ps }

tt_maybe_typed_binder:
  | LBRACE xs=opt_name(tt_name)+ RBRACE
    { List.map (fun x -> (x, None)) xs }

  | LBRACE xs=opt_name(tt_name)+ COLON t=pattern RBRACE
    { List.map (fun x -> (x, Some t)) xs }

(* ML types *)

op_mlsig:
  | lst=separated_nonempty_list(ARROW, prod_mlty)
    { match List.rev lst with
      | t :: ts -> (List.rev ts, t)
      | [] -> assert false
     }

ml_schema: mark_location(ml_schema_)  { $1 }
ml_schema_:
  | MLFORALL params=opt_name(ml_name)+ COMMA t=mlty
    { Sugared.ML_Forall (params, t) }

  | t=mlty
    { Sugared.ML_Forall ([], t) }

mlty: mark_location(mlty_) { $1 }
mlty_:
  | prod_mlty_
    { $1 }

  | t1=prod_mlty ARROW t2=mlty
    { Sugared.ML_Arrow (t1, t2) }

  | t1=prod_mlty DARROW t2=mlty
    { Sugared.ML_Handler (t1, t2) }

prod_mlty: mark_location(prod_mlty_) { $1 }
prod_mlty_:
  | ts=separated_nonempty_list(STAR, app_mlty)
    { match ts with
      | [] -> assert false
      | [{Location.it=t;_}] -> t
      | _::_::_ -> Sugared.ML_Prod ts
    }

app_mlty: mark_location(app_mlty_) { $1 }
app_mlty_:
  | simple_mlty_
    { $1 }

  | REF t=simple_mlty
    { Sugared.ML_Ref t }

  | c=long(ml_name) args=nonempty_list(simple_mlty)
    { Sugared.ML_TyApply (c, args) }

simple_mlty: mark_location(simple_mlty_) { $1 }
simple_mlty_:
  | LPAREN t=mlty_ RPAREN
    { t }

  | c=long(ml_name)
    { Sugared.ML_TyApply (c, []) }

  | MLJUDGEMENT
    { Sugared.ML_Judgement }

  | MLBOUNDARY
    { Sugared.ML_Boundary }

  | MLDERIVATION
    { Sugared.ML_Derivation }

  | MLUNIT
    { Sugared.ML_Prod [] }

  | MLSTRING
    { Sugared.ML_String }

  | UNDERSCORE
    { Sugared.ML_Anonymous }

mlty_defs:
  | lst=separated_nonempty_list(AND, mlty_def)
    { lst }

mlty_def:
  | a=ml_name xs=list(opt_name(ml_name)) EQ body=mlty_def_body
    { (a, (xs, body)) }

mlty_def_body:
  | t=mlty
    { Sugared.ML_Alias t }

  | c=mlty_constructor BAR lst=separated_list(BAR, mlty_constructor)
    { Sugared.ML_Sum (c :: lst) }

  | BAR lst=separated_list(BAR, mlty_constructor)
    { Sugared.ML_Sum lst }

mlty_constructor:
  | c=constr_name OF t=mlty
    { (c, [t]) }

  | c=constr_name
    { (c, []) }

mark_location(X):
  x=X
  { Location.mark ~at:(Location.make $startpos $endpos) x }
%%
