(* TEMPORARY syntaxe pas homogène: parfois le tuple vide est \epsilon, parfois c'est NOTHING *)
(* TEMPORARY on autorise f(g(x)) si f attend deux arguments, mais pas A(g(x)) si A attend deux arguments *)

%{

open Source.Raw

let boolpat b =
  Annotation.make Location.dummy (PBool b)

%}

%token <Identifier.t> ID TAG
%token LPAR COMMA RPAR LANGLE RANGLE FRESH IN IF THEN ELSE CASE OF END LET FUN BAR DEFEQ CMPEQ ARROW EOF WILD
%token EMPTYSET SETUNION SETMINUS WHERE DISJOINT ACCEPTS PRODUCES NOTHING TYPE INNER OUTER
%token STAR BINDS ATOM ATOMSET COLON SUPPORT BOUND ABSURD BOOL TRUE FALSE BNOT BAND BOR MEMBER SUBSET FAIL
%token NEXT
%start <Source.Raw.program> program
%start <Source.Raw.specification> specification_alone

%left BOR
%left BAND
%nonassoc BNOT
%nonassoc CMPEQ
%left SETUNION
%nonassoc SETMINUS

%%

(* ------------------------------------------------------------------------- *)

(* Attaching locations to semantic values. *)

located(X):
| x = X
    { Annotation.make ($startpos, $endpos) x }

(* ------------------------------------------------------------------------- *)

(* Parenthesized tuples, with or without special cases. *)

parenthesized_tuple(X):
| LPAR xs = separated_list(COMMA, X) RPAR
    { xs }

lenient_parenthesized_tuple(X):
|
    (* Epsilon represents the empty tuple. *)
    { [] }
| LPAR xs = separated_nonempty_list(COMMA, X) RPAR
    (* If there are one or more elements, they must be parenthesized. *)
    { xs }

(* ------------------------------------------------------------------------- *)

(* Set expressions. *)

set_function:
| SUPPORT
    { SFSupport }
| OUTER
    { SFOuter }
| INNER
    { SFInner}
| BOUND
    { SFBound }

set_expression:
| EMPTYSET
    { SEEmpty }
| e1 = set_expression SETUNION e2 = set_expression
    { SEUnion (e1, e2) }
| e1 = set_expression SETMINUS e2 = set_expression
    { SEDifference (e1, e2) }
| f = set_function LPAR x = ID RPAR
    { SEApp (f, x) }
| LPAR e = set_expression RPAR
    { e }

(* ------------------------------------------------------------------------- *)

(* Set constraints. *)

set_operator:
| SUBSET
    { OpSubset }
| CMPEQ
    { OpEqual }
| DISJOINT
    { OpDisjoint }

set_constraint:
| e1 = set_expression op = set_operator e2 = set_expression
    { (e1, op, e2) }

(* ------------------------------------------------------------------------- *)

(* Boolean constraints. *)

boolean_expression:
| FALSE
    { FFalse }
| TRUE
    { FTrue }
| b1 = boolean_expression BAND b2 = boolean_expression
    { FAnd (b1, b2) }
| b1 = boolean_expression CMPEQ b2 = boolean_expression
    { FIff (b1, b2) }
| b1 = boolean_expression BOR b2 = boolean_expression
    { FChoice (b1, b2) }
| BNOT b = boolean_expression
    { FNot b }
| x = ID
    { FVar x }
| LPAR b = boolean_expression RPAR
    { b }

(* ------------------------------------------------------------------------- *)

(* General constraints. *)

contrainte:
| b = boolean_expression
    { CBoolean b }
| c = set_constraint
    { CSet (FTrue, c) }
| b = boolean_expression ARROW c = set_constraint
    { CSet (b, c) }

contraintes:
| cs = preceded(WHERE, contrainte)*
    { cs }

(* ------------------------------------------------------------------------- *)

(* Patterns. *)

raw_pattern:
| WILD
    { PWildcard }
| x = ID
    { PVar x }
| tag = TAG ps = lenient_parenthesized_tuple(pattern)
    { PTagTuple (tag, ps) }
| TRUE
    { PBool true }
| FALSE
    { PBool false }

pattern:
| p = located(raw_pattern)
    { p }

(* ------------------------------------------------------------------------- *)

(* Expressions. *)

prefix_callee:
| f = ID
    { CUser f }
| SUPPORT
    { CPrim PrimGenericSupport }
| OUTER
    { CPrim PrimGenericOuter }
| INNER
    { CPrim PrimGenericInner }
| BOUND
    { CPrim PrimGenericBound }
| MEMBER
    { CPrim PrimMember }

%inline infix_primitive:
| CMPEQ
    { PrimAtomEquality }
| BAND
    { PrimBoolAnd }
| BOR
    { PrimBoolOr }

raw_atomic_expression:
| x = ID
    { EVar x }
| TRUE
    { EBool true }
| FALSE
    { EBool false }
| tag = TAG es = lenient_parenthesized_tuple(atomic_expression)
    { ETagTuple (tag, es) }
| CASE e = expression OF branches = branch+ END
    { ECase (e, branches) }
| callee = prefix_callee LPAR e = expression RPAR
    { ECall (callee, e) }
| LPAR e = raw_expression RPAR
    { e }
| BNOT e = atomic_expression
    { ECall (CPrim PrimBoolNot, e) }
| e1 = atomic_expression p = infix_primitive e2 = atomic_expression
    { ECall (CPrim p, Annotation.make ($startpos, $endpos) (EMulti [ e1; e2 ])) }
| NEXT CASE
    { ENextCase }

atomic_expression:
| e = located(raw_atomic_expression)
    { e }

variables: (* TEMPORARY *)
| NOTHING
    { [] }
| xs = separated_nonempty_list(COMMA, ID)
    { xs }

raw_expression:
| ABSURD
    { EAbsurd }
| FAIL
    { EFail }
| FRESH xs = variables IN e = expression
    { EFresh (xs, e) }
| IF e = expression THEN e1 = expression ELSE e2 = expression
    (* Syntactic sugar: [if] instead of [case]. *)
    { ECase (e, [ [ boolpat true ], e1 ; [ boolpat false ], e2 ]) }
| LET xs = variables claim = contraintes DEFEQ e1 = expression IN e2 = expression
    { ELetMulti (e1, (xs, claim, e2)) }
| LET tag = TAG ps = lenient_parenthesized_tuple(pattern) DEFEQ e1 = expression IN e2 = expression
    (* Syntactic sugar for single-branch case constructs. *)
    { let p = Annotation.make ($startpos(tag), $endpos(ps)) (PTagTuple (tag, ps)) in
      ECase (e1, [ [ p ], e2 ]) }
| NOTHING
    { EMulti [] }
| e = atomic_expression COMMA es = separated_nonempty_list(COMMA, atomic_expression)
    { EMulti (e :: es) }
| e = raw_atomic_expression
    { e }

expression:
| e = located(raw_expression)
    { e }

branch:
| BAR ps = separated_nonempty_list(COMMA, pattern) ARROW e = expression
    { ps, e }

(* ------------------------------------------------------------------------- *)

(* Types. *)

typ:
| t = ID
    { TData t }
| ATOM
    { TAtom }
| ATOMSET
    { TAtomSet }
| BOOL
    { TBool }

(* ------------------------------------------------------------------------- *)

(* Layouts are tuple patterns with binding structure. *)

atomic_layout:
| x = ioption(terminated(ID, COLON)) t = typ
    { LComponent (x, t) }
| INNER layout = located(atomic_layout)
    { LInner layout }
| OUTER layout = located(atomic_layout)
    { LOuter layout }
| LANGLE layout = located(layout) RANGLE
    { LAbstraction layout }

layout:
| layout = atomic_layout
    { layout }
| head = located(atomic_layout) STAR tail = separated_nonempty_list(STAR, located(atomic_layout))
    { LTuple (head :: tail) }

optional_layout:
| (* epsilon *)
    { LTuple [] }
| OF layout = layout
    { layout }

(* ------------------------------------------------------------------------- *)

(* Declarations and definitions. *)

data_constructor_declaration:
| BAR tag = TAG layout = located(optional_layout) guard = contraintes
    { tag, (layout, guard) }

specification:
| ACCEPTS  xs = variables pre  = contraintes
  PRODUCES ys = variables post = contraintes
    { xs, pre, (ys, post) }

definition:
| FUN f = ID spec = specification DEFEQ e = expression
    { let xs, pre, opc = spec in
      DefValFun (f, (xs, pre, opc, e)) }
| TYPE t = ID kind = kind DEFEQ ds = data_constructor_declaration*
    { DefDataType (t, (kind, ds)) }

kind:
| (* epsilon *)
    { KExpression }
| BINDS
    { KPattern }

program:
| defs = definition* EOF
    { defs }

specification_alone:
| spec = specification EOF
    { spec }

