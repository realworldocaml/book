(*****************************************************************************)
(*  Mezzo, a programming language based on permissions                       *)
(*  Copyright (C) 2011, 2012 Jonathan Protzenko and François Pottier         *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU General Public License as published by     *)
(*  the Free Software Foundation, either version 3 of the License, or        *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU General Public License for more details.                             *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License        *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

(** The grammar of Mezzo. *)

(* ---------------------------------------------------------------------------- *)

(* Syntactic categories of names. *)

(* Term variables, type variables, type constructors, and fields are not
   syntactically distinguished. Placing term variables, type variables, and
   type constructors within a single syntactic category is natural because
   they share certain mechanisms (e.g. types and terms can be abstracted over
   them). They will be distinguished using sorts. Placing term variables and
   fields within a single syntactic category is required because we wish to
   allow puns. *)

%token<string> LIDENT

(* As in ocaml, we set up a separate namespace for data constructors. This allows
   distinguishing variables and data constructors in a pattern. (Another solution
   would be to require data constructors to be explicitly followed with braces.) *)

%token<string> UIDENT

(* ---------------------------------------------------------------------------- *)

(* Other tokens. *)

%token          OPEN BUILTIN
%token          VALUE TYPE PERM
%token          UNKNOWN DYNAMIC EXCLUSIVE MUTABLE
%token          DATA ALIAS BAR UNDERSCORE
%token          LBRACKET RBRACKET LBRACE RBRACE LPAREN RPAREN
%token          COMMA COLON COLONCOLON SEMI AT AS
%token          ARROW LARROW DBLARROW TAGOF FUN
%token          EMPTY ASSERT EXPLAIN FAIL
%token          CONSUMES DUPLICABLE FACT ABSTRACT
%token          FLEX PACK WITNESS
%token          VAL LET REC AND IN DOT WITH BEGIN END MATCH FOR ABOVE BELOW DOWNTO
%token          IF THEN ELSE PRESERVING WHILE DO
%token          TAKE FROM GIVE TO ADOPTS TAKING
%token<int>     INT
%token<string>  OPPREFIX OPINFIX0a OPINFIX0b OPINFIX0c OPINFIX0d OPINFIX1 OPINFIX2 OPINFIX3 OPINFIX4
%token<string>  EQUAL STAR PLUS MINUS COLONEQUAL (* special cases of operators *)
%token          EOF

%nonassoc THEN
%nonassoc ELSE

%nonassoc ADOPTS
%nonassoc COLONEQUAL
%left     OPINFIX0a
%left     OPINFIX0b
%left     OPINFIX0c EQUAL (* EQUAL is also a OPINFIX0c *)
%left     OPINFIX0d
%right    OPINFIX1
%left     OPINFIX2 PLUS MINUS (* MINUS is also an OPINFIX2 *)
%left     OPINFIX3 STAR  (* STAR is also an OPINFIX3 *)
%right    OPINFIX4

(* ---------------------------------------------------------------------------- *)

(* Miscellaneous directives. *)

%start <SurfaceSyntax.implementation> implementation
%start <SurfaceSyntax.interface> interface
%start <(ClFlags.flag * (int * int)) list> warn_error_list

%{

open Kind
open SurfaceSyntax
open ParserUtils

%}

%%

(* ---------------------------------------------------------------------------- *)

(* Namespaces. *)

(* We work with several namespaces, each of which is obtained by applying
   the functor [Identifier.Make] and defines an abstract type [name]. This
   should help us avoid confusions between namespaces. *)

(* At the moment, there are three namespaces: variables, data constructors,
   and modules. *)

%inline infix_operator:
  | o = OPINFIX0a
  | o = OPINFIX0b
  | o = OPINFIX0c
  | o = OPINFIX0d
  | o = OPINFIX1
  | o = OPINFIX2
  | o = OPINFIX3
  | o = OPINFIX4
  | o = EQUAL
  | o = STAR
  | o = MINUS
  | o = PLUS
  | o = COLONEQUAL
      { o }

variable:
    (* A identifier that begins with a lowercase letter is a variable. *)
  | x = LIDENT
    (* As per the OCaml convention, a parenthesized operator is a variable. *)
    (* TEMPORARY maybe this could be recognized by the lexer, saving about 30 states in the LR automaton? *)
  | LPAREN x = OPPREFIX RPAREN
  | LPAREN x = infix_operator RPAREN
      { Variable.register x }

%inline datacon:
  datacon = UIDENT
    { Datacon.register datacon }

%inline module_name:
  (* A module name must begin with a lowercase letter. *)
  name = LIDENT
    { Module.register name }

(* ---------------------------------------------------------------------------- *)

(* A variable or data constructor can be qualified with a module name. *)

maybe_qualified(X):
  x = X
    { Unqualified x }
| m = module_name COLONCOLON x = X
    { Qualified (m, x) }

%inline maybe_qualified_type_variable:
  x = maybe_qualified(variable)
    { TyVar x }

%inline datacon_reference:
  d = maybe_qualified(datacon)
    { mk_datacon_reference d }

%inline maybe_qualified_variable:
  x = maybe_qualified(variable)
    { EVar x }

(* ---------------------------------------------------------------------------- *)

(* In a right-flexible list, the last delimiter is optional, i.e., [delim]
   can be viewed as a terminator or a separator, as desired. *)

(* There are several ways of expressing this. One could say it is either a
   separated list or a terminated list; this works if one uses right
   recursive lists. Or, one could say that it is separated list followed
   with an optional delimiter; this works if one uses a left-recursive
   list. The following formulation is direct and seems most natural. It
   should lead to the smallest possible automaton. *)

right_flexible_list(delim, X):
| (* nothing *)
    { [] }
| x = X
    { [x] }
| x = X delim xs = right_flexible_list(delim, X)
    { x :: xs }

(* In a left-flexible list, the first delimiter is optional, i.e., [delim]
   can be viewed as an opening or as a separator, as desired. *)

(* Again, there are several ways of expressing this, and again, I suppose
   the following formulation is simplest. It is the mirror image of the
   above definition, so it is naturally left-recursive, this time. *)

reverse_left_flexible_list(delim, X):
| (* nothing *)
    { [] }
| x = X
    { [x] }
| xs = reverse_left_flexible_list(delim, X) delim x = X
    { x :: xs }

%inline left_flexible_list(delim, X):
  xs = reverse_left_flexible_list(delim, X)
    { List.rev xs }

(* A separated list of at least two elements. *)

%inline separated_list_of_at_least_two(sep, X):
| x1 = X sep x2 = separated_nonempty_list(sep, X)
    { x1 :: x2 }

(* ---------------------------------------------------------------------------- *)

(* Syntax for type/type applications. *)

(* Applications of types to types are based on juxtaposition, just like
   applications of terms to terms. *)

(* In the abstract syntax, type/type applications are n-ary. In the
   grammar of types, though, they must be binary, in order to avoid
   ambiguity. *)

%inline type_type_application(X, Y):
  ty1 = X ty2 = Y (* juxtaposition *)
    { mktyapp ty1 ty2 }

%inline iterated_type_type_application(X, Y):
  x = X ys = Y* (* iterated juxtaposition *)
    { x, ys }

(* ---------------------------------------------------------------------------- *)

(* Syntax for type abstraction and universal quantification. *)

type_parameters:
| LBRACKET bs = right_flexible_list(COMMA, type_binding) RBRACKET
    { bs }

(* Syntax for existential quantification. *)

existential_quantifiers:
| LBRACE bs = right_flexible_list(COMMA, type_binding) RBRACE
    { bs }

(* ---------------------------------------------------------------------------- *)

(* Syntax for binding type variables. *)

(* Because the syntax of type/type applications is juxtaposition, the
   syntax of type variable bindings must be atomic (well-delimited). *)

atomic_type_binding:
| x = variable (* TYPE is the default kind *)
    { x, KType, ($startpos(x), $endpos) }
| LPAREN b = type_binding RPAREN
    { b }

variance:
| PLUS
    { Covariant }
| MINUS
    { Contravariant }
|
    { Invariant }

atomic_type_binding_with_variance:
| v = variance b = atomic_type_binding
    { v, b }

type_binding:
| b = atomic_type_binding
    { b }
| x = variable COLON kind = kind
    { x, kind, ($startpos(x), $endpos) }

(* ---------------------------------------------------------------------------- *)

(* Kinds. *)

atomic_kind:
| LPAREN kind = kind RPAREN
    { kind }
| VALUE
    { KValue }
| TYPE
    { KType }
| PERM
    { KPerm }

%inline kind:
| kind = atomic_kind
    { kind }

(* ---------------------------------------------------------------------------- *)

(* Types and permissions. *)

(* Because types and permissions are distinguished via the kind system, they
   are not (and must not be) distinguished in the syntax. Thus, the
   productions that concern permissions (the empty permission, anchored
   permissions, permission conjunction, etc.) appear as part of the syntax of
   types. *)

(* The syntax of types is stratified into the following levels:

     parenthetic_type
     atomic_type
     tight_type
     normal_type
     loose_type
     consumes_type
     very_loose_type
     fat_type

*)

%inline tlocated (X):
| x = X
    { TyLocated (x, ($startpos(x), $endpos)) }

(* A parenthetic type is a type that is delimited by parentheses. This
   syntactic category is used as the formal parameter in a function
   definition. *)

%inline parenthetic_type:
| ty = tlocated(raw_parenthetic_type)
    { ty }

raw_parenthetic_type:
(* The empty tuple type. *)
| LPAREN RPAREN
    { TyTuple [] }
(* Parentheses are used as a disambiguation device, as is standard. *)
| LPAREN ty = arbitrary_type RPAREN
    { ty }

(* An atomic type is one that is clearly delimited. This includes the
   parenthetic types, plus various kinds of atoms (type variables, etc.) *)

%inline atomic_type:
| ty = tlocated(raw_atomic_type)
    { ty }

raw_atomic_type:
| ty = raw_parenthetic_type
    { ty }
(* The top type. *)
| UNKNOWN
    { TyUnknown }
(* The type [dynamic] represents a permission to test membership in a dynamic region. *)
| DYNAMIC
    { TyDynamic }
(* The top permission. A neutral element for permission conjunction. *)
| EMPTY
    { TyEmpty }
(* The wildcard, which allows a limited form of type inference for functions. *)
| UNDERSCORE
    { TyWildcard }
(* Term variable, type variable, permission variable, abstract type, or concrete type. *)
| x = maybe_qualified_type_variable
    { x }
(* A structural type without an [adopts] clause (which is the usual case)
   is an atomic type. *)
| b = data_type_branch
    { mk_concrete ($startpos(b), $endpos) b None }

%inline tight_type:
| ty = tlocated(raw_tight_type)
    { ty }

raw_tight_type:
| ty = raw_atomic_type
    { ty }
(* A singleton type. *)
| EQUAL x = maybe_qualified_type_variable
    { TySingleton x }
(* A type application. *)
| ty = type_type_application(tight_type, atomic_type)
    { ty }

%inline normal_type:
| ty = tlocated(raw_normal_type)
    { ty }

%inline raw_normal_type_no_adopts_rec(X):
| ty = raw_tight_type
    { ty }
(* The syntax of function types is [t -> t], as usual. *)
| ty1 = tight_type ARROW ty2 = X
    { TyArrow (ty1, ty2) }
(* A polymorphic type. *)
| bs = type_parameters ty = X
    { List.fold_right (fun b ty -> TyForall (b, ty)) bs ty }
(* An existential type. *)
| bs = existential_quantifiers ty = X
    { List.fold_right (fun b ty -> TyExists (b, ty)) bs ty }
(* A type that carries a mode constraint (implication). *)
| c = mode_constraint DBLARROW ty = X
    { TyImply (c, ty) }

raw_normal_type_no_adopts:
| x = raw_normal_type_no_adopts_rec(raw_normal_type_no_adopts)
    { x }

raw_normal_type:
| t = raw_normal_type_no_adopts_rec(raw_normal_type)
    { t }
(* A structural type with an [adopts] clause is a considered a normal type.
   This allows the type in the [adopts] clause to be itself a normal type. *)
| b = data_type_branch ADOPTS t = raw_normal_type
    { mk_concrete ($startpos(b), $endpos) b (Some t) }

%inline loose_type:
| ty = tlocated(raw_loose_type)
    { ty }

raw_loose_type:
| ty = raw_normal_type
    { ty }
(* In an anchored permission [x@t], the name [x] is free. This
   represents an assertion that we have permission to use [x] at
   type [t]. *)
| x = maybe_qualified_type_variable AT ty = normal_type
    { TyAnchoredPermission (x, ty) }
(* [x = y] is also an anchored permission; it is sugar for [x@=y]. *)
| x = maybe_qualified_type_variable EQUAL y = maybe_qualified_type_variable
    { TyAnchoredPermission (x, TySingleton y) }
(* In a name introduction form [x:t], the name [x] is bound. The scope
   of [x] is defined by somewhat subtle rules that need not concern us
   here. These rules are spelled out later on when we desugar the surface-level
   types into a lower-level representation. *)
| x = variable COLON ty = normal_type
    { TyNameIntro (x, ty) }
(* We also allow a name introduction form where the name is [_]. *)
| UNDERSCORE COLON ty = normal_type
    { ty }

%inline consumes_type:
| ty = tlocated(raw_consumes_type)
    { ty }

raw_consumes_type:
| ty = raw_loose_type
    { ty }
(* A type can be annotated with the [CONSUMES] keyword. This really
   makes sense only in certain contexts, e.g. in the left-hand side of an
   arrow, and possibly further down under tuples, stars, etc. The grammar
   allows this everywhere. This notation is checked for consistency and
   desugared in a separate pass. *)
| CONSUMES ty = loose_type
    { TyConsumes ty }

%inline very_loose_type:
| ty = tlocated(raw_very_loose_type)
    { ty }

(* [COMMA] and [STAR] are at the same level, but cannot be mixed with
   each other. *)

raw_very_loose_type:
| ty = raw_consumes_type
    { ty }
(* Permission conjunction is a binary operator. *)
| ty1 = consumes_type STAR ty2 = very_loose_type
    { TyStar (ty1, ty2) }
(* A tuple type of length at least two is written [t1, ..., tn], without
   parentheses. A tuple type of length one cannot be written -- there is
   no syntax for it. *)
| tcs = separated_list_of_at_least_two(COMMA, consumes_type)
    { TyTuple tcs }

%inline fat_type:
| ty = tlocated(raw_fat_type)
  { ty }

raw_fat_type:
| ty = raw_very_loose_type
    { ty }
(* The conjunction of a type and a permission is written [t | p]. It is
   typically used as the domain or codomain of a function type. *)
| ty1 = fat_type BAR ty2 = very_loose_type
    { TyBar (ty1, ty2) }
| BAR ty2 = very_loose_type
    { TyBar (TyTuple [], ty2) }
(* A type that carries a mode constraint (conjunction). *)
| ty = fat_type BAR c = mode_constraint
    { TyAnd (c, ty) }

%inline arbitrary_type:
  ty = fat_type
    { ty }

(* ---------------------------------------------------------------------------- *)

(* Mode constraints are used as part of toplevel function definitions. *)

(* We allow just one constraint at a time. Multiple constraints, separated
   with commas, could perhaps be allowed, but I am afraid that this looks
   too much like a tuple. *)

(* There is no syntax for the bottom mode or the top mode. *)

mode:
| EXCLUSIVE
    { Mode.ModeExclusive }
| DUPLICABLE
    { Mode.ModeDuplicable }

%inline mode_constraint:
| m = mode t = atomic_type
    { m, t }

(* ---------------------------------------------------------------------------- *)

(* Some generic definitions concerning applications of data constructors. *)

(* A data constructor application takes the generic form [D { ... }]. As a
   special case, a pair of empty braces can be omitted. *)

%inline curly_application(X, Y):
| x = X
    { x, [] }
| x = X LBRACE y = Y RBRACE
    { x, y }

generic_datacon_application(Y):
| x = curly_application (datacon_reference, Y)
    { x }

generic_bare_datacon_application(Y):
| x = curly_application (datacon, Y)
    { x }

(* It is often the case that the contents of the curly braces is a semicolon-
   separated (or -terminated) list of things. *)

%inline datacon_application(Y):
| xys = generic_datacon_application(right_flexible_list(SEMI, Y))
    { xys }

(* ---------------------------------------------------------------------------- *)

(* Data type definitions. *)

(* Please note that the distinction between value fields and permission fields
 * only exists temporarily (hence, the anonymous variant type); these variants
 * disappear as soon as the user of [data_type_branch] calls
 * [ParserUtils.mk_concrete]. The reason why we group [`FieldValue]'s and
 * [`FieldPermission]'s in a common list is that [curly_brace] hardcodes the
 * empty list as a default value. *)

data_field_def:
(* A field definition normally mentions a field name and a field type. Multiple
   field names, separated with commas, can be specified: this means that they
   share a common type. *)
| fs = separated_nonempty_list(COMMA, variable) COLON ty = normal_type
    { List.map (fun f -> `FieldValue (f, ty)) fs }
(* The double-colon stands for a binding field name whose scope is the entire
 * concrete type. *)
| fs = separated_nonempty_list(COMMA, variable) COLONCOLON ty = normal_type
    { List.map (fun f -> `FieldBindingValue (f, ty)) fs }
(* We also allow a field definition to take the form of an equality between
   a field name [f] and a term variable [y]. This is understood as sugar for
   a definition of the field [f] at the singleton type [=y]. In this case,
   only one field name is allowed. This short-hand is useful in the syntax
   of structural permissions. *)
| f = variable EQUAL y = maybe_qualified_type_variable
    { [ `FieldValue (f, TySingleton y) ] }
(* Going one step further, we allow a field definition to consist of just
   a field name [f]. This is a pun: it means [f = f], or in other words,
   [f: =f]. *)
| f = variable
    { [ `FieldValue (f, TySingleton (TyVar (Unqualified f))) ] }

(* Field definitions are semicolon-separated or -terminated. *)

%inline data_fields_def:
  fss = right_flexible_list(SEMI, data_field_def)
    { List.flatten fss }

(* A list of field definitions is optionally followed with BAR and a
   permission. *)

data_type_def_branch_content:
  fs = data_fields_def
    { fs }
| fs = data_fields_def BAR perm = very_loose_type
    { fs @ [ `FieldPermission perm ] }

(* A branch in a data type definition is a constructor application,
   where, within the braces, we have the above content. This is also
   the syntax of structural permissions. *)

(* The [mutable] keyword may appear either in front of the algebraic
   data type definition, in which case it concerns all branches, or
   in front of a branch, in which case it concerns this branch only. *)

%inline data_type_flavor:
| (* nothing *)
    { DataTypeFlavor.Immutable }
| MUTABLE
    { DataTypeFlavor.Mutable }

%inline data_type_branch:
  dfs = generic_datacon_application(data_type_def_branch_content)
    { dfs }

%inline data_type_def_branch:
  flavor = data_type_flavor
  t = tlocated(raw_normal_type_no_adopts)
    {
      flavor, t
    }

%inline data_type_def_lhs:
  x = variable ys = atomic_type_binding_with_variance*
    { (* A little hack: we don't know the kind yet, so we abstract over it. *)
      fun kind ->
        (x, kind, ($startpos(x), $endpos(x))),
        ys
    }

%inline data_type_def_rhs:
  bs = left_flexible_list(BAR, data_type_def_branch)
    { bs }

%inline optional_kind_annotation:
| (* nothing *)
    { KType }
| COLON k = kind
    { k }

fact:
| FACT cs = separated_nonempty_list(DBLARROW, mode_constraint)
    { match List.rev cs with goal :: hypotheses -> Fact (List.rev hypotheses, goal) | [] -> assert false }

concrete_data_type_def:
| flavor = data_type_flavor
  lhs = data_type_def_lhs
  EQUAL
  rhs = data_type_def_rhs
  a = preceded(ADOPTS, arbitrary_type)?
    { { lhs = lhs KType; rhs = Concrete (flavor, rhs, a) } }

abstract_data_type_def:
| lhs = data_type_def_lhs
  k = optional_kind_annotation
  facts = fact*
    { { lhs = lhs k; rhs = Abstract facts } }

(* A concrete data type is necessarily of kind type. We do not allow defining
   concrete data types of kind perm. In principle, we could allow it. I think
   we can live without it (experience will tell). *)

(* Type abbreviations. *)

abbreviation_def:
  lhs = data_type_def_lhs
  k = optional_kind_annotation
  EQUAL t = arbitrary_type
    { { lhs = lhs k; rhs = Abbrev t } }

%inline data_type_group_body:
| DATA
  defs = separated_nonempty_list(AND, concrete_data_type_def)
    { ($startpos(defs), $endpos), Recursive, defs }
| ABSTRACT
  def = abstract_data_type_def
    { ($startpos(def), $endpos), Nonrecursive, [def] }
| ALIAS def = abbreviation_def
    { ($startpos(def), $endpos), Nonrecursive, [def] }

%inline data_type_group:
| g = data_type_group_body
    { DataTypeGroup g }


(* ---------------------------------------------------------------------------- *)

(* Patterns. *)

(* The syntax of patterns is stratified into the following levels:

     atomic_pattern
     normal_pattern
     loose_pattern

*)

%inline plocated (X):
| x = X
    { PLocated (x, ($startpos(x), $endpos)) }

%inline atomic_pattern:
| p = plocated(raw_atomic_pattern)
    { p }

raw_atomic_pattern:
(* The unit pattern. *)
| LPAREN RPAREN
    { PTuple [] }
| LPAREN p = pattern RPAREN
    { p }
| dc = datacon_application(data_field_pattern)
    { PConstruct dc }
| x = variable
    { PVar x }
| UNDERSCORE
    { PAny }

data_field_pattern:
| f = variable EQUAL p = pattern
    { f, p }
| f = variable
    (* Punning *)
    { f, PVar f }

%inline normal_pattern:
| p = plocated(raw_normal_pattern)
    { p }

(* Following OCaml, we interpret [x, y as z] as [(x, y) as z], and
   we interpret [w as x, y as z] as [((w as x), y) as z]. This is
   not great, but it seems wise to follow OCaml. A stricter option
   would be to reject these dubious examples by requiring an
   [atomic_pattern] before the keyword [AS]. *)

raw_normal_pattern:
| p = raw_atomic_pattern
    { p }
| ps = separated_list_of_at_least_two(COMMA, atomic_pattern)
    { PTuple ps }
| p = normal_pattern AS v = variable
    { PAs (p, v) }

%inline loose_pattern:
| p = plocated(raw_loose_pattern)
    { p }

raw_loose_pattern:
| p = raw_normal_pattern
    { p }
| p = loose_pattern COLON t = normal_type
    { PConstraint (p, t) }

%inline pattern:
| p = loose_pattern
    { p }

(* ---------------------------------------------------------------------------- *)

(* Sometimes a variable is viewed as a term binding. This is a degenerate
   case of a pattern. *)

%inline variable_as_term_binding:
  x = variable
    { x, KValue, ($startpos(x), $endpos) }

(* ---------------------------------------------------------------------------- *)

(* Expressions. *)

(* The syntax of expressions is stratified into the following levels:

     atomic_expression        e.g. x
     tight_expression         e.g. x.tail
     application_expression   e.g. length x.tail
     algebraic_expression     e.g. length x.tail + 1
     reasonable_expression    e.g. x.size <- length x.tail + 1
     fragile_expression       e.g. x.size <- length x.tail + 1; x.size

   Furthermore, tuples receive special treatment. Tuples are normally made
   of algebraic expressions and need not be explicitly parenthesized. Yet,
   in the special case where a tuple is parenthesized, we are able to be a
   bit more flexible and allow the *last* component of the tuple to be an
   arbitrary (fragile) expression. This may sound ridiculous, but it covers
   the important use case of encoding loops using higher-order functions. *)

%inline elocated (X):
| x = X
    { ELocated (x, ($startpos(x), $endpos(x))) }

%inline atomic_expression:
| e = elocated(raw_atomic_expression)
    { e }

raw_atomic_expression:
(* The regular prefix operators, represented by the token [OPPREFIX], bind
   very tightly. Here, we follow OCaml. For instance, [!x.foo] is interpreted
   as [(!x).foo]. Thus, the prefix operators bind more tightly than the dot. *)
| o = OPPREFIX e = atomic_expression
    { mkprefix o e }
| v = maybe_qualified_variable
    { v }
| i = INT
    { EInt i }
| FAIL
    { EFail }
| dc = datacon_application(data_field_expression)
    { EConstruct dc }
| MATCH
  b = explain
  e = expression
  WITH
  bs = left_flexible_list(BAR, match_branch)
  END
    { EMatch (b, e, bs) }
(* The unit value. *)
| LPAREN RPAREN
    { ETuple [] }
(* A parenthesized tuple. Thanks to the presence of the parentheses,
   we can be somewhat flexible and allow the *last* component to be
   a fragile expression. The other components cannot be fragile.
   If it turns out that there is only one component, then this is
   not a tuple, but a normal use of the parentheses. *)
| LPAREN es = parenthesized_tuple_components RPAREN
    { match es with [ e ] -> e | _ -> ETuple es }
(* Same as above. *)
| BEGIN es = parenthesized_tuple_components END
    { match es with [ e ] -> e | _ -> ETuple es }

(* A non-empty, comma-separated list of tuple components. The last
   component is a fragile expression, whereas the other components
   must be algebraic expressions. *)
parenthesized_tuple_components:
| e = fragile_expression
    { [ e ] }
| e1 = algebraic_expression COMMA e2 = parenthesized_tuple_components
    { e1 :: e2 }

data_field_expression:
(* In a record construction expression, field definitions are separated by
   semicolons. Thus, the expression in the right-hand side of a field
   definition must not contain a bare semi-colon, as this would lead to
   ambiguity. For this reason, we disallow [let] and sequence here. *)
(* We could allow a [tuple_or_reasonable_expression] here, but it seems
   better do disallow it; the presence of a comma suggests that the user
   has used a comma instead of a semicolon to separate fields. *)
| f = variable EQUAL e = reasonable_expression
    { f, e }
| f = variable
    (* Punning *)
    { f, EVar (Unqualified f) }

explain:
| (* nothing *)
    { false }
| EXPLAIN
    { true }

%inline match_branch:
| p = normal_pattern ARROW e = expression
    { p, e }

%inline tight_expression:
  e = elocated(raw_tight_expression)
    { e }

raw_tight_expression:
| e = tight_expression DOT f = variable
    { EAccess (e, mk_field f) }
| a = raw_atomic_expression
    { a }

%inline application_expression:
| e = elocated(raw_application_expression)
    { e }

raw_application_expression:
| e1 = application_expression e2 = tight_expression
    { EApply (e1, e2) }
| e1 = application_expression
  LBRACKET ts = separated_nonempty_list(COMMA, type_application_component) RBRACKET
    { ETApply (e1, ts) }
| e = raw_tight_expression
    { e }

type_application_component:
| t = normal_type
    { Ordered t }
| v = variable EQUAL t = normal_type
    { Named (v, t) }
(* TEMPORARY syntaxe pas géniale car
   "x [y = z]" et "x [(y = z)]"
   signifient alors deux choses différentes.
   En plus elle nous empêche d'autoriser un type plus haut que normal_type *)

%inline algebraic_expression:
| e = elocated(raw_algebraic_expression)
    { e }

raw_algebraic_expression:
| e1 = algebraic_expression o = infix_operator e2 = algebraic_expression
    { mkinfix e1 o e2 }
(* Whereas the regular prefix operators, represented by the token [OPPREFIX],
   bind very tightly, the unary [MINUS] operator binds more loosely. Here, we
   follow OCaml. The goal is to interpret [f !x] as [f (!x)] and [f -1] as
   [f - 1]. Like OCaml, we allow [-f x], which is interpreted as [-(f x)]. *)
| MINUS e = application_expression
    { mkinfix (EInt 0) "-" e }
    (* TEMPORARY here, unary minus is treated as a non-hygienic macro for
       binary minus; do we want this? does OCaml allow redefining unary
       minus? *)
| e1 = algebraic_expression ADOPTS e2 = algebraic_expression
    { EOwns (e1, e2) }
| e = raw_application_expression
    { e }

(* Unparenthesized tuples are made of algebraic expressions. They are not
   part of a fixed priority level, but can be explicitly allowed to appear
   in certain places where a reasonable expression or a fragile expression
   is expected. *)

raw_tuple_or(E):
| es = separated_list_of_at_least_two(COMMA, algebraic_expression)
    { ETuple es }
| e = E
    { e }

%inline tuple_or_reasonable_expression:
| e = elocated(raw_tuple_or(raw_reasonable_expression))
    { e }

reasonable_expression:
| e = elocated(raw_reasonable_expression)
    { e }

(* A reasonable expression is one that can be followed with a semicolon
   and will not swallow it. *)
raw_reasonable_expression:
  (* We disallow "let" inside of "then" or "else", because this is too fragile.
     It is a common source of errors in OCaml. We do allow unparenthesized
     tuples in the branches of an "if" construct. *)
| IF b = explain e1 = expression THEN e2 = tuple_or_reasonable_expression
    { EIfThenElse (b, e1, e2, ETuple []) }
| IF b = explain e1 = expression THEN e2 = tuple_or_reasonable_expression
                                 ELSE e3 = tuple_or_reasonable_expression
    { EIfThenElse (b, e1, e2, e3) }
| p = optional_preserving
  WHILE e1 = expression
  DO e2 = reasonable_expression
    { EWhile (p, e1, e2) }
| p = optional_preserving
  FOR x = variable_as_term_binding EQUAL e1 = expression f = direction e2 = expression
  DO e = reasonable_expression
    { EFor (p, x, e1, f, e2, e) }
  (* We cannot allow "let" on the right-hand side of an assignment, because
     the right-hand side of "let" can contain a semi-colon. We disallow
     unparenthesized tuples, even though we could allow them, for two
     reasons: 1- the presence of a comma might suggest a confusion between
     comma and semicolon; and 2- we might reserve the syntax x.f, y.f <- e1, e2
     for multiple assignments. *)
| e1 = tight_expression DOT f = variable LARROW e2 = reasonable_expression
    { EAssign (e1, mk_field f, e2) }
| TAGOF e1 = tight_expression LARROW d = datacon_reference
    { EAssignTag (e1, d, mk_tag_update_info ()) }
| TAKE e1 = expression FROM e2 = reasonable_expression
    { ETake (e1, e2) }
| GIVE e1 = expression TO e2 = reasonable_expression
    { EGive (e1, e2) }
| taking = TAKING e1 = expression FROM e2 = tight_expression BEGIN e = expression fin = END
    {
      taking; fin; (* avoid ocaml warnings about unused variables *)
      let eval_e1, v1 = name "adoptee" e1
      and eval_e2, v2 = name "adopter" e2
      and eval_e , v  = name "result"  e in
      eval_e1 (
      eval_e2 (
      ESequence (
      ELocated (ETake (v1, v2), ($startpos(taking), $endpos(e2))),
      eval_e (
      ESequence (
      ELocated (EGive (v1, v2), ($startpos(fin), $endpos(fin))),
      v
      )))))
    }
| ASSERT t = very_loose_type
    { EAssert t }
| PACK t1 = very_loose_type WITNESS t2 = very_loose_type
    { EPack (t1, t2) }
(* An expression that carries a type constraint. We cannot allow a fat type
   here because they have BARs in them and that would create an ambiguity
   when used inside a match construct! *)
| e = algebraic_expression COLON t = very_loose_type
    { EConstraint (e, t) }
| e = raw_algebraic_expression
    { e }

optional_preserving:
| PRESERVING p = arbitrary_type
    { p }
| (* nothing *)
    { TyEmpty }

%inline tuple_or_fragile_expression:
| e = elocated(raw_tuple_or(raw_fragile_expression))
    { e }

%inline fragile_expression:
| e = elocated(raw_fragile_expression)
    { e }

raw_fragile_expression:
(* The semi-colon can be used as a separator. *)
| e1 = reasonable_expression SEMI e2 = tuple_or_fragile_expression
    { ESequence (e1, e2) }
(* The semi-colon can also be used as a terminator. This should facilitate
   swapping instructions in sequences. This flexibility is possible thanks
   to the fact that local and toplevel definitions are distinguished by a
   different leading keyword. *)
 | e1 = reasonable_expression SEMI
    { e1 }
(* Local value definitions are introduced by [let], whereas toplevel definitions
   are introduced by [val]. Their syntax is otherwise identical. *)
| LET flag_defs = definitions IN e = tuple_or_fragile_expression
    { let flag, defs = flag_defs in ELet (flag, defs, e) }
| LET FLEX v = type_binding IN e = tuple_or_fragile_expression
    { ELetFlex (v, e) }
| LET d = data_type_group_body IN e = tuple_or_fragile_expression
    { ELocalType (d, e) }
| FUN e = anonymous_function
    { e }
| e = raw_reasonable_expression
    { e }

rec_flag:
| REC
    { Recursive }
|
    { Nonrecursive }

direction:
| TO
    { To }
| DOWNTO
    { Downto }
| BELOW
    { Below }
| ABOVE
    { Above }

%inline expression:
| e = tuple_or_fragile_expression
    { e }

(* ---------------------------------------------------------------------------- *)

(* Value and function definitions. *)

(* Top-level definitions and local definitions have the same syntax, except
   for the leading keyword, which is [let] or [val]. We factor out this
   keyword and define the common part here. *)

(* Following the leading [let] or [val] keyword, we have an optional [rec]
   keyword, followed with a list of definitions, separated by [and]. *)

%inline definitions:
  flag = rec_flag
  defs = separated_list(AND, definition)
    { flag, defs }

(* A definition is of one the following forms. We define several alternative
   forms, but we could also define a single form with optional components. *)

definition:
| p = normal_pattern EQUAL e = expression
    { p, e }
| p = normal_pattern COLON t = normal_type EQUAL e = expression
    { PConstraint (p, t), e }
| p = normal_pattern e = anonymous_function
    { p, e }
| p = normal_pattern COLON t = normal_type EQUAL bb = BUILTIN b = LIDENT
    { PConstraint (p, t), ELocated (EBuiltin b, ($startpos(bb), $endpos(bb))) }

(* The syntax of anonymous function appears as part of definitions (above)
   and also, when preceded with the keyword [fun], as a stand-alone
   expression. *)

(* We could allow the formal arguments to be an [atomic_type], but I prefer
   to require the formal arguments to be surrounded with parentheses; this
   is why I define and use the category [parenthetic_type]. *)

anonymous_function:
  (* Optional type parameters: [a] *)
  type_parameters = loption(type_parameters)
  (* Optional constraint(s): duplicable a => *)
  cs = terminated(mode_constraint, DBLARROW)*
  (* Formal arguments: (x: a) *)
  formal = parenthetic_type
  (* Result type: : (a, a) *)
  COLON result = normal_type
  (* Function body: = (x, x) *)
  EQUAL body = expression
    { let formal = List.fold_right (fun c ty -> TyAnd (c, ty)) cs formal in
      let formal = TyLocated (formal, ($startpos(formal), $endpos(formal))) in
      EFun (type_parameters, formal, result, body) }

(* ---------------------------------------------------------------------------- *)

(* Top-level value definitions. *)

(* This is a toplevel item; it appears in implementations only. *)

definition_group:
| v = VAL flag_defs = definitions
    { let flag, defs = flag_defs in
      let loc = ($startpos(v), $endpos) in
      ValueDefinitions (loc, flag, defs) }
(* TEMPORARY why do we have a single location for an entire group
   of definitions? *)

(* ---------------------------------------------------------------------------- *)

(* Top-level value declarations. *)

(* This is a toplevel item; it appears in interfaces only. *)

(* We currently do not allow mutually recursive value declarations, e.g.
   two variables [x] and [y], where the type of [x] mentions [y], and
   vice-versa. *)

value_declaration:
| VAL x = variable_as_term_binding COLON ty = arbitrary_type
    { ValueDeclaration (x, ty) }

(* ---------------------------------------------------------------------------- *)

(* Open directives. *)

(* This is a toplevel item; it appears in interfaces and implementations. *)

%inline open_directive:
| OPEN m = module_name
    { OpenDirective m }

(* ---------------------------------------------------------------------------- *)

(* Program units, i.e. module implementations *)

(* Because we don't want to clobber the parser with complicated logic, we
   first parse each value or type definition as being in its own group, and
   later call [ParserUtils.group] so as to group adjacent definitions
   together. This is done in implementation and interface files. *)

implementation_item:
| item = data_type_group
| item = definition_group
| item = open_directive
    { item }

implementation:
| items = implementation_item* EOF
    { items }

(* ---------------------------------------------------------------------------- *)

(* Module signatures, i.e. interfaces. *)

interface_item:
| item = data_type_group
| item = value_declaration
| item = open_directive
    { item }

interface:
| items = interface_item* EOF
  { items }

(* ---------------------------------------------------------------------------- *)

(* Parsing of command-line error/warning/silent flags. *)

warn_error_list:
| ws = warn_error+ EOF
  { ws }

warn_error:
| f = flag r = range
  { f, r }

flag:
| AT
  { ClFlags.CError }
| MINUS
  { ClFlags.CSilent }
| PLUS
  { ClFlags.CWarning }

range:
| i = INT
  { i, i }
| i = INT DOT DOT j = INT
  { i, j }

(* ---------------------------------------------------------------------------- *)

(* Below this line: ideas for the record or for future consideration. *)

(* We have removed arrow kinds because they complicate the inference and
   expression of ``mode facts''. That is, in the presence of type variables of
   higher kinds, it becomes difficult to specify exactly under what conditions
   a type is duplicable, exclusive, or affine. Perhaps the loss of arrow kinds
   will be compensated by introducing a module system (functors, etc.) where
   facts can appear as module components. *)

(* If the [if] construct was closed with [endif], then one could again
   authorize a [let] construct to appear naked within a [then] or [else]
   branch. *)

(* I considered using COMMA for separating conjunction. However, COMMA is
   already used to separate multiple arguments in a type application, so this
   means that parentheses will sometimes be necessary (e.g. when a conjunction
   of permissions is used as an argument in a type application). Even worse,
   COMMA is used in the syntax of tuples, and this leads to conflicts with
   tuple types and multi-argument function types. So, I give up and use a
   dedicated symbol, STAR, for conjunction. Somewhat analogously, yet another
   symbol, BAR, is now used for the conjunction of a type and a permission. *)
