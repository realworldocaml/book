%token<string> IDENT
%token<int> INTLITERAL
%token FUN IN LET PRINT REC
%token ARROW EQ LPAREN RPAREN
%token<RawLambda.binop> MULOP ADDOP
%token EOF

%start<RawLambda.term> entry

%{

open RawLambda

%}

%%

(* -------------------------------------------------------------------------- *)

(* A toplevel phrase is just a term. *)

entry:
  t = any_term EOF
    { t }

(* -------------------------------------------------------------------------- *)

(* The syntax of terms is stratified as follows:

   atomic_term             -- unambiguously delimited terms
   application_term        -- n-ary applications of atomic terms
   multiplicative_term     -- built using multiplication & division
   additive_term           -- built using addition & subtraction
   any_term                -- everything

   A [match/with/end] construct is terminated with an [end] keyword, as in Coq,
   so it is an atomic term. *)

atomic_term_:
| LPAREN t = any_term RPAREN
    { t.value }
| x = IDENT
    { Var x }
| i = INTLITERAL
    { Lit i }

application_term_:
| t = atomic_term_
    { t }
| t1 = placed(application_term_) t2 = placed(atomic_term_)
    { App (t1, t2) }
| PRINT t2 = placed(atomic_term_)
    { Print t2 }

%inline multiplicative_term_:
  t = left_associative_level(application_term_, MULOP, mkbinop)
    { t }

%inline additive_term_:
  t = left_associative_level(multiplicative_term_, ADDOP, mkbinop)
    { t }

any_term_:
| t = additive_term_
    { t }
| FUN x = IDENT ARROW t = any_term
    { Lam (x, t) }
| LET mode = recursive x = IDENT EQ t1 = any_term IN t2 = any_term
    { Let (mode, x, t1, t2) }

%inline any_term:
  t = placed(any_term_)
    { t }

(* -------------------------------------------------------------------------- *)

(* An infix-left-associative-operator level in a hierarchy of arithmetic
   expressions. *)

(* [basis] is the next lower level in the hierarchy.
   [op] is the category of binary operators.
   [action] is a ternary sequencing construct. *)

left_associative_level(basis, op, action):
| t = basis
| t = action(
        left_associative_level(basis, op, action),
        op,
        basis
      )
    { t }

(* -------------------------------------------------------------------------- *)

(* A ternary sequence whose semantic action builds a [BinOp] node. *)

%inline mkbinop(term1, op, term2):
  t1 = placed(term1) op = op t2 = placed(term2)
    { BinOp (t1, op, t2) }

(* -------------------------------------------------------------------------- *)

(* A [let] construct carries an optional [rec] annotation. *)

recursive:
| REC { Recursive }
|     { NonRecursive }

(* -------------------------------------------------------------------------- *)

(* A term is annotated with its start and end positions, for use in error
   messages. *)

%inline placed(X):
  x = X
    { { place = ($startpos, $endpos); value = x } }

(* -------------------------------------------------------------------------- *)

(* In a right-flexible list, the last delimiter is optional, i.e., [delim] can
   be viewed as a terminator or a separator, as desired. *)

(* There are several ways of expressing this. One could say it is either a
   separated list or a terminated list; this works if one uses right recursive
   lists. Or, one could say that it is a separated list followed with an
   optional delimiter; this works if one uses a left-recursive list. The
   following formulation is direct and seems most natural. It should lead to
   the smallest possible automaton. *)

right_flexible_list(delim, X):
| (* nothing *)
    { [] }
| x = X
    { [x] }
| x = X delim xs = right_flexible_list(delim, X)
    { x :: xs }

(* In a left-flexible list, the first delimiter is optional, i.e., [delim] can
   be viewed as an opening or as a separator, as desired. *)

(* Again, there are several ways of expressing this, and again, I suppose the
   following formulation is simplest. It is the mirror image of the above
   definition, so it is naturally left-recursive, this time. *)

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
