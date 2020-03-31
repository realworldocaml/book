
%{

open Hol_type
open Term
open Signature
open Lexing

let print_error startpos endpos msg =
  if startpos.pos_lnum = endpos.pos_lnum then
    Printf.printf "line %d, characters %d-%d: %s\n"
      startpos.pos_lnum (startpos.pos_cnum - startpos.pos_bol)
      (endpos.pos_cnum - endpos.pos_bol)
      msg
  else
    Printf.printf "line %d, character %d - line %d, character %d: %s\n"
      startpos.pos_lnum (startpos.pos_cnum - startpos.pos_bol)
      endpos.pos_lnum (endpos.pos_cnum - endpos.pos_bol)
      msg

let terms = ref []
let sigma = ref (new_signature ())
let bound_typevars = ref (Hashtbl.create 17)
let typevar_counter = ref 0
let termroles = ref (Hashtbl.create 3)
let add_fun = ref (fun _ _ -> ())

let reset () = let t= !terms in
               let s= !sigma in
               let r= !termroles in
               terms := [];
               sigma := new_signature ();
               termroles := (Hashtbl.create 3);
               (t,s,r)

let reset_bound_typevars () =
  bound_typevars := Hashtbl.create 17;
  typevar_counter := 0

let add_typevar s =
  Hashtbl.add !bound_typevars s (!typevar_counter);
  typevar_counter := !typevar_counter + 1

let add_term t =
  terms := t :: !terms;
  add_fun := fun n r -> Hashtbl.add !termroles r (n,t)

let add_def (s,t) =
  try (
    let ty = Term.type_of (type_of_symbol !sigma) t in
    add_defined_symbol ~ty:(Some ty) !sigma s t
  ) with Failure msg ->
    begin
      print_string ("\nin definition "^s^":\n"^msg^"\n\n");
      add_defined_symbol !sigma s t
    end;
  add_fun := fun n r -> Hashtbl.add !termroles r (s,t)

let add_const_strict (c,ty) =
  add_uninterpreted_symbol !sigma c ty;
  add_fun := fun n r -> Hashtbl.add !termroles r (c, Symbol c)

let add_const (c,ty) =
  if ty = Signature.bt_type then
    Signature.add_basetype !sigma c
  else
  try (
    let ty2 = type_of_symbol !sigma c in
    (* print_string ("Symbol "^c^": ."^(Hol_type.to_string ty2)^"., ."^(Hol_type.to_string ty)^".\n"); *)
    if not (ty2 = ty)
    then failwith ("add_const: type mismatch for symbol "^c^" and types "^
                   (Hol_type.to_string ty2)^" and "^(Hol_type.to_string ty2))
  ) with Failure _ -> add_const_strict (c,ty)

%}

%token AMPERSAND
%token AT_SIGN
%token CARET
%token CNF
%token COLON
%token COMMA
%token EQUALS
%token EXCLAMATION
%token DOUBLEEXCLAMATION
%token FOF
%token QMF
%token GETS
%token GREATER
%token HOF
%token IF
%token IFF
%token IMPLIES
%token INCLUDE_TOK
%token INPUT_CLAUSE
%token INPUT_FORMULA
%token LAMBDA
%token LBRKT
%token LPAREN
%token MAP_TO
%token MMINUS
%token NAMPERSAND
%token NEQUALS
%token NIFF
%token NVLINE
%token PERIOD
%token PPLUS
%token QUESTION
%token DOUBLEQUESTION
%token RBRKT
%token RPAREN
%token TILDE
%token BOX
%token DIAMOND
%token TOK_FALSE
%token TOK_I
%token TOK_O
%token TOK_INT
%token TOK_REAL
%token TOK_TRUE
%token TOK_TYPE
%token VLINE
%token EOF

%token DTHF
%token DFOF
%token DCNF
%token DFOT

%token <string> Unsigned_integer
%token <string> Signed_integer
%token <string> Decimal_part
%token <string> Real
%token <string> Atomic_system_word
%token <string> Distinct_object
%token <string> Single_quoted
%token <string> Upper_word
/* %token <string> Sq_upper_word */
%token <string> Lower_word
%token Unrecognized

/* associativity and precedences (from lowest to highest) */
/* FIXME: operator precedence doesn't work: "(~$false & $true)" is still translated to "neg ((and false) true)" instead of "(and (neg false)) true" */
%nonassoc IFF NIFF
%right IMPLIES IF
%nonassoc EQUALS NEQUALS
%right VLINE NVLINE
%left AMPERSAND NAMPERSAND
%left AT_SIGN
%nonassoc TILDE
%right DOUBLEEXCLAMATION DOUBLEQUESTION
%nonassoc EXCLAMATION QUESTION LAMBDA CARET

/* entry point */
%start input
%type <Term.term list * Signature.signature * (string , string * Term.term) Hashtbl.t> input

%%

/*
*/

input : /* (Term.term list * Signature.signature * (string , string * Term.term) Hashtbl.t) */
    null { reset () }
  | input hof_annotated { (!terms,!sigma,!termroles) }
  | input fof_annotated { (!terms,!sigma,!termroles) }
  | input cnf_annotated { (!terms,!sigma,!termroles) }
  | input qmf_annotated { (!terms,!sigma,!termroles) }
  | error { let startpos = Parsing.rhs_start_pos 1 in
            let endpos = Parsing.rhs_end_pos 1
            in
              print_error startpos endpos "syntax error";
              raise PARSER }
;


fof_annotated : /* unit */
    FOF LPAREN name COMMA formula_role COMMA fof_top annotations RPAREN PERIOD {State.set_input_logic "FOF"; !add_fun $3 $5}
;

qmf_annotated : /* unit */
    QMF LPAREN name COMMA formula_role COMMA qmf_top annotations RPAREN PERIOD {State.set_input_logic "QMF"; !add_fun $3 $5}
;

cnf_annotated : /* unit */
    CNF LPAREN name COMMA formula_role COMMA cnf_top annotations RPAREN PERIOD {State.set_input_logic "CNF"; !add_fun $3 $5}
;

formula_role : /* string */
  atomic_word { $1 }
;

annotations: /* null */
    COMMA source optional_info {}
  | null {}
;

source: /* null */
    general_term {}
;

general_term: /* null */
    general_data {}
  | general_data COLON general_term {}
  | general_list {}
;

general_data: /* null */
    atomic_word {}
  | atomic_word LPAREN general_terms RPAREN {}
  | variable {}
  | number {}
  | Distinct_object {}
  | formula_data {}
;

formula_data: /* null */
    DTHF LPAREN thf_logic_formula RPAREN {}
  | DFOF LPAREN fof_formula RPAREN {}
  | DCNF LPAREN cnf_formula RPAREN {}
  | DFOT LPAREN fof_term RPAREN {}
;

general_list: /* null */
    LBRKT RBRKT {}
  | LBRKT general_terms RBRKT {}
;

general_terms: /* null */
    general_term {}
  | general_term COMMA general_terms {}
;

optional_info: /* null */
    COMMA useful_info {}
  | null {}
;

useful_info: /* null */
    general_list {}
;




hof_annotated : /* unit */
    HOF LPAREN name COMMA formula_role COMMA typed_constant annotations RPAREN PERIOD {State.set_input_logic "THF"; add_const $7; !add_fun $3 $5}
  | HOF LPAREN name COMMA formula_role COMMA definition annotations RPAREN PERIOD {State.set_input_logic "THF"; add_def $7; reset_bound_typevars(); !add_fun $3 $5}
  | HOF LPAREN name COMMA formula_role COMMA thf_logic_formula annotations RPAREN PERIOD {State.set_input_logic "THF";
            match ($5,$7) with
              ("definition",(Appl (Appl (Symbol "=", Symbol definiendum), definiens))) -> (add_def (definiendum,definiens);
                                                                                           reset_bound_typevars();
                                                                                           !add_fun $3 $5)
            | _ -> (add_term $7; reset_bound_typevars();!add_fun $3 $5)}




hof_formula : /* unit */
    term { add_term $1; reset_bound_typevars() } /* axioms, theorems etc. */
  | definition { add_def $1; reset_bound_typevars() } /* definitions */
  | typed_constant { add_const_strict $1 } /* constants */
;


definition : /* (string * Term.term) */
/*    defined_element GETS term { ($1,$3) } */
/*  | LPAREN defined_element GETS term RPAREN{ ($2,$4) } */
    defined_element GETS thf_logic_formula { ($1,$3) }
  | LPAREN defined_element GETS thf_logic_formula RPAREN{ ($2,$4) }
;


defined_element : /* string */
    constant { $1 }
  | typed_constant { fst $1 } /* TODO: use type information */
;

typed_constant : /* (string * Holtype.holtype) */
    LPAREN constant COLON type_expr RPAREN { ($2,$4) }
  | constant COLON type_expr { ($1,$3) }
;

constant : /* string */
    TOK_TRUE { Signature.ctrue }
  | TOK_FALSE { Signature.cfalse }
  | atomic_word { $1 }
;


fof_top:
    fof_formula {(* print_string ("Read: "^(Term.to_hotptp $1)^"\n\n"); *)
      add_term (multi_quantified (Symbol Signature.forall) (List.map (fun x -> (x,bt_i)) (free_vars $1)) $1);
      reset_bound_typevars() }
;


qmf_top:
    qmf_formula {(* print_string ("Read: "^(Term.to_hotptp $1)^"\n\n"); *)
      add_term (multi_quantified (Symbol Signature.forall) (List.map (fun x -> (x,bt_i)) (free_vars $1)) $1);
      reset_bound_typevars() }
;


cnf_top:
    cnf_formula {
      add_term (multi_quantified (Symbol Signature.forall) (List.map (fun x -> (x,bt_i)) (free_vars $1)) $1);
      reset_bound_typevars() }
;

cnf_formula:
    cnf_literal { $1 }
  | cnf_literal VLINE cnf_formula { Appl(Appl(Symbol(Signature.disjunction),$1),$3) }
  | LPAREN cnf_formula RPAREN { $2 }
;

cnf_literal:
    fof_atomic_formula { $1 }
  | TILDE fof_atomic_formula { Appl(Symbol(Signature.neg),$2) }
;

fof_formula: /* Term.term */
    fof_unitary_formula { $1 }
  | fof_binary_formula { $1 }
;

fof_binary_formula: /* Term.term */
    fof_nonassoc_binary { $1 }
  | fof_assoc_binary { $1 }
;

fof_nonassoc_binary: /* Term.term */
    fof_unitary_formula fof_binary_connective fof_unitary_formula { Appl(Appl($2,$1),$3) }
;

fof_assoc_binary: /* Term.term */
    fof_and_formula { $1 }
  | fof_or_formula { $1 }
;

fof_and_formula: /* Term.term */
    fof_unitary_formula AMPERSAND fof_unitary_formula{ Appl(Appl(Symbol(Signature.conjunction),$1),$3) }
  | fof_and_formula AMPERSAND fof_unitary_formula { Appl(Appl(Symbol(Signature.conjunction),$1),$3) }
;

fof_or_formula: /* Term.term */
    fof_or_formula VLINE fof_unitary_formula { Appl(Appl(Symbol(Signature.disjunction),$1),$3) }
  | fof_unitary_formula { $1 }
;

fof_unitary_formula: /* Term.term */
    fof_quantified_formula { $1 }
  | fof_unary_formula { $1 }
  | fof_atomic_formula { $1 }
  | LPAREN fof_formula RPAREN { $2 }
;

fof_quantified_formula: /* Term.term */
    quantifier LBRKT fof_variable_decls RBRKT COLON fof_unitary_formula { multi_quantified $1 $3 $6 }
;

fof_variable_decls: /* (string * Hol_type.hol_type) list */
    fof_variable_decl { [$1] }
  | fof_variable_decl COMMA fof_variable_decls { $1::$3 }
  ;

fof_variable_decl: /* (string * Hol_type.hol_type) */
    fof_variable { ($1,bt_i) }
;

fof_unary_formula: /* Term.term */
    fof_unary_connective fof_unitary_formula { Appl($1,$2) }
;

fof_unary_connective: /* Term.term */
    TILDE { Symbol(Signature.neg) }
;

fof_atomic_formula: /* Term.term */
    fof_prop_const { Symbol $1 }
  | fof_term fof_infix_pred fof_term { Appl(Appl($2,$1),$3) }
/*  | fof_functor LPAREN fof_args RPAREN fof_infix_pred fof_term {
      add_const (get_symbol $1, mk_funtype (List.map (fun _ -> bt_i) $3) bt_i);
      Appl(Appl($5,List.fold_left (fun acc arg -> Appl(acc,arg)) $1 $3),$6) } */
  | fof_pred LPAREN fof_args RPAREN { (* print_string ("fof_pred "^(Term.to_hotptp $1)^" with args ");
                                      List.map (fun a -> print_string ((Term.to_hotptp a)^", ")) $3;
                                      print_string "\n"; *)
      add_const (get_symbol $1, mk_funtype (List.map (fun _ -> bt_i) $3) bt_o);
      List.fold_left (fun acc arg -> Appl(acc,arg)) $1 $3 }
;

fof_prop_const: /* String */
    TOK_TRUE { Signature.ctrue }
  | TOK_FALSE { Signature.cfalse }
  | atomic_word { add_const ($1,bt_o); $1 }
;

fof_infix_pred: /* Term.term */
  | EQUALS { Symbol(Signature.equality) }
  | NEQUALS { Symbol(Signature.nequals) }
;

fof_args: /* Term.term list */
    fof_term { [$1] }
  | fof_term COMMA fof_args { $1::$3 }
;

fof_term: /* Term.term */
    fof_constant { $1 }
  | fof_variable { Symbol $1 }
/*  | fof_functor LPAREN fof_args RPAREN { */
  | fof_pred LPAREN fof_args RPAREN {
      add_const (get_symbol $1, mk_funtype (List.map (fun _ -> bt_i) $3) bt_i);
      List.fold_left (fun acc arg -> Appl(acc,arg)) $1 $3 }
;

fof_constant:
    atomic_word { (* print_string ("There's a constant of type $i: "^$1^"\n"); *) add_const ($1, bt_i);
      Symbol $1 }
  | number { Symbol $1 }
;

fof_variable:
    variable { $1 }
;

/*
fof_functor:*/ /* Term.term */ /*
    atomic_word { Symbol $1 }
;
*/

fof_pred: /* Term.term */
    atomic_word { Symbol $1 }
;


qmf_formula: /* Term.term */
    qmf_unitary_formula { $1 }
  | qmf_binary_formula { $1 }
;

qmf_binary_formula: /* Term.term */
    qmf_nonassoc_binary { $1 }
  | qmf_assoc_binary { $1 }
;

qmf_nonassoc_binary: /* Term.term */
    qmf_unitary_formula fof_binary_connective qmf_unitary_formula { Appl(Appl($2,$1),$3) }
;

qmf_assoc_binary: /* Term.term */
    qmf_and_formula { $1 }
  | qmf_or_formula { $1 }
;

qmf_and_formula: /* Term.term */
    qmf_unitary_formula AMPERSAND qmf_unitary_formula{ Appl(Appl(Symbol(Signature.conjunction),$1),$3) }
  | qmf_and_formula AMPERSAND qmf_unitary_formula { Appl(Appl(Symbol(Signature.conjunction),$1),$3) }
;

qmf_or_formula: /* Term.term */
    qmf_or_formula VLINE qmf_unitary_formula { Appl(Appl(Symbol(Signature.disjunction),$1),$3) }
  | qmf_unitary_formula { $1 }
;

qmf_unitary_formula: /* Term.term */
    qmf_quantified_formula { $1 }
  | qmf_unary_formula { $1 }
  | qmf_atomic_formula { $1 }
  | qmf_boxed_formula { $1 }
  | LPAREN qmf_formula RPAREN { $2 }
;

qmf_boxed_formula: /* Term.term */
    boxoperator COLON qmf_unitary_formula { Appl($1,$3) }
;

qmf_quantified_formula: /* Term.term */
    quantifier LBRKT qmf_variable_decls RBRKT COLON qmf_unitary_formula { multi_quantified $1 $3 $6 }
;

qmf_variable_decls: /* (string * Hol_type.hol_type) list */
    qmf_variable_decl { [$1] }
  | qmf_variable_decl COMMA qmf_variable_decls { $1::$3 }
  ;

qmf_variable_decl: /* (string * Hol_type.hol_type) */
    qmf_variable { ($1,bt_i) }
;

qmf_unary_formula: /* Term.term */
    qmf_unary_connective qmf_unitary_formula { Appl($1,$2) }
;

qmf_unary_connective: /* Term.term */
    TILDE { Symbol(Signature.neg) }
;

qmf_atomic_formula: /* Term.term */
    qmf_prop_const { Symbol $1 }
  | qmf_term qmf_infix_pred qmf_term { Appl(Appl($2,$1),$3) }
/*  | qmf_functor LPAREN qmf_args RPAREN qmf_infix_pred qmf_term {
      add_const (get_symbol $1, mk_funtype (List.map (fun _ -> bt_i) $3) bt_i);
      Appl(Appl($5,List.fold_left (fun acc arg -> Appl(acc,arg)) $1 $3),$6) } */
  | qmf_pred LPAREN qmf_args RPAREN { (* print_string ("qmf_pred "^(Term.to_hotptp $1)^" with args ");
                                      List.map (fun a -> print_string ((Term.to_hotptp a)^", ")) $3;
                                      print_string "\n"; *)
      add_const (get_symbol $1, mk_funtype (List.map (fun _ -> bt_i) $3) bt_o);
      List.fold_left (fun acc arg -> Appl(acc,arg)) $1 $3 }
;

qmf_prop_const: /* String */
    TOK_TRUE { Signature.ctrue }
  | TOK_FALSE { Signature.cfalse }
  | atomic_word { add_const ($1,bt_o); $1 }
;

qmf_infix_pred: /* Term.term */
  | EQUALS { Symbol(Signature.equality) }
  | NEQUALS { Symbol(Signature.nequals) }
;

qmf_args: /* Term.term list */
    qmf_term { [$1] }
  | qmf_term COMMA qmf_args { $1::$3 }
;

qmf_term: /* Term.term */
    qmf_constant { $1 }
  | qmf_variable { Symbol $1 }
/*  | qmf_functor LPAREN qmf_args RPAREN { */
  | qmf_pred LPAREN qmf_args RPAREN {
      add_const (get_symbol $1, mk_funtype (List.map (fun _ -> bt_i) $3) bt_i);
      List.fold_left (fun acc arg -> Appl(acc,arg)) $1 $3 }
;

qmf_constant:
    atomic_word { (* print_string ("There's a constant of type $i: "^$1^"\n"); *) add_const ($1, bt_i);
      Symbol $1 }
  | number { Symbol $1 }
;

qmf_variable:
    variable { $1 }
;

/*
qmf_functor:*/ /* Term.term */ /*
    atomic_word { Symbol $1 }
;
*/

qmf_pred: /* Term.term */
    atomic_word { Symbol $1 }
;

thf_logic_formula : /* Term.term */
    thf_binary_formula { $1 }
  | thf_unitary_formula { $1 }
;

thf_binary_formula : /* Term.term */

    thf_binary_pair { $1 }
  | thf_binary_tuple { $1 }
;

thf_binary_pair : /* Term.term */
    thf_unitary_formula thf_pair_connective thf_unitary_formula { Appl(Appl($2,$1),$3) }
;

thf_pair_connective : /* Term.term */
    IFF { Symbol(Signature.iff) }
  | NIFF { Symbol(Signature.negated_iff) }
  | IMPLIES { Symbol(Signature.implies) }
  | IF { Symbol(Signature.i_f) }
  | NVLINE { Symbol(Signature.negated_disjunction) }
  | NAMPERSAND { Symbol(Signature.negated_conjunction) }
  | EQUALS { Symbol(Signature.equality) }
  | NEQUALS { Symbol(Signature.nequals) }
;


thf_binary_tuple : /* Term.term */
    thf_or_formula { $1 }
  | thf_and_formula { $1 }
  | thf_apply_formula { $1 }
;

thf_or_formula : /* Term.term */
    thf_unitary_formula VLINE thf_unitary_formula { Appl(Appl(Symbol(Signature.disjunction),$1),$3) }
  | thf_or_formula VLINE thf_unitary_formula { Appl(Appl(Symbol(Signature.disjunction),$1),$3) }
;

thf_and_formula : /* Term.term */
    thf_unitary_formula AMPERSAND thf_unitary_formula { Appl(Appl(Symbol(Signature.conjunction),$1),$3) }
  | thf_and_formula AMPERSAND thf_unitary_formula { Appl(Appl(Symbol(Signature.conjunction),$1),$3) }
;

thf_apply_formula : /* Term.term */
    thf_unitary_formula AT_SIGN thf_unitary_formula { Appl($1,$3) }
  | thf_apply_formula AT_SIGN thf_unitary_formula { Appl($1,$3) }
;

thf_unitary_formula : /* Term.term */
    thf_quantified_formula { $1 }
  | thf_abstraction { $1 }
  | thf_unary_formula { $1 }
  | thf_atom { $1 }
/*  | thf_tuple */
/*  | thf_letrec */
  | LPAREN thf_logic_formula RPAREN { $2 }
;

thf_quantified_formula : /* Term.term */
    quantifier LBRKT variable_decls RBRKT COLON thf_unitary_formula { multi_quantified $1 $3 $6 }
;

thf_abstraction : /* Term.term */
    lambda_symbol LBRKT variable_decls RBRKT COLON thf_unitary_formula { multi_abstr $3 $6 }
  | lambda_symbol LBRKT type_variable_decls RBRKT COLON thf_unitary_formula { $6 }
;

thf_unary_formula : /* Term.term */
    TILDE LPAREN thf_logic_formula RPAREN { Appl(Symbol(Signature.neg),$3) }
  | quantifier_comb thf_logic_formula { Appl($1, $2) }
  | quantifier_comb AT_SIGN thf_logic_formula { Appl($1, $3) }
;

quantifier_comb : /* Term.term */
    DOUBLEEXCLAMATION { Symbol(Signature.forall_comb) }
  | DOUBLEQUESTION { Symbol(Signature.exists_comb) }
;

thf_atom : /* Term.term */
  | variable { Symbol $1 }
  | constant { Symbol $1 }
  | number { Symbol $1 }
  | binary_connective { $1 }
  | unary_connective { $1 }
;






term : /* Term.term */
    lambda_term { $1 }
  | apply_term { $1 }
  | LPAREN term RPAREN { $2 }
  | variable { Symbol $1 }
  | constant { Symbol $1 }
  | quantified_term { $1 }
  | unary_connective term { Appl($1,$2) }
  | term binary_connective term { Appl(Appl($2,$1),$3) }
  | number { Symbol $1 }
  | binary_connective { $1 }
  | unary_connective { $1 }
;

unary_connective : /* Term.term */
    TILDE { Symbol(Signature.neg) }
  | quantifier_comb thf_logic_formula { Appl($1, $2) }
;

binary_connective : /* Term.term */
    IFF { Symbol(Signature.iff) }
  | NIFF { Symbol(Signature.negated_iff) }
  | IMPLIES { Symbol(Signature.implies) }
  | IF { Symbol(Signature.i_f) }
  | VLINE { Symbol(Signature.disjunction) }
  | NVLINE { Symbol(Signature.negated_disjunction) }
  | AMPERSAND { Symbol(Signature.conjunction) }
  | NAMPERSAND { Symbol(Signature.negated_conjunction) }
  | EQUALS { Symbol(Signature.equality) }
  | NEQUALS { Symbol(Signature.nequals) }
;

fof_binary_connective : /* Term.term */
    IFF { Symbol(Signature.iff) }
  | NIFF { Symbol(Signature.negated_iff) }
  | IMPLIES { Symbol(Signature.implies) }
  | IF { Symbol(Signature.i_f) }
  | NVLINE { Symbol(Signature.negated_disjunction) }
  | NAMPERSAND { Symbol(Signature.negated_conjunction) }
;

quantified_term : /* Term.term */
    quantifier LBRKT variable_decls RBRKT COLON term { multi_quantified $1 $3 $6 }
;

quantifier : /* Term.term */
    EXCLAMATION { Symbol(Signature.forall) }
  | QUESTION { Symbol(Signature.exists) }
;

boxoperator : /* Term.term */
    BOX { Symbol(Signature.box) }
  | DIAMOND { Symbol(Signature.diamond) }
;

apply_term : /* Term.term */
    term AT_SIGN term { Appl($1,$3) }
;

lambda_term : /* Term.term */
    lambda_symbol LBRKT variable_decls RBRKT COLON term { multi_abstr $3 $6 }
  | lambda_symbol LBRKT type_variable_decls RBRKT COLON term { $6 }
;

lambda_symbol :
    LAMBDA { }
  | CARET { }
;


variable_decls : /* (string * Hol_type.hol_type) list */
    variable_decl { [$1] }
  | variable_decl COMMA variable_decls { $1::$3 }
;

variable_decl : /* (string * Hol_type.hol_type) */
    variable COLON type_expr { ($1,$3) }
;

type_variable_decls : /* string list */
    type_variable_decl { }
  | type_variable_decl COMMA type_variable_decls { }
;

type_variable_decl : /* string */
    variable COLON TOK_TYPE { add_typevar $1 }
;

type_expr : /* Hol_type.hol_type */
    atomic_type { $1 }
  | type_variable { if Hashtbl.mem !bound_typevars $1 then mk_polyvar (Hashtbl.find !bound_typevars $1) else basetype $1  }
  | poly_type_variable { basetype $1 }
  | type_expr map_arrow type_expr { abstr_type $1 $3 }
  | LPAREN type_expr RPAREN { $2 }
;

atomic_type : /* Hol_type.hol_type */
    TOK_O { bt_o }
  | TOK_I { bt_i }
  | TOK_INT { basetype "$int" }
  | TOK_REAL { basetype "$real" }
  | TOK_TYPE { bt_type }
  | Lower_word { basetype $1 }
;

type_variable : /* string */
	  Upper_word { $1 }
;

poly_type_variable: /* string */
    Upper_word { $1 }
/*    Sq_upper_word { $1 } */
;

map_arrow :
    MAP_TO { }
  | GREATER  { }
;

name : /* string */
    atomic_word { $1 }
  | Unsigned_integer { $1 }
;

atomic_word : /* string */
    Lower_word { $1 }
  | Single_quoted { $1 }
;

variable : /* string */
    Upper_word { $1 }
;

number : /* string */
    Unsigned_integer { add_const ($1, basetype "$int");  $1 }
  | Signed_integer { add_const ($1, basetype "$int"); $1 }
  | Real { add_const ($1, basetype "$real"); $1 }

null : {};

