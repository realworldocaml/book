%{

open Syntax

%}

%token <Lexing.position * Lexing.position * string> LID UID
%token <string> OCAML
%token OF TYPE BINDS INNER OUTER NEUTRAL SORT ATOM BAR EQUAL STAR COMMA LANGLE RANGLE EOF
%token CONTAINER WITH AND DOT LBRACE RBRACE COLON SEMICOLON QUOTE LPAREN RPAREN IDENTIFIER MODULE

%start phrase
%type <string * Syntax.declaration list> phrase

%{

  let error i msg =
    Error.error2 (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos i) msg

  (* Used to collect inline pattern type definitions without too much
     fuss. [inline_defs] gathers the inline definitions found so far.
     [current_params] contains the type parameters currently in scope. *)

  let inline_defs =
    ref []

  let current_params =
    ref []

%}

%%

typevar:
| QUOTE LID
    { $2 }

typevars:
| typevar
    { [ $1 ] }
| typevars COMMA typevar
    { $3 :: $1 }

params:
| /* epsilon */
    { [] }
| typevar
    { [ $1 ] }
| LPAREN typevars RPAREN
    { List.rev $2 }

/* parses the same strings as [params], but sets [current_params]
   on the fly */
params_and_set_current:
| params
    { let params = $1 in
      current_params := params;
      params }

identifier:
| LID
    { $1 }
| UID DOT identifier
    { let (pos1, _, id1) = $1
      and (_, pos2, id2) = $3 in
      (pos1, pos2, id1 ^ "." ^ id2) }

container:
| /* epsilon */
    { None }
| identifier
    { Some $1 }

expfactor:
| ATOM LID
    { EAtom $2 }
| OCAML
    { EEscape $1 }
| params LID container
    { ETypRef ($3, $1, $2) }
| LANGLE params LID RANGLE
    { EAbstraction ($2, $3) }
| LANGLE LPAREN LID BINDS sorts RPAREN patrhs RANGLE
    { inline_defs := DeclPatType (!current_params, $3, $5, $7) :: !inline_defs;
      EAbstraction (!current_params, $3) }
| ATOM error /* parser01.mla */
    { error 2 "\"atom\" should be followed by a sort (a lowercase identifier)" }
| LANGLE error /* parser03.mla */
    { error 2 "The contents of an abstraction should be either\n\
               a (possibly parameterized) pattern type identifier or\n\
               an inline pattern type definition." }
| INNER /* parser04.mla */
    { error 1 "\"inner\" does not make sense in an expression type." }
| OUTER /* parser05.mla */
    { error 1 "\"outer\" does not make sense in an expression type." }
| NEUTRAL /* parser06.mla */
    { error 1 "\"neutral\" does not make sense in an expression type." }
| error /* parser10.mla parser19.mla */
    { error 1 "Invalid expression factor." }

patfactor:
| ATOM LID
    { PAtom $2 }
| OCAML
    { PEscape $1 }
| params LID container
    { PTypRef (MRef, $3, $1, $2) }
| patmodifier params LID container
    { PTypRef ($1, $4, $2, $3) }
| ATOM error
    { error 2 "\"atom\" should be followed by a sort (a lowercase identifier)" }
| patmodifier error /* parser07.mla */
    { error 2 "\"inner\", \"outer\", and \"neutral\" should be followed by a\n(possibly parameterized) type identifier." }
| LANGLE /* parser08.mla */
    { error 1 "An abstraction does not make sense in a pattern type." }
| error /* parser20.mla */
    { error 1 "Invalid pattern factor." }

patmodifier:
| INNER
    { MInner }
| OUTER
    { MOuter }
| NEUTRAL
    { MNeutral }

expfactors:
| expfactor
    { [ (None, $1) ] }
| expfactors STAR expfactor
    { (None, $3) :: $1 }
| expfactors error /* parser27.mla */
    /* Difficult to say something meaningful here. Either this tuple
       is invalid, or the next declaration doesn't make sense. */
    { error 2 "Undetermined syntax error." }

patfactors:
| patfactor
    { [ (None, $1) ] }
| patfactors STAR patfactor
    { (None, $3) :: $1 }
| patfactors error /* parser24.mla parser25.mla */
    /* Difficult to say something meaningful here. Either this tuple
       is invalid, or the next declaration doesn't make sense. */
    { error 2 "Undetermined syntax error." }

explfactor:
| LID COLON expfactor
    { (Some $1, $3) }
| error /* parser09.mla */
    { error 1 "\"<label> : <expression factor>\" expected." }

explfactors:
| explfactor
    { [ $1 ] }
| explfactors SEMICOLON explfactor
    { $3 :: $1 }
| explfactors error /* parser26.mla */
    { error 2 "\";\" or \"}\" expected." }

patlfactor:
| LID COLON patfactor
    { (Some $1, $3) }
| error /* parser22.mla */
    { error 1 "\"<label> : <pattern factor>\" expected." }

patlfactors:
| patlfactor
    { [ $1 ] }
| patlfactors SEMICOLON patlfactor
    { $3 :: $1 }
| patlfactors error /* parser28.mla */
    { error 2 "\";\" or \"}\" expected." }

expsummand:
| BAR UID
    { Summand (Some $2, []) }
| BAR UID OF expfactors
    { Summand (Some $2, List.rev $4) }
| BAR error /* parser29.mla */
    { error 2 "\"| <uppercase identifier> [ of <expression factors> ]\" expected." }

patsummand:
| BAR UID
    { Summand (Some $2, []) }
| BAR UID OF patfactors
    { Summand (Some $2, List.rev $4) }
| BAR error /* parser30.mla */
    { error 2 "\"| <uppercase identifier> [ of <pattern factors> ]\" expected." }

expsummands:
| expsummand
    { [ $1 ] }
| expsummands expsummand
    { $2 :: $1 }

patsummands:
| patsummand
    { [ $1 ] }
| patsummands patsummand
    { $2 :: $1 }

sorts:
| LID
    { [ $1 ] }
| sorts COMMA LID
    { $3 :: $1 }

optional_semicolon:
| /* epsilon */
    { () }
| SEMICOLON
    { () }

exprhs:
| expsummands
    { List.rev $1 }
| expfactors
    { [ Summand (None, List.rev $1) ] }
| LBRACE explfactors optional_semicolon RBRACE
    { [ Summand (None, List.rev $2) ] }

patrhs:
| patsummands
    { List.rev $1 }
| patfactors
    { [ Summand (None, List.rev $1) ] }
| LBRACE patlfactors RBRACE
    { [ Summand (None, List.rev $2) ] }

declaration:
| TYPE params_and_set_current LID EQUAL exprhs
    { DeclExpType ($2, $3, $5) }
| TYPE params_and_set_current LID BINDS sorts EQUAL patrhs
    { DeclPatType ($2, $3, $5, $7) }
| TYPE error /* parser16.mla parser17.mla */
    { error 2 "\"type [ <parameters> ] <identifier> [ binds <sorts> ] = <type definition>\"\nexpected." }
| SORT LID
    { DeclSort $2 }
| SORT error /* parser18.mla */
    { error 2 "\"sort <identifier>\" expected." }
| CONTAINER identifier WITH identifier AND identifier
    { DeclContainer ($2, $4, $6) }
| CONTAINER error /* parser12.mla parser13.mla parser15.mla */
    { error 2 "\"container <identifier> with <identifier> and <identifier>\" expected.\n\
               The three identifiers are the container type and its map and fold functions." }
| IDENTIFIER MODULE UID
    { DeclIdentifier $3 }
| IDENTIFIER error /* parser31.mla */
    { error 2 "\"identifier module <uppercase identifier>\" expected." }

declarations:
| /* epsilon */
    { [] }
| declarations declaration
    { $2 :: $1 }

prologue:
| /* epsilon */
    { "\n\n" }
| OCAML
    { "\n\n(* Prologue. *)" ^ $1 }

phrase:
| prologue declarations EOF
    { $1, List.rev (!inline_defs @ $2) }
| error /* parser11.mla parser23.mla parser14.mla */
    { error 1 "\"type\", \"sort\", \"container\",\nor \"identifier module\" declaration expected." }
