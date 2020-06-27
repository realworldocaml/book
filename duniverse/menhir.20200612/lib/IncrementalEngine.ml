(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

type position = Lexing.position

open General

(* This signature describes the incremental LR engine. *)

(* In this mode, the user controls the lexer, and the parser suspends
   itself when it needs to read a new token. *)

module type INCREMENTAL_ENGINE = sig

  type token

  (* A value of type [production] is (an index for) a production. The start
     productions (which do not exist in an \mly file, but are constructed by
     Menhir internally) are not part of this type. *)

  type production

  (* The type ['a checkpoint] represents an intermediate or final state of the
     parser. An intermediate checkpoint is a suspension: it records the parser's
     current state, and allows parsing to be resumed. The parameter ['a] is
     the type of the semantic value that will eventually be produced if the
     parser succeeds. *)

  (* [Accepted] and [Rejected] are final checkpoints. [Accepted] carries a
     semantic value. *)

  (* [InputNeeded] is an intermediate checkpoint. It means that the parser wishes
     to read one token before continuing. *)

  (* [Shifting] is an intermediate checkpoint. It means that the parser is taking
     a shift transition. It exposes the state of the parser before and after
     the transition. The Boolean parameter tells whether the parser intends to
     request a new token after this transition. (It always does, except when
     it is about to accept.) *)

  (* [AboutToReduce] is an intermediate checkpoint. It means that the parser is
     about to perform a reduction step. It exposes the parser's current
     state as well as the production that is about to be reduced. *)

  (* [HandlingError] is an intermediate checkpoint. It means that the parser has
     detected an error and is currently handling it, in several steps. *)

  (* A value of type ['a env] represents a configuration of the automaton:
     current state, stack, lookahead token, etc. The parameter ['a] is the
     type of the semantic value that will eventually be produced if the parser
     succeeds. *)

  (* In normal operation, the parser works with checkpoints: see the functions
     [offer] and [resume]. However, it is also possible to work directly with
     environments (see the functions [pop], [force_reduction], and [feed]) and
     to reconstruct a checkpoint out of an environment (see [input_needed]).
     This is considered advanced functionality; its purpose is to allow error
     recovery strategies to be programmed by the user. *)

  type 'a env

  type 'a checkpoint = private
    | InputNeeded of 'a env
    | Shifting of 'a env * 'a env * bool
    | AboutToReduce of 'a env * production
    | HandlingError of 'a env
    | Accepted of 'a
    | Rejected

  (* [offer] allows the user to resume the parser after it has suspended
     itself with a checkpoint of the form [InputNeeded env]. [offer] expects the
     old checkpoint as well as a new token and produces a new checkpoint. It does not
     raise any exception. *)

  val offer:
    'a checkpoint ->
    token * position * position ->
    'a checkpoint

  (* [resume] allows the user to resume the parser after it has suspended
     itself with a checkpoint of the form [AboutToReduce (env, prod)] or
     [HandlingError env]. [resume] expects the old checkpoint and produces a new
     checkpoint. It does not raise any exception. *)

  val resume:
    'a checkpoint ->
    'a checkpoint

  (* A token supplier is a function of no arguments which delivers a new token
     (together with its start and end positions) every time it is called. *)

  type supplier =
    unit -> token * position * position

  (* A pair of a lexer and a lexing buffer can be easily turned into a supplier. *)

  val lexer_lexbuf_to_supplier:
    (Lexing.lexbuf -> token) ->
    Lexing.lexbuf ->
    supplier

  (* The functions [offer] and [resume] are sufficient to write a parser loop.
     One can imagine many variations (which is why we expose these functions
     in the first place!). Here, we expose a few variations of the main loop,
     ready for use. *)

  (* [loop supplier checkpoint] begins parsing from [checkpoint], reading
     tokens from [supplier]. It continues parsing until it reaches a
     checkpoint of the form [Accepted v] or [Rejected]. In the former case, it
     returns [v]. In the latter case, it raises the exception [Error]. *)

  val loop: supplier -> 'a checkpoint -> 'a

  (* [loop_handle succeed fail supplier checkpoint] begins parsing from
     [checkpoint], reading tokens from [supplier]. It continues parsing until
     it reaches a checkpoint of the form [Accepted v] or [HandlingError env]
     (or [Rejected], but that should not happen, as [HandlingError _] will be
     observed first). In the former case, it calls [succeed v]. In the latter
     case, it calls [fail] with this checkpoint. It cannot raise [Error].

     This means that Menhir's traditional error-handling procedure (which pops
     the stack until a state that can act on the [error] token is found) does
     not get a chance to run. Instead, the user can implement her own error
     handling code, in the [fail] continuation. *)

  val loop_handle:
    ('a -> 'answer) ->
    ('a checkpoint -> 'answer) ->
    supplier -> 'a checkpoint -> 'answer

  (* [loop_handle_undo] is analogous to [loop_handle], except it passes a pair
     of checkpoints to the failure continuation.

     The first (and oldest) checkpoint is the last [InputNeeded] checkpoint that
     was encountered before the error was detected. The second (and newest)
     checkpoint is where the error was detected, as in [loop_handle]. Going back
     to the first checkpoint can be thought of as undoing any reductions that
     were performed after seeing the problematic token. (These reductions must
     be default reductions or spurious reductions.)

     [loop_handle_undo] must initially be applied to an [InputNeeded] checkpoint.
     The parser's initial checkpoints satisfy this constraint. *)

  val loop_handle_undo:
    ('a -> 'answer) ->
    ('a checkpoint -> 'a checkpoint -> 'answer) ->
    supplier -> 'a checkpoint -> 'answer

  (* [shifts checkpoint] assumes that [checkpoint] has been obtained by
     submitting a token to the parser. It runs the parser from [checkpoint],
     through an arbitrary number of reductions, until the parser either
     accepts this token (i.e., shifts) or rejects it (i.e., signals an error).
     If the parser decides to shift, then [Some env] is returned, where [env]
     is the parser's state just before shifting. Otherwise, [None] is
     returned. *)

  (* It is desirable that the semantic actions be side-effect free, or that
     their side-effects be harmless (replayable). *)

  val shifts: 'a checkpoint -> 'a env option

  (* The function [acceptable] allows testing, after an error has been
     detected, which tokens would have been accepted at this point. It is
     implemented using [shifts]. Its argument should be an [InputNeeded]
     checkpoint. *)

  (* For completeness, one must undo any spurious reductions before carrying out
     this test -- that is, one must apply [acceptable] to the FIRST checkpoint
     that is passed by [loop_handle_undo] to its failure continuation. *)

  (* This test causes some semantic actions to be run! The semantic actions
     should be side-effect free, or their side-effects should be harmless. *)

  (* The position [pos] is used as the start and end positions of the
     hypothetical token, and may be picked up by the semantic actions. We
     suggest using the position where the error was detected. *)

  val acceptable: 'a checkpoint -> token -> position -> bool

  (* The abstract type ['a lr1state] describes the non-initial states of the
     LR(1) automaton. The index ['a] represents the type of the semantic value
     associated with this state's incoming symbol. *)

  type 'a lr1state

  (* The states of the LR(1) automaton are numbered (from 0 and up). *)

  val number: _ lr1state -> int

  (* Productions are numbered. *)

  (* [find_production i] requires the index [i] to be valid. Use with care. *)

  val production_index: production -> int
  val find_production: int -> production

  (* An element is a pair of a non-initial state [s] and a semantic value [v]
     associated with the incoming symbol of this state. The idea is, the value
     [v] was pushed onto the stack just before the state [s] was entered. Thus,
     for some type ['a], the state [s] has type ['a lr1state] and the value [v]
     has type ['a]. In other words, the type [element] is an existential type. *)

  type element =
    | Element: 'a lr1state * 'a * position * position -> element

  (* The parser's stack is (or, more precisely, can be viewed as) a stream of
     elements. The type [stream] is defined by the module [General]. *)

  (* As of 2017/03/31, the types [stream] and [stack] and the function [stack]
     are DEPRECATED. They might be removed in the future. An alternative way
     of inspecting the stack is via the functions [top] and [pop]. *)

  type stack = (* DEPRECATED *)
    element stream

  (* This is the parser's stack, a stream of elements. This stream is empty if
     the parser is in an initial state; otherwise, it is non-empty.  The LR(1)
     automaton's current state is the one found in the top element of the
     stack. *)

  val stack: 'a env -> stack (* DEPRECATED *)

  (* [top env] returns the parser's top stack element. The state contained in
     this stack element is the current state of the automaton. If the stack is
     empty, [None] is returned. In that case, the current state of the
     automaton must be an initial state. *)

  val top: 'a env -> element option

  (* [pop_many i env] pops [i] cells off the automaton's stack. This is done
     via [i] successive invocations of [pop]. Thus, [pop_many 1] is [pop]. The
     index [i] must be nonnegative. The time complexity is O(i). *)

  val pop_many: int -> 'a env -> 'a env option

  (* [get i env] returns the parser's [i]-th stack element. The index [i] is
     0-based: thus, [get 0] is [top]. If [i] is greater than or equal to the
     number of elements in the stack, [None] is returned. The time complexity
     is O(i). *)

  val get: int -> 'a env -> element option

  (* [current_state_number env] is (the integer number of) the automaton's
     current state. This works even if the automaton's stack is empty, in
     which case the current state is an initial state. This number can be
     passed as an argument to a [message] function generated by [menhir
     --compile-errors]. *)

  val current_state_number: 'a env -> int

  (* [equal env1 env2] tells whether the parser configurations [env1] and
     [env2] are equal in the sense that the automaton's current state is the
     same in [env1] and [env2] and the stack is *physically* the same in
     [env1] and [env2]. If [equal env1 env2] is [true], then the sequence of
     the stack elements, as observed via [pop] and [top], must be the same in
     [env1] and [env2]. Also, if [equal env1 env2] holds, then the checkpoints
     [input_needed env1] and [input_needed env2] must be equivalent. The
     function [equal] has time complexity O(1). *)

  val equal: 'a env -> 'a env -> bool

  (* These are the start and end positions of the current lookahead token. If
     invoked in an initial state, this function returns a pair of twice the
     initial position. *)

  val positions: 'a env -> position * position

  (* When applied to an environment taken from a checkpoint of the form
     [AboutToReduce (env, prod)], the function [env_has_default_reduction]
     tells whether the reduction that is about to take place is a default
     reduction. *)

  val env_has_default_reduction: 'a env -> bool

  (* [state_has_default_reduction s] tells whether the state [s] has a default
     reduction. This includes the case where [s] is an accepting state. *)

  val state_has_default_reduction: _ lr1state -> bool

  (* [pop env] returns a new environment, where the parser's top stack cell
     has been popped off. (If the stack is empty, [None] is returned.) This
     amounts to pretending that the (terminal or nonterminal) symbol that
     corresponds to this stack cell has not been read. *)

  val pop: 'a env -> 'a env option

  (* [force_reduction prod env] should be called only if in the state [env]
     the parser is capable of reducing the production [prod]. If this
     condition is satisfied, then this production is reduced, which means that
     its semantic action is executed (this can have side effects!) and the
     automaton makes a goto (nonterminal) transition. If this condition is not
     satisfied, [Invalid_argument _] is raised. *)

  val force_reduction: production -> 'a env -> 'a env

  (* [input_needed env] returns [InputNeeded env]. That is, out of an [env]
     that might have been obtained via a series of calls to the functions
     [pop], [force_reduction], [feed], etc., it produces a checkpoint, which
     can be used to resume normal parsing, by supplying this checkpoint as an
     argument to [offer]. *)

  (* This function should be used with some care. It could "mess up the
     lookahead" in the sense that it allows parsing to resume in an arbitrary
     state [s] with an arbitrary lookahead symbol [t], even though Menhir's
     reachability analysis (menhir --list-errors) might well think that it is
     impossible to reach this particular configuration. If one is using
     Menhir's new error reporting facility, this could cause the parser to
     reach an error state for which no error message has been prepared. *)

  val input_needed: 'a env -> 'a checkpoint

end

(* This signature is a fragment of the inspection API that is made available
   to the user when [--inspection] is used. This fragment contains type
   definitions for symbols. *)

module type SYMBOLS = sig

  (* The type ['a terminal] represents a terminal symbol. The type ['a
     nonterminal] represents a nonterminal symbol. In both cases, the index
     ['a] represents the type of the semantic values associated with this
     symbol. The concrete definitions of these types are generated. *)

  type 'a terminal
  type 'a nonterminal

  (* The type ['a symbol] represents a terminal or nonterminal symbol. It is
     the disjoint union of the types ['a terminal] and ['a nonterminal]. *)

  type 'a symbol =
    | T : 'a terminal -> 'a symbol
    | N : 'a nonterminal -> 'a symbol

  (* The type [xsymbol] is an existentially quantified version of the type
     ['a symbol]. This type is useful in situations where the index ['a]
     is not statically known. *)

  type xsymbol =
    | X : 'a symbol -> xsymbol

end

(* This signature describes the inspection API that is made available to the
   user when [--inspection] is used. *)

module type INSPECTION = sig

  (* The types of symbols are described above. *)

  include SYMBOLS

  (* The type ['a lr1state] is meant to be the same as in [INCREMENTAL_ENGINE]. *)

  type 'a lr1state

  (* The type [production] is meant to be the same as in [INCREMENTAL_ENGINE].
     It represents a production of the grammar. A production can be examined
     via the functions [lhs] and [rhs] below. *)

  type production

  (* An LR(0) item is a pair of a production [prod] and a valid index [i] into
     this production. That is, if the length of [rhs prod] is [n], then [i] is
     comprised between 0 and [n], inclusive. *)

  type item =
      production * int

  (* Ordering functions. *)

  val compare_terminals: _ terminal -> _ terminal -> int
  val compare_nonterminals: _ nonterminal -> _ nonterminal -> int
  val compare_symbols: xsymbol -> xsymbol -> int
  val compare_productions: production -> production -> int
  val compare_items: item -> item -> int

  (* [incoming_symbol s] is the incoming symbol of the state [s], that is,
     the symbol that the parser must recognize before (has recognized when)
     it enters the state [s]. This function gives access to the semantic
     value [v] stored in a stack element [Element (s, v, _, _)]. Indeed,
     by case analysis on the symbol [incoming_symbol s], one discovers the
     type ['a] of the value [v]. *)

  val incoming_symbol: 'a lr1state -> 'a symbol

  (* [items s] is the set of the LR(0) items in the LR(0) core of the LR(1)
     state [s]. This set is not epsilon-closed. This set is presented as a
     list, in an arbitrary order. *)

  val items: _ lr1state -> item list

  (* [lhs prod] is the left-hand side of the production [prod]. This is
     always a non-terminal symbol. *)

  val lhs: production -> xsymbol

  (* [rhs prod] is the right-hand side of the production [prod]. This is
     a (possibly empty) sequence of (terminal or nonterminal) symbols. *)

  val rhs: production -> xsymbol list

  (* [nullable nt] tells whether the non-terminal symbol [nt] is nullable.
     That is, it is true if and only if this symbol produces the empty
     word [epsilon]. *)

  val nullable: _ nonterminal -> bool

  (* [first nt t] tells whether the FIRST set of the nonterminal symbol [nt]
     contains the terminal symbol [t]. That is, it is true if and only if
     [nt] produces a word that begins with [t]. *)

  val first: _ nonterminal -> _ terminal -> bool

  (* [xfirst] is analogous to [first], but expects a first argument of type
     [xsymbol] instead of [_ terminal]. *)

  val xfirst: xsymbol -> _ terminal -> bool

  (* [foreach_terminal] enumerates the terminal symbols, including [error].
     [foreach_terminal_but_error] enumerates the terminal symbols, excluding
     [error]. *)

  val foreach_terminal:           (xsymbol -> 'a -> 'a) -> 'a -> 'a
  val foreach_terminal_but_error: (xsymbol -> 'a -> 'a) -> 'a -> 'a

  (* The type [env] is meant to be the same as in [INCREMENTAL_ENGINE]. *)

  type 'a env

  (* [feed symbol startp semv endp env] causes the parser to consume the
     (terminal or nonterminal) symbol [symbol], accompanied with the semantic
     value [semv] and with the start and end positions [startp] and [endp].
     Thus, the automaton makes a transition, and reaches a new state. The
     stack grows by one cell. This operation is permitted only if the current
     state (as determined by [env]) has an outgoing transition labeled with
     [symbol]. Otherwise, [Invalid_argument _] is raised. *)

  val feed: 'a symbol -> position -> 'a -> position -> 'b env -> 'b env

end

(* This signature combines the incremental API and the inspection API. *)

module type EVERYTHING = sig

  include INCREMENTAL_ENGINE

  include INSPECTION
    with type 'a lr1state := 'a lr1state
    with type production := production
    with type 'a env := 'a env

end
