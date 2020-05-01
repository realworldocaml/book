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

(* This file defines several types and module types that are used in the
   specification of module [Engine]. *)

(* --------------------------------------------------------------------------- *)

(* It would be nice if we could keep the structure of stacks and environments
   hidden. However, stacks and environments must be accessible to semantic
   actions, so the following data structure definitions must be public. *)

(* --------------------------------------------------------------------------- *)

(* A stack is a linked list of cells. A sentinel cell -- which is its own
   successor -- is used to mark the bottom of the stack. The sentinel cell
   itself is not significant -- it contains dummy values. *)

type ('state, 'semantic_value) stack = {

  (* The state that we should go back to if we pop this stack cell. *)

  (* This convention means that the state contained in the top stack cell is
     not the current state [env.current]. It also means that the state found
     within the sentinel is a dummy -- it is never consulted. This convention
     is the same as that adopted by the code-based back-end. *)

  state: 'state;

  (* The semantic value associated with the chunk of input that this cell
     represents. *)

  semv: 'semantic_value;

  (* The start and end positions of the chunk of input that this cell
     represents. *)

  startp: Lexing.position;
  endp: Lexing.position;

  (* The next cell down in the stack. If this is a self-pointer, then this
     cell is the sentinel, and the stack is conceptually empty. *)

  next: ('state, 'semantic_value) stack;

}

(* --------------------------------------------------------------------------- *)

(* A parsing environment contains all of the parser's state (except for the
   current program point). *)

type ('state, 'semantic_value, 'token) env = {

  (* If this flag is true, then the first component of [env.triple] should
     be ignored, as it has been logically overwritten with the [error]
     pseudo-token. *)

  error: bool;

  (* The last token that was obtained from the lexer, together with its start
     and end positions. Warning: before the first call to the lexer has taken
     place, a dummy (and possibly invalid) token is stored here. *)

  triple: 'token * Lexing.position * Lexing.position;

  (* The stack. In [CodeBackend], it is passed around on its own,
     whereas, here, it is accessed via the environment. *)

  stack: ('state, 'semantic_value) stack;

  (* The current state. In [CodeBackend], it is passed around on its
     own, whereas, here, it is accessed via the environment. *)

  current: 'state;

}

(* --------------------------------------------------------------------------- *)

(* This signature describes the parameters that must be supplied to the LR
   engine. *)

module type TABLE = sig

  (* The type of automaton states. *)

  type state

  (* States are numbered. *)

  val number: state -> int

  (* The type of tokens. These can be thought of as real tokens, that is,
     tokens returned by the lexer. They carry a semantic value. This type
     does not include the [error] pseudo-token. *)

  type token

  (* The type of terminal symbols. These can be thought of as integer codes.
     They do not carry a semantic value. This type does include the [error]
     pseudo-token. *)

  type terminal

  (* The type of nonterminal symbols. *)

  type nonterminal

  (* The type of semantic values. *)

  type semantic_value

  (* A token is conceptually a pair of a (non-[error]) terminal symbol and
     a semantic value. The following two functions are the pair projections. *)

  val token2terminal: token -> terminal
  val token2value: token -> semantic_value

  (* Even though the [error] pseudo-token is not a real token, it is a
     terminal symbol. Furthermore, for regularity, it must have a semantic
     value. *)

  val error_terminal: terminal
  val error_value: semantic_value

  (* [foreach_terminal] allows iterating over all terminal symbols. *)

  val foreach_terminal: (terminal -> 'a -> 'a) -> 'a -> 'a

  (* The type of productions. *)

  type production

  val production_index: production -> int
  val find_production: int -> production

  (* If a state [s] has a default reduction on production [prod], then, upon
     entering [s], the automaton should reduce [prod] without consulting the
     lookahead token. The following function allows determining which states
     have default reductions. *)

  (* Instead of returning a value of a sum type -- either [DefRed prod], or
     [NoDefRed] -- it accepts two continuations, and invokes just one of
     them. This mechanism allows avoiding a memory allocation. *)

  val default_reduction:
    state ->
    ('env -> production -> 'answer) ->
    ('env -> 'answer) ->
    'env -> 'answer

  (* An LR automaton can normally take three kinds of actions: shift, reduce,
     or fail. (Acceptance is a particular case of reduction: it consists in
     reducing a start production.) *)

  (* There are two variants of the shift action. [shift/discard s] instructs
     the automaton to discard the current token, request a new one from the
     lexer, and move to state [s]. [shift/nodiscard s] instructs it to move to
     state [s] without requesting a new token. This instruction should be used
     when [s] has a default reduction on [#]. See [CodeBackend.gettoken] for
     details. *)

  (* This is the automaton's action table. It maps a pair of a state and a
     terminal symbol to an action. *)

  (* Instead of returning a value of a sum type -- one of shift/discard,
     shift/nodiscard, reduce, or fail -- this function accepts three
     continuations, and invokes just one them. This mechanism allows avoiding
     a memory allocation. *)

  (* In summary, the parameters to [action] are as follows:

     - the first two parameters, a state and a terminal symbol, are used to
       look up the action table;

     - the next parameter is the semantic value associated with the above
       terminal symbol; it is not used, only passed along to the shift
       continuation, as explained below;

     - the shift continuation expects an environment; a flag that tells
       whether to discard the current token; the terminal symbol that
       is being shifted; its semantic value; and the target state of
       the transition;

     - the reduce continuation expects an environment and a production;

     - the fail continuation expects an environment;

     - the last parameter is the environment; it is not used, only passed
       along to the selected continuation. *)

  val action:
    state ->
    terminal ->
    semantic_value ->
    ('env -> bool -> terminal -> semantic_value -> state -> 'answer) ->
    ('env -> production -> 'answer) ->
    ('env -> 'answer) ->
    'env -> 'answer

  (* This is the automaton's goto table. This table maps a pair of a state
     and a nonterminal symbol to a new state. By extension, it also maps a
     pair of a state and a production to a new state. *)

  (* The function [goto_nt] can be applied to [s] and [nt] ONLY if the state
     [s] has an outgoing transition labeled [nt]. Otherwise, its result is
     undefined. Similarly, the call [goto_prod prod s] is permitted ONLY if
     the state [s] has an outgoing transition labeled with the nonterminal
     symbol [lhs prod]. The function [maybe_goto_nt] involves an additional
     dynamic check and CAN be called even if there is no outgoing transition. *)

  val       goto_nt  : state -> nonterminal -> state
  val       goto_prod: state -> production  -> state
  val maybe_goto_nt:   state -> nonterminal -> state option

  (* [is_start prod] tells whether the production [prod] is a start production. *)

  val is_start: production -> bool

  (* By convention, a semantic action is responsible for:

     1. fetching whatever semantic values and positions it needs off the stack;

     2. popping an appropriate number of cells off the stack, as dictated
        by the length of the right-hand side of the production;

     3. computing a new semantic value, as well as new start and end positions;

     4. pushing a new stack cell, which contains the three values
        computed in step 3;

     5. returning the new stack computed in steps 2 and 4.

     Point 1 is essentially forced upon us: if semantic values were fetched
     off the stack by this interpreter, then the calling convention for
     semantic actions would be variadic: not all semantic actions would have
     the same number of arguments. The rest follows rather naturally. *)

  (* Semantic actions are allowed to raise [Error]. *)

  exception Error

  type semantic_action =
      (state, semantic_value, token) env -> (state, semantic_value) stack

  val semantic_action: production -> semantic_action

  (* [may_reduce state prod] tests whether the state [state] is capable of
     reducing the production [prod]. This function is currently costly and
     is not used by the core LR engine. It is used in the implementation
     of certain functions, such as [force_reduction], which allow the engine
     to be driven programmatically. *)

  val may_reduce: state -> production -> bool

  (* The LR engine requires a number of hooks, which are used for logging. *)

  (* The comments below indicate the conventional messages that correspond
     to these hooks in the code-based back-end; see [CodeBackend]. *)

  (* If the flag [log] is false, then the logging functions are not called.
     If it is [true], then they are called. *)

  val log : bool

  module Log : sig

    (* State %d: *)

    val state: state -> unit

    (* Shifting (<terminal>) to state <state> *)

    val shift: terminal -> state -> unit

    (* Reducing a production should be logged either as a reduction
       event (for regular productions) or as an acceptance event (for
       start productions). *)

    (* Reducing production <production> / Accepting *)

    val reduce_or_accept: production -> unit

    (* Lookahead token is now <terminal> (<pos>-<pos>) *)

    val lookahead_token: terminal -> Lexing.position -> Lexing.position -> unit

    (* Initiating error handling *)

    val initiating_error_handling: unit -> unit

    (* Resuming error handling *)

    val resuming_error_handling: unit -> unit

    (* Handling error in state <state> *)

    val handling_error: state -> unit

  end

end

(* --------------------------------------------------------------------------- *)

(* This signature describes the monolithic (traditional) LR engine. *)

(* In this interface, the parser controls the lexer. *)

module type MONOLITHIC_ENGINE = sig

  type state

  type token

  type semantic_value

  (* An entry point to the engine requires a start state, a lexer, and a lexing
     buffer. It either succeeds and produces a semantic value, or fails and
     raises [Error]. *)

  exception Error

  val entry:
    state ->
    (Lexing.lexbuf -> token) ->
    Lexing.lexbuf ->
    semantic_value

end

(* --------------------------------------------------------------------------- *)

(* The following signatures describe the incremental LR engine. *)

(* First, see [INCREMENTAL_ENGINE] in the file [IncrementalEngine.ml]. *)

(* The [start] function is set apart because we do not wish to publish
   it as part of the generated [parser.mli] file. Instead, the table
   back-end will publish specialized versions of it, with a suitable
   type cast. *)

module type INCREMENTAL_ENGINE_START = sig

  (* [start] is an entry point. It requires a start state and a start position
     and begins the parsing process. If the lexer is based on an OCaml lexing
     buffer, the start position should be [lexbuf.lex_curr_p]. [start] produces
     a checkpoint, which usually will be an [InputNeeded] checkpoint. (It could
     be [Accepted] if this starting state accepts only the empty word. It could
     be [Rejected] if this starting state accepts no word at all.) It does not
     raise any exception. *)

  (* [start s pos] should really produce a checkpoint of type ['a checkpoint],
     for a fixed ['a] that depends on the state [s]. We cannot express this, so
     we use [semantic_value checkpoint], which is safe. The table back-end uses
     [Obj.magic] to produce safe specialized versions of [start]. *)

  type state
  type semantic_value
  type 'a checkpoint

  val start:
    state ->
    Lexing.position ->
    semantic_value checkpoint

end

(* --------------------------------------------------------------------------- *)

(* This signature describes the LR engine, which combines the monolithic
   and incremental interfaces. *)

module type ENGINE = sig

  include MONOLITHIC_ENGINE

  include IncrementalEngine.INCREMENTAL_ENGINE
    with type token := token
     and type 'a lr1state = state (* useful for us; hidden from the end user *)

  include INCREMENTAL_ENGINE_START
    with type state := state
     and type semantic_value := semantic_value
     and type 'a checkpoint := 'a checkpoint

end
