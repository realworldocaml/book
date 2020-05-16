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
open EngineTypes

(* The LR parsing engine. *)

(* This module is used:

   - at compile time, if so requested by the user, via the --interpret options;
   - at run time, in the table-based back-end. *)

module Make (T : TABLE) = struct

  (* This propagates type and exception definitions. The functions [number],
     [production_index], [find_production], too, are defined by this [include]
     declaration. *)

  include T

  type 'a env =
      (state, semantic_value, token) EngineTypes.env

  (* ------------------------------------------------------------------------ *)

  (* The type [checkpoint] represents an intermediate or final result of the
     parser. See [EngineTypes]. *)

  (* The type [checkpoint] is presented to the user as a private type (see
     [IncrementalEngine]). This prevents the user from manufacturing
     checkpoints (i.e., continuations) that do not make sense. (Such
     continuations could potentially violate the LR invariant and lead to
     crashes.) *)

  (* 2017/03/29 Although [checkpoint] is a private type, we now expose a
     constructor function, [input_needed]. This function allows manufacturing
     a checkpoint out of an environment. For this reason, the type [env] must
     also be parameterized with ['a]. *)

  type 'a checkpoint =
    | InputNeeded of 'a env
    | Shifting of 'a env * 'a env * bool
    | AboutToReduce of 'a env * production
    | HandlingError of 'a env
    | Accepted of 'a
    | Rejected

  (* ------------------------------------------------------------------------ *)

  (* In the code-based back-end, the [run] function is sometimes responsible
     for pushing a new cell on the stack. This is motivated by code sharing
     concerns. In this interpreter, there is no such concern; [run]'s caller
     is always responsible for updating the stack. *)

  (* In the code-based back-end, there is a [run] function for each state
     [s]. This function can behave in two slightly different ways, depending
     on when it is invoked, or (equivalently) depending on [s].

     If [run] is invoked after shifting a terminal symbol (or, equivalently,
     if [s] has a terminal incoming symbol), then [run] discards a token,
     unless [s] has a default reduction on [#]. (Indeed, in that case,
     requesting the next token might drive the lexer off the end of the input
     stream.)

     If, on the other hand, [run] is invoked after performing a goto
     transition, or invoked directly by an entry point, then there is nothing
     to discard.

     These two cases are reflected in [CodeBackend.gettoken].

     Here, the code is structured in a slightly different way. It is up to the
     caller of [run] to indicate whether to discard a token, via the parameter
     [please_discard]. This flag is set when [s] is being entered by shifting
     a terminal symbol and [s] does not have a default reduction on [#]. *)

  (* The following recursive group of functions are tail recursive, produce a
     checkpoint of type [semantic_value checkpoint], and cannot raise an
     exception. A semantic action can raise [Error], but this exception is
     immediately caught within [reduce]. *)

  let rec run env please_discard : semantic_value checkpoint =

    (* Log the fact that we just entered this state. *)

    if log then
      Log.state env.current;

    (* If [please_discard] is set, we discard the current lookahead token and
       fetch the next one. In order to request a token from the user, we
       return an [InputNeeded] continuation, which, when invoked by the user,
       will take us to [discard]. If [please_discard] is not set, we skip this
       step and jump directly to [check_for_default_reduction]. *)

    if please_discard then
      InputNeeded env
    else
      check_for_default_reduction env

  (* [discard env triple] stores [triple] into [env], overwriting the previous
     token. It is invoked by [offer], which itself is invoked by the user in
     response to an [InputNeeded] checkpoint. *)

  and discard env triple =
    if log then begin
      let (token, startp, endp) = triple in
      Log.lookahead_token (T.token2terminal token) startp endp
    end;
    let env = { env with error = false; triple } in
    check_for_default_reduction env

  and check_for_default_reduction env =

    (* Examine what situation we are in. This case analysis is analogous to
       that performed in [CodeBackend.gettoken], in the sub-case where we do
       not have a terminal incoming symbol. *)

    T.default_reduction
      env.current
      announce_reduce       (* there is a default reduction; perform it *)
      check_for_error_token (* there is none; continue below *)
      env

  and check_for_error_token env =

    (* There is no default reduction. Consult the current lookahead token
       so as to determine which action should be taken. *)

    (* Peeking at the first input token, without taking it off the input
       stream, is done by reading [env.triple]. We are careful to first
       check [env.error]. *)

    (* Note that, if [please_discard] was true, then we have just called
       [discard], so the lookahead token cannot be [error]. *)

    (* Returning [HandlingError env] is equivalent to calling [error env]
       directly, except it allows the user to regain control. *)

    if env.error then begin
      if log then
        Log.resuming_error_handling();
      HandlingError env
    end
    else
      let (token, _, _) = env.triple in

      (* We consult the two-dimensional action table, indexed by the
         current state and the current lookahead token, in order to
         determine which action should be taken. *)

      T.action
        env.current                    (* determines a row *)
        (T.token2terminal token)       (* determines a column *)
        (T.token2value token)
        shift                          (* shift continuation *)
        announce_reduce                (* reduce continuation *)
        initiate                       (* failure continuation *)
        env

  (* ------------------------------------------------------------------------ *)

  (* This function takes care of shift transitions along a terminal symbol.
     (Goto transitions are taken care of within [reduce] below.) The symbol
     can be either an actual token or the [error] pseudo-token. *)

  (* Here, the lookahead token CAN be [error]. *)

  and shift env
      (please_discard : bool)
      (terminal : terminal)
      (value : semantic_value)
      (s' : state) =

    (* Log the transition. *)

    if log then
      Log.shift terminal s';

    (* Push a new cell onto the stack, containing the identity of the
       state that we are leaving. *)

    let (_, startp, endp) = env.triple in
    let stack = {
      state = env.current;
      semv = value;
      startp;
      endp;
      next = env.stack;
    } in

    (* Switch to state [s']. *)

    let new_env = { env with stack; current = s' } in

    (* Expose the transition to the user. (In principle, we have a choice
       between exposing the transition before we take it, after we take
       it, or at some point in between. This affects the number and type
       of the parameters carried by [Shifting]. Here, we choose to expose
       the transition after we take it; this allows [Shifting] to carry
       only three parameters, whose meaning is simple.) *)

    Shifting (env, new_env, please_discard)

  (* ------------------------------------------------------------------------ *)

  (* The function [announce_reduce] stops the parser and returns a checkpoint
     which allows the parser to be resumed by calling [reduce]. *)

  (* Only ordinary productions are exposed to the user. Start productions
     are not exposed to the user. Reducing a start production simply leads
     to the successful termination of the parser. *)

  and announce_reduce env (prod : production) =
    if T.is_start prod then
      accept env prod
    else
      AboutToReduce (env, prod)

  (* The function [reduce] takes care of reductions. It is invoked by
     [resume] after an [AboutToReduce] event has been produced. *)

  (* Here, the lookahead token CAN be [error]. *)

  (* The production [prod] CANNOT be a start production. *)

  and reduce env (prod : production) =

    (* Log a reduction event. *)

    if log then
      Log.reduce_or_accept prod;

    (* Invoke the semantic action. The semantic action is responsible for
       truncating the stack and pushing a new cell onto the stack, which
       contains a new semantic value. It can raise [Error]. *)

    (* If the semantic action terminates normally, it returns a new stack,
       which becomes the current stack. *)

    (* If the semantic action raises [Error], we catch it and initiate error
       handling. *)

    (* This [match/with/exception] construct requires OCaml 4.02. *)

    match T.semantic_action prod env with
    | stack ->

        (* By our convention, the semantic action has produced an updated
           stack. The state now found in the top stack cell is the return
           state. *)

        (* Perform a goto transition. The target state is determined
           by consulting the goto table at the return state and at
           production [prod]. *)

        let current = T.goto_prod stack.state prod in
        let env = { env with stack; current } in
        run env false

    | exception Error ->
        initiate env

  and accept env prod =
    (* Log an accept event. *)
    if log then
      Log.reduce_or_accept prod;
    (* Extract the semantic value out of the stack. *)
    let v = env.stack.semv in
    (* Finish. *)
    Accepted v

  (* ------------------------------------------------------------------------ *)

  (* The following functions deal with errors. *)

  (* [initiate] initiates or resumes error handling. *)

  (* Here, the lookahead token CAN be [error]. *)

  and initiate env =
    if log then
      Log.initiating_error_handling();
    let env = { env with error = true } in
    HandlingError env

  (* [error] handles errors. *)

  and error env =
    assert env.error;

    (* Consult the column associated with the [error] pseudo-token in the
       action table. *)

    T.action
      env.current                    (* determines a row *)
      T.error_terminal               (* determines a column *)
      T.error_value
      error_shift                    (* shift continuation *)
      error_reduce                   (* reduce continuation *)
      error_fail                     (* failure continuation *)
      env

  and error_shift env please_discard terminal value s' =

    (* Here, [terminal] is [T.error_terminal],
       and [value] is [T.error_value]. *)

    assert (terminal = T.error_terminal && value = T.error_value);

    (* This state is capable of shifting the [error] token. *)

    if log then
      Log.handling_error env.current;
    shift env please_discard terminal value s'

  and error_reduce env prod =

    (* This state is capable of performing a reduction on [error]. *)

    if log then
      Log.handling_error env.current;
    reduce env prod
      (* Intentionally calling [reduce] instead of [announce_reduce].
         It does not seem very useful, and it could be confusing, to
         expose the reduction steps taken during error handling. *)

  and error_fail env =

    (* This state is unable to handle errors. Attempt to pop a stack
       cell. *)

    let cell = env.stack in
    let next = cell.next in
    if next == cell then

      (* The stack is empty. Die. *)

      Rejected

    else begin

      (* The stack is nonempty. Pop a cell, updating the current state
         with that found in the popped cell, and try again. *)

      let env = { env with
        stack = next;
        current = cell.state
      } in
      HandlingError env

    end

  (* End of the nest of tail recursive functions. *)

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (* The incremental interface. See [EngineTypes]. *)

  (* [start s] begins the parsing process. *)

  let start (s : state) (initial : position) : semantic_value checkpoint =

    (* Build an empty stack. This is a dummy cell, which is its own successor.
       Its [next] field WILL be accessed by [error_fail] if an error occurs and
       is propagated all the way until the stack is empty. Its [endp] field WILL
       be accessed (by a semantic action) if an epsilon production is reduced
       when the stack is empty. *)

    let rec empty = {
      state = s;                          (* dummy *)
      semv = T.error_value;               (* dummy *)
      startp = initial;                   (* dummy *)
      endp = initial;
      next = empty;
    } in

    (* Build an initial environment. *)

    (* Unfortunately, there is no type-safe way of constructing a
       dummy token. Tokens carry semantic values, which in general
       we cannot manufacture. This instance of [Obj.magic] could
       be avoided by adopting a different representation (e.g., no
       [env.error] field, and an option in the first component of
       [env.triple]), but I like this representation better. *)

    let dummy_token = Obj.magic () in
    let env = {
      error = false;
      triple = (dummy_token, initial, initial); (* dummy *)
      stack = empty;
      current = s;
    } in

    (* Begin parsing. *)

    (* The parameter [please_discard] here is [true], which means we know
       that we must read at least one token. This claim relies on the fact
       that we have ruled out the two special cases where a start symbol
       recognizes the empty language or the singleton language {epsilon}. *)

    run env true

  (* [offer checkpoint triple] is invoked by the user in response to a
     checkpoint of the form [InputNeeded env]. It checks that [checkpoint] is
     indeed of this form, and invokes [discard]. *)

  (* [resume checkpoint] is invoked by the user in response to a checkpoint of
     the form [AboutToReduce (env, prod)] or [HandlingError env]. It checks
     that [checkpoint] is indeed of this form, and invokes [reduce] or
     [error], as appropriate. *)

  (* In reality, [offer] and [resume] accept an argument of type
     [semantic_value checkpoint] and produce a checkpoint of the same type.
     The choice of [semantic_value] is forced by the fact that this is the
     parameter of the checkpoint [Accepted]. *)

  (* We change this as follows. *)

  (* We change the argument and result type of [offer] and [resume] from
     [semantic_value checkpoint] to ['a checkpoint]. This is safe, in this
     case, because we give the user access to values of type [t checkpoint]
     only if [t] is indeed the type of the eventual semantic value for this
     run. (More precisely, by examining the signatures [INCREMENTAL_ENGINE]
     and [INCREMENTAL_ENGINE_START], one finds that the user can build a value
     of type ['a checkpoint] only if ['a] is [semantic_value]. The table
     back-end goes further than this and produces versions of [start] composed
     with a suitable cast, which give the user access to a value of type
     [t checkpoint] where [t] is the type of the start symbol.) *)

  let offer : 'a . 'a checkpoint ->
                   token * position * position ->
                   'a checkpoint
  = function
    | InputNeeded env ->
        Obj.magic discard env
    | _ ->
        invalid_arg "offer expects InputNeeded"

  let resume : 'a . 'a checkpoint -> 'a checkpoint = function
    | HandlingError env ->
        Obj.magic error env
    | Shifting (_, env, please_discard) ->
        Obj.magic run env please_discard
    | AboutToReduce (env, prod) ->
        Obj.magic reduce env prod
    | _ ->
        invalid_arg "resume expects HandlingError | Shifting | AboutToReduce"

  (* ------------------------------------------------------------------------ *)
  (* ------------------------------------------------------------------------ *)

  (* The traditional interface. See [EngineTypes]. *)

  (* ------------------------------------------------------------------------ *)

  (* Wrapping a lexer and lexbuf as a token supplier. *)

  type supplier =
    unit -> token * position * position

  let lexer_lexbuf_to_supplier
      (lexer : Lexing.lexbuf -> token)
      (lexbuf : Lexing.lexbuf)
  : supplier =
    fun () ->
      let token = lexer lexbuf in
      let startp = lexbuf.Lexing.lex_start_p
      and endp = lexbuf.Lexing.lex_curr_p in
      token, startp, endp

  (* ------------------------------------------------------------------------ *)

  (* The main loop repeatedly handles intermediate checkpoints, until a final
     checkpoint is obtained. This allows implementing the monolithic interface
     ([entry]) in terms of the incremental interface ([start], [offer],
     [handle], [reduce]). *)

  (* By convention, acceptance is reported by returning a semantic value,
     whereas rejection is reported by raising [Error]. *)

  (* [loop] is polymorphic in ['a]. No cheating is involved in achieving this.
     All of the cheating resides in the types assigned to [offer] and [handle]
     above. *)

  let rec loop : 'a . supplier -> 'a checkpoint -> 'a =
    fun read checkpoint ->
    match checkpoint with
    | InputNeeded _ ->
        (* The parser needs a token. Request one from the lexer,
           and offer it to the parser, which will produce a new
           checkpoint. Then, repeat. *)
        let triple = read() in
        let checkpoint = offer checkpoint triple in
        loop read checkpoint
    | Shifting _
    | AboutToReduce _
    | HandlingError _ ->
        (* The parser has suspended itself, but does not need
           new input. Just resume the parser. Then, repeat. *)
        let checkpoint = resume checkpoint in
        loop read checkpoint
    | Accepted v ->
        (* The parser has succeeded and produced a semantic value.
           Return this semantic value to the user. *)
        v
    | Rejected ->
        (* The parser rejects this input. Raise an exception. *)
        raise Error

  let entry (s : state) lexer lexbuf : semantic_value =
    let initial = lexbuf.Lexing.lex_curr_p in
    loop (lexer_lexbuf_to_supplier lexer lexbuf) (start s initial)

  (* ------------------------------------------------------------------------ *)

  (* [loop_handle] stops if it encounters an error, and at this point, invokes
     its failure continuation, without letting Menhir do its own traditional
     error-handling (which involves popping the stack, etc.). *)

  let rec loop_handle succeed fail read checkpoint =
    match checkpoint with
    | InputNeeded _ ->
        let triple = read() in
        let checkpoint = offer checkpoint triple in
        loop_handle succeed fail read checkpoint
    | Shifting _
    | AboutToReduce _ ->
        let checkpoint = resume checkpoint in
        loop_handle succeed fail read checkpoint
    | HandlingError _
    | Rejected ->
        (* The parser has detected an error. Invoke the failure continuation. *)
        fail checkpoint
    | Accepted v ->
        (* The parser has succeeded and produced a semantic value. Invoke the
           success continuation. *)
        succeed v

  (* ------------------------------------------------------------------------ *)

  (* [loop_handle_undo] is analogous to [loop_handle], except it passes a pair
     of checkpoints to the failure continuation.

     The first (and oldest) checkpoint is the last [InputNeeded] checkpoint
     that was encountered before the error was detected. The second (and
     newest) checkpoint is where the error was detected, as in [loop_handle].
     Going back to the first checkpoint can be thought of as undoing any
     reductions that were performed after seeing the problematic token. (These
     reductions must be default reductions or spurious reductions.) *)

  let rec loop_handle_undo succeed fail read (inputneeded, checkpoint) =
    match checkpoint with
    | InputNeeded _ ->
        (* Update the last recorded [InputNeeded] checkpoint. *)
        let inputneeded = checkpoint in
        let triple = read() in
        let checkpoint = offer checkpoint triple in
        loop_handle_undo succeed fail read (inputneeded, checkpoint)
    | Shifting _
    | AboutToReduce _ ->
        let checkpoint = resume checkpoint in
        loop_handle_undo succeed fail read (inputneeded, checkpoint)
    | HandlingError _
    | Rejected ->
        fail inputneeded checkpoint
    | Accepted v ->
        succeed v

  (* For simplicity, we publish a version of [loop_handle_undo] that takes a
     single checkpoint as an argument, instead of a pair of checkpoints. We
     check that the argument is [InputNeeded _], and duplicate it. *)

  (* The parser cannot accept or reject before it asks for the very first
     character of input. (Indeed, we statically reject a symbol that
     generates the empty language or the singleton language {epsilon}.)
     So, the [start] checkpoint must match [InputNeeded _]. Hence, it is
     permitted to call [loop_handle_undo] with a [start] checkpoint. *)

  let loop_handle_undo succeed fail read checkpoint =
    assert (match checkpoint with InputNeeded _ -> true | _ -> false);
    loop_handle_undo succeed fail read (checkpoint, checkpoint)

  (* ------------------------------------------------------------------------ *)

  let rec shifts checkpoint =
    match checkpoint with
    | Shifting (env, _, _) ->
        (* The parser is about to shift, which means it is willing to
           consume the terminal symbol that we have fed it. Return the
           state just before this transition. *)
        Some env
    | AboutToReduce _ ->
        (* The parser wishes to reduce. Just follow. *)
        shifts (resume checkpoint)
    | HandlingError _ ->
        (* The parser fails, which means it rejects the terminal symbol
           that we have fed it. *)
        None
    | InputNeeded _
    | Accepted _
    | Rejected ->
        (* None of these cases can arise. Indeed, after a token is submitted
           to it, the parser must shift, reduce, or signal an error, before
           it can request another token or terminate. *)
        assert false

  let acceptable checkpoint token pos =
    let triple = (token, pos, pos) in
    let checkpoint = offer checkpoint triple in
    match shifts checkpoint with
    | None      -> false
    | Some _env -> true

  (* ------------------------------------------------------------------------ *)

  (* The type ['a lr1state] describes the (non-initial) states of the LR(1)
     automaton. The index ['a] represents the type of the semantic value
     associated with the state's incoming symbol. *)

  (* The type ['a lr1state] is defined as an alias for [state], which itself
     is usually defined as [int] (see [TableInterpreter]). So, ['a lr1state]
     is technically a phantom type, but should really be thought of as a GADT
     whose data constructors happen to be represented as integers. It is
     presented to the user as an abstract type (see [IncrementalEngine]). *)

  type 'a lr1state =
      state

  (* ------------------------------------------------------------------------ *)

  (* Stack inspection. *)

  (* We offer a read-only view of the parser's state as a stream of elements.
     Each element contains a pair of a (non-initial) state and a semantic
     value associated with (the incoming symbol of) this state. Note that the
     type [element] is an existential type. *)

  (* As of 2017/03/31, the type [stack] and the function [stack] are DEPRECATED.
     If desired, they could now be implemented outside Menhir, by relying on
     the functions [top] and [pop]. *)

  type element =
    | Element: 'a lr1state * 'a * position * position -> element

  open General

  type stack =
    element stream

  (* If [current] is the current state and [cell] is the top stack cell,
     then [stack cell current] is a view of the parser's state as a stream
     of elements. *)

  let rec stack cell current : element stream =
    lazy (
      (* The stack is empty iff the top stack cell is its own successor. In
         that case, the current state [current] should be an initial state
         (which has no incoming symbol).
         We do not allow the user to inspect this state. *)
      let next = cell.next in
      if next == cell then
        Nil
      else
        (* Construct an element containing the current state [current] as well
           as the semantic value contained in the top stack cell. This semantic
           value is associated with the incoming symbol of this state, so it
           makes sense to pair them together. The state has type ['a state] and
           the semantic value has type ['a], for some type ['a]. Here, the OCaml
           type-checker thinks ['a] is [semantic_value] and considers this code
           well-typed. Outside, we will use magic to provide the user with a way
           of inspecting states and recovering the value of ['a]. *)
        let element = Element (
          current,
          cell.semv,
          cell.startp,
          cell.endp
        ) in
        Cons (element, stack next cell.state)
    )

  let stack env : element stream =
    stack env.stack env.current

  (* As explained above, the function [top] allows access to the top stack
     element only if the stack is nonempty, i.e., only if the current state
     is not an initial state. *)

  let top env : element option =
    let cell = env.stack in
    let next = cell.next in
    if next == cell then
      None
    else
      Some (Element (env.current, cell.semv, cell.startp, cell.endp))

  (* [equal] compares the stacks for physical equality, and compares the
     current states via their numbers (this seems cleaner than using OCaml's
     polymorphic equality). *)

  (* The two fields that are not compared by [equal], namely [error] and
     [triple], are overwritten by the function [discard], which handles
     [InputNeeded] checkpoints. Thus, if [equal env1 env2] holds, then the
     checkpoints [input_needed env1] and [input_needed env2] are
     equivalent: they lead the parser to behave in the same way. *)

  let equal env1 env2 =
    env1.stack == env2.stack &&
    number env1.current = number env2.current

  let current_state_number env =
    number env.current

  (* ------------------------------------------------------------------------ *)

  (* Access to the position of the lookahead token. *)

  let positions { triple = (_, startp, endp); _ } =
    startp, endp

  (* ------------------------------------------------------------------------ *)

  (* Access to information about default reductions. *)

  (* This can be a function of states, or a function of environments.
     We offer both. *)

  (* Instead of a Boolean result, we could return a [production option].
     However, we would have to explicitly test whether [prod] is a start
     production, and in that case, return [None], I suppose. Indeed, we
     have decided not to expose the start productions. *)

  let state_has_default_reduction (state : _ lr1state) : bool =
    T.default_reduction state
      (fun _env _prod -> true)
      (fun _env -> false)
      ()

  let env_has_default_reduction env =
    state_has_default_reduction env.current

  (* ------------------------------------------------------------------------ *)

  (* The following functions work at the level of environments (as opposed to
     checkpoints). The function [pop] causes the automaton to go back into the
     past, pretending that the last input symbol has never been read. The
     function [force_reduction] causes the automaton to re-interpret the past,
     by recognizing the right-hand side of a production and reducing this
     production. The function [feed] causes the automaton to progress into the
     future by pretending that a (terminal or nonterminal) symbol has been
     read. *)

  (* The function [feed] would ideally be defined here. However, for this
     function to be type-safe, the GADT ['a symbol] is needed. For this
     reason, we move its definition to [InspectionTableInterpreter], where
     the inspection API is available. *)

  (* [pop] pops one stack cell. It cannot go wrong. *)

  let pop (env : 'a env) : 'a env option =
    let cell = env.stack in
    let next = cell.next in
    if next == cell then
      (* The stack is empty. *)
      None
    else
      (* The stack is nonempty. Pop off one cell. *)
      Some { env with stack = next; current = cell.state }

  (* [force_reduction] is analogous to [reduce], except that it does not
     continue by calling [run env] or [initiate env]. Instead, it returns
     [env] to the user. *)

  (* [force_reduction] is dangerous insofar as it executes a semantic action.
     This semantic action could have side effects: nontermination, state,
     exceptions, input/output, etc. *)

  let force_reduction prod (env : 'a env) : 'a env =
    (* Check if this reduction is permitted. This check is REALLY important.
       The stack must have the correct shape: that is, it must be sufficiently
       high, and must contain semantic values of appropriate types, otherwise
       the semantic action will crash and burn. *)
    (* We currently check whether the current state is WILLING to reduce this
       production (i.e., there is a reduction action in the action table row
       associated with this state), whereas it would be more liberal to check
       whether this state is CAPABLE of reducing this production (i.e., the
       stack has an appropriate shape). We currently have no means of
       performing such a check. *)
    if not (T.may_reduce env.current prod) then
      invalid_arg "force_reduction: this reduction is not permitted in this state"
    else begin
      (* We do not expose the start productions to the user, so this cannot be
         a start production. Hence, it has a semantic action. *)
      assert (not (T.is_start prod));
      (* Invoke the semantic action. *)
      let stack = T.semantic_action prod env in
      (* Perform a goto transition. *)
      let current = T.goto_prod stack.state prod in
      { env with stack; current }
    end

  (* The environment manipulation functions -- [pop] and [force_reduction]
     above, plus [feed] -- manipulate the automaton's stack and current state,
     but do not affect the automaton's lookahead symbol. When the function
     [input_needed] is used to go back from an environment to a checkpoint
     (and therefore, resume normal parsing), the lookahead symbol is clobbered
     anyway, since the only action that the user can take is to call [offer].
     So far, so good. One problem, though, is that this call to [offer] may
     well place the automaton in a configuration of a state [s] and a
     lookahead symbol [t] that is normally unreachable. Also, perhaps the
     state [s] is a state where an input symbol normally is never demanded, so
     this [InputNeeded] checkpoint is fishy. There does not seem to be a deep
     problem here, but, when programming an error recovery strategy, one
     should pay some attention to this issue. Ideally, perhaps, one should use
     [input_needed] only in a state [s] where an input symbol is normally
     demanded, that is, a state [s] whose incoming symbol is a terminal symbol
     and which does not have a default reduction on [#]. *)

  let input_needed (env : 'a env) : 'a checkpoint =
    InputNeeded env

  (* The following functions are compositions of [top] and [pop]. *)

  let rec pop_many i env =
    if i = 0 then
      Some env
    else match pop env with
    | None ->
        None
    | Some env ->
        pop_many (i - 1) env

  let get i env =
    match pop_many i env with
    | None ->
        None
    | Some env ->
        top env

end
