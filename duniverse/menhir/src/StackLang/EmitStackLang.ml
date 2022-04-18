(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open Grammar
open Invariant
let prefix = CodeBits.prefix
open StackLang
open StackLangBuilder

(* -------------------------------------------------------------------------- *)

(* The following auxiliary functions detect which states can reduce a unit
   production. *)

let is_unit prod =
  Production.length prod = 1

let _may_perform_unit_reduction s =
  match Default.has_default_reduction s with
  | Some (prod, _) ->
      is_unit prod
  | None ->
      TerminalMap.fold (fun tok prods accu ->
        accu || tok <> Terminal.error && List.exists is_unit prods
      ) (Lr1.reductions s) false

let has_default_unit_reduction s =
  match Default.has_default_reduction s with
  | Some (prod, _) ->
      is_unit prod
  | None ->
      false

(* -------------------------------------------------------------------------- *)

(* Auxiliary functions for code generation. *)

let vreg (r : register) =
  VReg r

let vregs (rs : register list) =
  List.map vreg rs

let push (rs : register list) =
  push (vregs rs)

(* -------------------------------------------------------------------------- *)

(* Auxiliary functions for constructing block types. *)

(* [origin_to_final] converts an origin, as defined by the module Invariant,
   to a final, as defined by StackLang. In short, if a block is reachable
   from a single start symbol [nt], then its final is [Some nt]; otherwise
   it is [None], which means that this block is polymorphic. *)

let origin_to_final origin =
  match origin with
  | Origin.SingleOrigin nt ->
      Some nt
  | Origin.Dead
  | Origin.MultipleOrigins ->
      None

(* [make_block_type] constructs a block type. In the stack shape, we keep
   only the cells where at least one the four fields is present; in other
   words, absent cells are removed. This is consistent with the fact that
   [push] and [pop] generate no code when applied to a list of zero
   components. *)

let make_block_type stack origin =
  let stack = filter present stack
  and final = origin_to_final origin in
  { stack; final }

(* [set_block_type] combines [make_block_type] and [set_block_type] (which
   is defined in [StackLangBuilder]) in a convenient way. *)

let set_block_type stack origin =
  let block_type = make_block_type stack origin in
  set_block_type block_type

(* ------------------------------------------------------------------------ *)

(* An auxiliary function to describe a stack cell. *)

(* [components cell state semv startp endp] is a sublist of the list
   [[state; semv; startp; endp]], where only the relevant fields are
   retained, as dictated by [cell]. *)

let cons flag x xs =
  if flag then x :: xs else xs

let components cell state semv startp endp =
  cons (holds_state cell) state (
  cons (holds_semv cell) semv (
  cons (holds_startp cell) startp (
  cons (holds_endp cell) endp [])))

(* -------------------------------------------------------------------------- *)

(* [log] emits an [ITrace] instruction. *)

(* We are careful to ensure that every back-end produces exactly the same
   trace; this can be very helpful while debugging Menhir. *)

let log format =
  Printf.ksprintf trace format

(* -------------------------------------------------------------------------- *)

(* For each routine, we distinguish two modes: normal mode and error-handling
   mode, where a syntax error has been detected and is being handled. The
   Boolean parameter [mode] indicates which mode we are in. In order to
   produce more efficient and more readable code, we perform compile-time
   specialization: that is, we make [mode] a compile-time parameter, and
   generate two specialized versions of each routine: one version for normal
   mode and one version for error-handling mode. *)

type mode =
  | NormalMode
  | ErrorHandlingMode

(* -------------------------------------------------------------------------- *)

(* [must_read_positions_upon_entering s] determines whether the start and end
   positions of the current token should be read from [lexbuf] upon entering
   the state [s]. This is the case if and only if [s] is entered via a shift
   transition. (Otherwise, [startp] and [endp] are already defined.) We need
   them in that case because [run] pushes a new cell when entered through a
   shift transition. *)

let must_read_positions_upon_entering s =
  match Lr1.incoming_symbol s with
  | None
  | Some (Symbol.N _) ->
      false
  | Some (Symbol.T _) ->
      true

(* -------------------------------------------------------------------------- *)

(* [must_query_lexer_upon_entering s] determines whether the lexer must be
   queried for the next token upon entering the state [s]. *)

let must_query_lexer_upon_entering mode s =
  (* The lexer is queried only in normal mode. In error-handling mode,
     in the simplified strategy, it is never queried. *)
  mode = NormalMode &&
  match Lr1.incoming_symbol s with
  | Some (Symbol.N _) ->
      (* The state [s] is entered via a goto transition. No token must
         be consumed; the current token must be kept. *)
      false
  | None
  | Some (Symbol.T _) ->
      (* The state [s] either is an initial state or is entered via a shift
         transition. *)
      match Default.has_default_reduction s with
      | Some (_, toks)
        when TerminalSet.mem Terminal.sharp toks ->
          assert (TerminalSet.cardinal toks = 1);
          (* The state [s] has a default reduction on [#]. The next token must
             not be requested, as that would drive the lexer beyond the end of
             the input stream. *)
          false
      | _ ->
          (* The state [s] has either no default reduction or a default
             reduction on a set of tokens that does not contain [#].
             The lexer must be queried for the next token. *)
          true

(* -------------------------------------------------------------------------- *)

(* When a "goto" transition (a transition labeled with a nonterminal symbol)
  is taken, a new cell must be pushed onto the stack. Several possible
   placements come to mind for the PUSH instruction:

   - One convention, "goto pushes", is to place this instruction in the [goto]
     subroutine, prior to the case analysis whose branches contain jumps
     to [run] subroutines.

   - Another convention, "run pushes", is to place this instruction at the
     beginning of [run].

   The first convention can be interesting if one wishes to optimize for code
   size. However, we prefer to keep the complexity low and avoid introducing
   a choice between these two conventions. Thus, we always use "run pushes",
   which is the preferred choice in a setting where one wishes to allow PUSH
   instructions to travel forward, meet POP instructions, and cancel out. *)

(* In "run pushes", every [run] function pushes a new cell onto the stack,
   except those that are associated with an initial state. *)

let runpushes s =
  Lr1.incoming_symbol s <> None

(* -------------------------------------------------------------------------- *)

(* Conventional names for registers and auxiliary functions. *)

(* It is extremely difficult to predict or document where each register is
   live (that is, potentially needed in the future). We abandon any hope of
   doing so. Instead, we run a global liveness analysis (implemented in the
   module NeededRegisters and invoked by StackLangBuilder). *)

(* The registers [lexer] and [lexbuf] hold the lexer and the lexing buffer
   throughout. They are the only two registers that are allowed to be live
   at a public entry point. In other words, in the OCaml code that is
   eventually generated, they are the only two parameters that are taken. *)

let lexer, lexbuf =
  "lexer" |> prefix |> Reg.import,
  "lexbuf" |> prefix |> Reg.import

let required =
  [ lexer; lexbuf ]

(* The [token] register holds the most recently read token. *)

(* The [state] register holds the current state of the parser. *)

(* The register [semv] holds the semantic value that is produced by a
   semantic action. *)

(* The registers [startp] and [endp] hold the start and end positions
   that are produced by a semantic action. *)

(* The registers [beforeendp], [startpos ids i], and [endpos ids i] hold
   positions that are needed by a semantic action. *)

(* The register [initp] holds the initial position of the lexer. It is
   read from the lexing buffer before the lexer is ever invoked. *)

let token, state, semv, beforeendp, startp, endp =
  CodePieces.(
    token |> Reg.import,
    state |> Reg.import,
    semv |> Reg.import,
    beforeendp |> Reg.import,
    startp |> Reg.import,
    endp |> Reg.import
  )

let startpos ids i =
  CodePieces.(startpos ids i |> Reg.import)

let endpos ids i =
  CodePieces.(endpos ids i |> Reg.import)

let initp =
  "initp" |> prefix |> Reg.import

(* -------------------------------------------------------------------------- *)

(* Code labels and code generation. *)

(* A code label is potentially the target of [jump] instructions. *)

(* We need four kinds of code labels. The most common three are [run] (one
   such label per state), [reduce] (one per production), and [goto] (one per
   nonterminal symbol). On rare occasions, we also need [act_goto]: there is
   one such label per production, but this applies only to non-start
   non-epsilon productions that use [$endpos($0)]. *)

(* Making [A] a functor allows computing the long invariant only if needed. *)

module A () = struct

type address =
  | Run of mode * Lr1.node
  | Reduce of mode * Production.index
  | Goto of mode * Nonterminal.t
  | ActGoto of mode * Production.index

let mode_prefix mode name =
  match mode with
  | NormalMode ->
      name
  | ErrorHandlingMode ->
      "error_" ^ name

let print addr =
  Label.import (prefix (
    match addr with
    | Run (mode, s) ->
        let s = Misc.padded_index Lr1.n (Lr1.number s) in
        sprintf "run_%s" s
        |> mode_prefix mode
    | Reduce (mode, prod) ->
        let prod = Misc.padded_index Production.n (Production.p2i prod) in
        sprintf "reduce_%s" prod
        |> mode_prefix mode
    | Goto (mode, nt) ->
        let nt = Nonterminal.print true nt in
        sprintf "goto_%s" nt
        |> mode_prefix mode
    | ActGoto (mode, prod) ->
        let prod = Misc.padded_index Production.n (Production.p2i prod) in
        sprintf "act_goto_%s" prod
        |> mode_prefix mode
  ))

let jump addr =
  jump (print addr)

let iter mode yield =
  Lr1.iter (fun s -> yield (Run (mode, s)));
  Production.iter (fun prod -> yield (Reduce (mode, prod)));
  Nonterminal.iterx (fun nt -> yield (Goto (mode, nt)));
  Production.iterx (fun prod -> yield (ActGoto (mode, prod)))

let iter yield =
  iter NormalMode yield;
  iter ErrorHandlingMode yield

(* -------------------------------------------------------------------------- *)

(* Code for the [run] subroutine associated with mode [mode] and state [s]. *)

let rec run mode s =

  (* Optionally, mark this [run] routine as a candidate for specialization in
     a situation where the current state (a predecessor of [s]) is known. This
     is potentially desirable if the state [s] can reduce a unit production,
     because then we can inline [reduce] and inline [goto], where the CASEtag
     instruction disappears.

     It is debatable whether we should do this as soon as [s] can reduce a
     unit production (for some value of the lookahead token) or only if [s]
     has a default reduction of a unit production. The latter criterion is
     more conservative and refuses to specialize a [run] routine that contains
     a CASEtok instruction, thus avoiding a potentially large amount of code
     duplication. We choose this cautious approach. *)

  if mode = NormalMode && has_default_unit_reduction s then
    set_spec SpecAllowed;

  (* Determine whether the start and end positions of the current token
     should be read from [lexbuf]. *)

  let must_read_positions = must_read_positions_upon_entering s in

  (* Determine whether a new cell must be pushed onto the stack. *)

  let must_push = runpushes s in

  (* A sanity check: [must_read_positions] implies [must_push]. Indeed,
     the sole reason why we read these positions is that we must push
     a new cell. *)

  (* The reverse implication is not true. If this state is entered via
     a goto transition, then [must_push] may be true or false, whereas
     [must_read_positions] is false. *)

  assert (not must_read_positions || must_push);

  (* Determine whether the lexer should be queried for the next token. *)

  let must_query_lexer = must_query_lexer_upon_entering mode s in

  (* Log that we are entering state [s]. *)

  log "State %d:" (Lr1.number s);

  (* If necessary, read the positions of the current token from [lexbuf]. *)

  if must_read_positions then begin
    prim startp (PrimOCamlFieldAccess (VReg lexbuf, "Lexing.lex_start_p"));
    prim endp (PrimOCamlFieldAccess (VReg lexbuf, "Lexing.lex_curr_p"))
  end;

  (* Note that [state] does not contain the state [s]; instead, it contains a
     predecessor state. *)

  (* If [run] is expected to push a new cell onto the stack, do so now. *)

  if must_push then begin
    let cell = top (Short.stack s) in
    if present cell then
      push (components cell state semv startp endp) cell
  end;

  (* Define the current state to be [s]. Do so only if the state [s] is
     represented; otherwise, the tag [s] does not even exist. *)

  if represented s then
    def [PReg state] [VTag (Tag.make s)];

  (* If this is a start state, define [endp] and [initp]. These registers may
     be later read if this state can reduce a production whose semantic action
     uses the keyword [$endpos($0)]. (This includes epsilon productions that
     mention [$startpos] or [$endpos]; see KeywordExpansion.) We initialize
     these registers with the lexer's current position at the beginning of the
     parsing process, which we read before the lexer is ever queried.

     [endp] may be read in the future if this state can reduce an epsilon
     production that needs [$endpos($0)]. [initp] may be read in the future if
     this state can reduce a non-epsilon production that needs [$endpos($0)].

     If [initp] register is never needed anywhere, then it will be considered
     dead and this initialization instruction will be removed. If it is needed
     somewhere, then it will be carried all along, so many routines may end up
     needing [initp]. *)

  if Lr1.is_start s then begin
    prim endp (PrimOCamlFieldAccess (VReg lexbuf, "Lexing.lex_curr_p"));
    move initp endp
  end;

  (* If necessary, query the lexer for the next token, and rebind [token].
     This is done by calling [discard], a global function that is defined
     directly in IL code, outside StackLang. *)

  if must_query_lexer then
    prim token (PrimLexerCall [ VReg lexer; VReg lexbuf ]);

  (* If the state [s] has a default reduction of production [prod], then jump
     to the subroutine that reduces this production. *)

  (* If the state [s] has no default reduction, then (in normal mode) perform
     a case analysis of the current token [token]. The branches involve
     (shift) transitions, reductions, and (possibly) a default branch that
     triggers an error. *)

  (* If the state [s] has no default reduction, then (in error-handling mode)
     perform a similar case analysis, while pretending that the current token
     is the [error] token. *)

  match Default.has_default_reduction s, mode with
  | Some (prod, _), _ ->
      jump (Reduce (mode, prod))
  | None, NormalMode ->
      run_normal_dispatch s
  | None, ErrorHandlingMode ->
      (* The old code-back end claims that this point in the code can be
         reached only if the state [s] satisfies [errorpeeker s], that is,
         only if [s] is the target of a reduction on [error]. Here, this may
         or may not be true. (One must keep in mind that the old code back-end
         uses the legacy strategy, whereas we use the simplified strategy,
         which is somewhat different.) I prefer not to take chances and to
         generate this code anyway. *)
      log "Resuming error handling";
      run_error_dispatch s

(* -------------------------------------------------------------------------- *)

(* The continuation of the [run] subroutine, in normal mode. *)

(* A dispatch on the current token. *)

and run_normal_dispatch s =

  let mode = NormalMode in

  case_token token begin fun branch default ->

    (* Transitions. *)

    Lr1.transitions s
    |> SymbolMap.remove (Symbol.T Terminal.error)
    |> SymbolMap.iter begin fun symbol s' ->
      match symbol with
      | Symbol.T tok ->
          (* A transition of [s] along [tok] to [s']. *)
          assert (not (Terminal.pseudo tok));
          (* Use a pattern that recognizes the token [tok] and binds [semv] to
             its semantic value. *)
          branch (TokSingle (tok, semv)) begin fun () ->
            (* Log that we are shifting. *)
            log "Shifting (%s) to state %d"
              (Terminal.print tok) (Lr1.number s');
            (* Invoke [run s']. *)
            jump (Run (mode, s'))
          end
      | Symbol.N _ ->
          ()
    end;

    (* Reductions. *)

    Lr1.reductions s
    |> TerminalMap.remove Terminal.error
    |> Lr0.invert
    |> ProductionMap.iter begin fun prod toks ->
      (* A reduction of [prod] on every token in the set [toks]. *)
      branch (TokMultiple toks) begin fun () ->
        jump (Reduce (mode, prod))
      end
    end;

    (* A default branch, where we switch to error-handling mode. *)

    (* If the state [s] is able to take a shift or reduce action upon error,
       then [run_error_dispatch s] boils down to [stop()]. In particular, if
       the grammar does not use the [error] token at all, then we produce no
       error-handling code at all. *)

    default begin fun () ->
      log "Initiating error handling";
      run_error_dispatch s
    end

  end

(* -------------------------------------------------------------------------- *)

(* The continuation of the [run] subroutine, in error-handling mode. *)

(* When [--trace] is off, [log] generates no code, so [run_error_dispatch s]
   generates a single [jump] instruction. *)

and run_error_dispatch s =

  let tok = Terminal.error
  and mode = ErrorHandlingMode in

  match SymbolMap.find (Symbol.T tok) (Lr1.transitions s) with
  | s' ->

      (* There is a shift transition on error. Bind [semv] to a unit value,
         because the [error] token has type unit. Invoke [run s'], still in
          error-handling mode. *)

      log "Handling error in state %d" (Lr1.number s);
      log "Shifting (error) to state %d" (Lr1.number s');
      def [PReg semv] [VUnit];
      jump (Run (mode, s'))

  | exception Not_found ->

  match TerminalMap.lookup tok (Lr1.reductions s) with
  | prods ->
      let prod = Misc.single prods in

      (* There is a reduce transition on error. Invoke [reduce prod], still in
         error-handling mode. Note that [reduce] can invoke [goto] which can
         invoke another [run] function, still in error-handling mode. *)

      log "Handling error in state %d" (Lr1.number s);
      jump (Reduce (mode, prod))

  | exception Not_found ->

      (* This state is unable to handle errors. Stop. *)

      stop (Lr1.number s)

(* -------------------------------------------------------------------------- *)

(* Code for executing the semantic action associated with production [prod]. *)

(* This code is part of the [reduce] routine for production [prod]. *)

let act prod =

  assert (not (Production.is_start prod));
  let nt = Production.nt prod
  and n = Production.length prod
  and ids = Production.identifiers prod
  and action = Production.action prod in

  let symbol = Symbol.N nt
  and is_epsilon = (n = 0)
  and stack = Short.prodstack prod in
  assert (length stack = n);

  (* Log that we are reducing production [prod]. *)

  log "Reducing production %s" (Production.print prod);

  (* The semantic action does not need the registers [startp] and [endp].
     Indeed, the keywords [$startpos] and [$endpos] are translated away
     using other keywords, typically [$startpos($i)] , [$endpos($i)], and
     [$endpos($0)], also known as [beforeendp]. See KeywordExpansion. *)

  assert (
    let vars = Reg.Set.import (Action.posvars action) in
    not (Reg.Set.mem startp vars || Reg.Set.mem endp vars)
  );

  (* Execute the semantic action. Store its result in [semv]. *)

  prim semv (PrimOCamlAction (Bindings.empty, prod, action));

  (* If [prod] is not an epsilon production, then the following implications
     hold: if [nt] keeps track of its start position, then the stack cell
     [stack.(0)] holds a start position; if [nt] keeps track of its end
     position, then the stack cell [stack.(n-1)] holds an end position. *)

  if not is_epsilon then begin
    assert (track_startp symbol <= holds_startp (get stack  0     ));
    assert (track_endp   symbol <= holds_endp   (get stack (n - 1)))
  end;

  (* If the symbol [nt] keeps track of its start and end positions, define
     [startp] and [endp], which are needed by [goto]. *)

  if track_startp symbol then
    move startp (if is_epsilon then endp else startpos ids 0);
  if track_endp   symbol then
    move endp   (if is_epsilon then endp else endpos ids (n - 1))

(* -------------------------------------------------------------------------- *)

(* Code for the [reduce] subroutine associated with mode [mode] and production
   [prod]. *)

let reduce mode prod =

  (* The array [ids] lists the identifiers that are bound by this production.
     These identifiers can be referred to by the semantic action. *)

  let ids = Production.identifiers prod
  and n = Production.length prod in
  assert (Array.length ids = n);
  let is_epsilon = (n = 0) in
  let id i = Reg.import ids.(i) in


  (* Pop [n] stack cells and store their content in suitable registers.
     The state stored in the bottom cell (the one that is popped last)
     is stored in the register [state] and thus becomes the new current
     state. *)

  let stack = Short.prodstack prod in
  assert (length stack = n);

  let pops = ref 0 in
  for i = n - 1 downto 0 do
    let cell = get stack i in
    if present cell then begin
      incr pops;
      pop (
        components cell
          (if i = 0 then PReg state else PWildcard)
          (PReg (id i))
          (PReg (startpos ids i))
          (PReg (endpos ids i))
      ) cell
    end;
    (* If this stack cell does not hold a semantic value, then the semantic
       values has type [unit] and and we must define it. *)
    if not (holds_semv cell) then
      def [PReg (id i)] [VUnit]
  done;

  set_hint (IfCells !pops);

  (* If this is a start production, then reducing this production means
     accepting. This is done via a [return] instruction, which ends the
     execution of the program. *)

  if Production.is_start prod then begin
    assert (n = 1);
    log "Accepting";
    let nt = Option.force (Production.classify prod) in
    return nt (VReg (id 0))
  end
  else

  (* If this is not a start production, then it has a semantic action. *)

  let action = Production.action prod in
  let has_beforeend = Action.has_beforeend action in

  (* In the most common case, the semantic action does not use [$endpos($0)],
     also known as [beforeendp]. In that case, we execute the semantic action
     and jump to the [goto] routine. *)

  if not has_beforeend then begin
    act prod;
    jump (Goto (mode, Production.nt prod))
  end

  (* If the semantic action does use [$endpos($0)], and if this is an epsilon
     production, then we are in luck: the register [endp] must contain the end
     position of the top stack cell (because this cell has just been pushed
     onto the stack), so we do not need to peek at this stack cell. This works
     also if we are in initial state: then, there is no top stack cell, but
     the register [endp] nevertheless contains the desired position. *)

  else if is_epsilon then begin
    assert has_beforeend;
    comment "Defining $endpos($0) in the case of an epsilon production";
    move beforeendp endp;
    act prod;
    jump (Goto (mode, Production.nt prod))
  end

  (* If the semantic action uses [$endpos($0)] and if this is not an
     epsilon production, then we must peek at the top stack cell.
     We do so by jumping to a routine that combines [act] and [goto]. *)

  else begin
    assert has_beforeend;
    assert (not is_epsilon);
    jump (ActGoto (mode, prod))
  end

(* -------------------------------------------------------------------------- *)

(* Code for the [goto] subroutine associated with mode [mode] and nonterminal
   symbol [nt]. *)

(* We perform a case analysis on the current state [state]. In each branch, we
   jump to an appropriate new state. There is no default branch. Although a
   default branch may be added at the level of OCaml in order to avoid a
   warning from the OCaml compiler, this default branch is dead. *)

(* We use [case_tag], a smart constructor, which removes the case analysis if
   it has only one branch. The fact that we can do this without endangering
   well-typedness is nontrivial: for the case analysis to be redundant, it
   must be the case that no type information is gained through it. (Remember,
   in OCaml, this is a [match] on a GADT). This is true because we use the
   long invariant. *)

let goto mode nt =

  let has_case_tag =
    case_tag state begin fun branch ->
      Lr1.targets begin fun () sources target ->
        (* If the current state is a member of [sources], jump to [target]. *)
        branch
          (List.map Tag.lazy_make sources)
          (fun () -> jump (Run (mode, target)))
      end () (Symbol.N nt)
    end
  in

  (* If we have generated a [casetag] instruction, then this function must
     *not* be inlined, unless the [casetag] instruction can be eliminated on
     the fly. If we haven't generated a [casetag] instruction, then it is
     desirable to always inline this function. *)

  set_hint (if has_case_tag then OnlyIfKnownState else Always)

(* -------------------------------------------------------------------------- *)

(* Code for a routine that combines the semantic action for production [prod]
   and the [goto] function that follows. *)

(* When this routine is invoked by [reduce], the cells that correspond to the
   right-hand side of the production have already been popped off the stack. *)

let rec act_goto mode prod =

  (* [prod] is not a start production. *)
  assert (not (Production.is_start prod));
  (* [prod] is not an epsilon production. *)
  let n = Production.length prod in
  assert (n > 0);
  (* The semantic action needs [$endpos($0)]. *)
  let action = Production.action prod in
  assert (Action.has_beforeend action);

  let nt = Production.nt prod in
  let symbol = Symbol.N nt in

  (* We must assign a correct value to the register [beforeendp] before we
     execute the semantic action. To do so, a case analysis on the current
     state is required. [Invariant] imposes that this state be represented,
     so this case analysis is possible. *)

  let has_case_tag =
    case_tag state begin fun branch ->
      Lr1.targets begin fun () sources target ->
        sources |> List.iter begin fun source ->
          (* Generate one branch per edge of [source] to [target]. *)
          branch [Tag.lazy_make source] begin fun () ->
            (* Assign [beforeendp]; execute the semantic action; jump to
               the [run] routine for the state [target]. *)
            assign_beforeendp source;
            act prod;
            jump (Run (mode, target))
          end
        end
      end () symbol
    end
  in
  set_hint (if has_case_tag then OnlyIfKnownState else Always)

(* [assign_beforeendp] assigns the register [beforeendp] in a suitable way,
   under the assumption that the current state is [source]. *)

(* If the current state is an initial state, then we want to define
   [beforeendp] as [initp], the initial position. (We cannot peek at the top
   stack cell, because there is none. We could set things up so that there
   is a sentinel cell below the stack, but that would complicate the
   treatment of the type of the stack everywhere.) *)

(* If the current state is not an initial state, then it has an incoming
   symbol, which must keep track of its end position; the module [Invariant]
   imposes this requirement. Therefore, the stack must be nonempty, and the
   top stack cell must contain an end position; this is the position that we
   wish to extract. Peeking at the top stack cell requires knowing that this
   cell exists, and knowing its shape; this knowledge is available because
   the current state is statically known. *)

and assign_beforeendp source =
  if Lr1.is_start source then begin
    comment "Defining $endpos($0) in an initial state";
    move beforeendp initp
  end
  else begin
    comment "Defining $endpos($0) by peeking at the top stack cell";
    let stack = Short.stack source in
    assert (length stack > 0);
    let cell = top stack in
    assert (present cell);
    let wild = PWildcard in
    peek (components cell wild wild wild (PReg beforeendp)) cell
  end

(* -------------------------------------------------------------------------- *)

(* Code for all subroutines. *)

(* This is where we declare the type of each subroutine. *)

(* The type information that we produce is based on the long invariant, which
   we compute here. Because we are inside the functor [L()], this computation
   is performed only if necessary. *)

module Long =
  Invariant.Long()

let code label =
  (* A potential confusion exists between the [pop] functions provided by the
     modules [Invariant] and [StackLangBuilder]. *)
  let pop = Invariant.pop in
  match label with

  | Run (mode, s) ->
      (* If [run] does not push a new stack cell, then the shape of the stack
         at the beginning of [run] is given by [Long.stack]. If [run] pushes a
         new cell, then we must pop one cell off this shape. *)
      let stack = Long.stack s in
      let stack = if runpushes s then pop stack else stack in
      set_block_type stack (Origin.run s);
      run mode s

  | Reduce (mode, prod) ->
      let stack = Long.prodstack prod in
      set_block_type stack (Origin.reduce prod);
      reduce mode prod

  | Goto (mode, nt) ->
      (* The function [Long.gotostack] provides the shape of the stack
         *after* the goto transition has been taken. Here, we want the shape
         of the stack when the [goto] function is invoked, that is, *before*
         the goto transition is taken. This is why we pop one cell. *)
      let stack = pop (Long.gotostack nt) in
      set_block_type stack (Origin.goto nt);
      goto mode nt

  | ActGoto (mode, prod) ->
      (* Same type as in the previous case. *)
      let nt = Production.nt prod in
      let stack = pop (Long.gotostack nt) in
      set_block_type stack (Origin.goto nt);
      act_goto mode prod

(* -------------------------------------------------------------------------- *)

(* The entry points. *)

let entry =
  let mode = NormalMode in
  ProductionMap.fold (fun _prod s accu ->
      let nt = Item.startnt (Lr1.start2item s) in
      let name = Nonterminal.print true nt in
      StringMap.add name (Run (mode, s)) accu
  ) Lr1.entry StringMap.empty

(* -------------------------------------------------------------------------- *)

(* A map of the represented states to their types. *)

let states =
  Lr1.fold (fun states s ->
    if represented s then
      let bty = make_block_type (Long.stack s) (Origin.run s) in
      Tag.Map.add (Tag.make s) bty states
    else
      states
  ) Tag.Map.empty

end (* L *)

(* -------------------------------------------------------------------------- *)

(* Build a StackLang program. *)

module Run () = struct

  (* The program. *)
  include Build (A())

end
