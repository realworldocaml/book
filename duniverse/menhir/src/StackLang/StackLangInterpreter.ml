(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let length, map = List.(length, map)
open Lexing
open Printf
open Grammar
open StackLang
open StackLangMeasure

type outcome =
  | Accept
  | Reject of int
  | AbortBySemanticAction

(* -------------------------------------------------------------------------- *)

(* The registers and the stack contain ground values. A ground value can be a
   token (produced by the lexer), a tag, a dummy value, or a tuple of ground
   values. *)

(* We cannot execute actual semantic actions, because they are just text. We
   cannot even hope to build concrete syntax trees (as done in the reference
   interpreter), because an optimized StackLang program does not necessarily
   keep track of all semantic values. Thus, we represent both positions and
   semantic values as dummy (unit) values. This means that a StackLang program
   is interpreted as a recognizer: it either accepts or rejects the input, but
   does not return any information beyond this single bit. *)

type gvalue =
  | GVToken of Terminal.t
  | GVTag of tag
  | GVDummy

type gvalues =
  gvalue list

(* -------------------------------------------------------------------------- *)

(* The runtime environment maps registers to ground values. *)

module Env = Reg.Map

type env = gvalue Env.t

(* -------------------------------------------------------------------------- *)

(* The interpreter's state is as follows. *)

type state = {
  (* How to emit a trace message. *)
  emit : string -> unit;
  (* A lexer that produces terminal symbols instead of actual tokens. *)
  lexer : lexbuf -> Terminal.t;
  (* A lexing buffer, used by the lexer, and out of which positions are read. *)
  lexbuf : lexbuf;
  (* The program. *)
  program : program;
  (* Dynamic instruction counts. *)
  m : measure;
  (* The runtime environment. *)
  mutable env : env;
  (* The stack. Each cell holds a list of values. *)
  mutable stack : gvalues list;
  (* The label of the current block. *)
  mutable label : label;
}

(* -------------------------------------------------------------------------- *)

(* The exception [RuntimeError] is raised when the interpreter encounters an
   unexpected failure at runtime. This indicates that the StackLang program is
   incorrect in some way. *)

(* Checking (prior to execution) that the program is well-formed eliminates a
   category of errors (namely, accesses to undefined registers), but other
   sources of errors remain, such as attempting to pop off an empty stack,
   using a tuple pattern to deconstruct a value that is not a tuple of
   suitable arity, etc. *)

exception RuntimeError of string

let error state format =
  ksprintf (fun s ->
    let msg = sprintf "Inside block %s: %s" (Label.export state.label) s in
    raise (RuntimeError msg)
  ) format

(* -------------------------------------------------------------------------- *)

(* Basic operations on values. *)

let asToken state = function
  | GVToken tok ->
      tok
  | _ ->
      error state "a token was expected"

let asTag state = function
  | GVTag tag ->
      tag
  | _ ->
      error state "a tag was expected"

(* -------------------------------------------------------------------------- *)

(* Evaluating a value [v] yields a ground value. *)

let eval state (v : value) : gvalue =
  match v with
  | VTag tag ->
      GVTag tag
  | VReg r ->
      begin try
        Reg.Map.find r state.env
      with Not_found ->
        error state "undefined register: %s" (Reg.export r)
      end
  | VUnit ->
      GVDummy

(* -------------------------------------------------------------------------- *)

(* Matching a ground value [gv] against a pattern [p] extends the environment
   with new bindings. *)

let bind1 p gv (env : env) : env =
  match p with
  | PWildcard ->
      env
  | PReg r ->
      Env.add r gv env

let bind state ps gvs =
  (* Assuming that no name is bound twice, we can perform a sequence
     of assignments. *)
  if length ps = length gvs then
    state.env <- List.fold_right2 bind1 ps gvs state.env
  else
    error state "%d patterns cannot match %d values"
      (length ps) (length gvs)

let eval_bind state bs =
  (* All values in the codomain of [bs] are evaluated in [state.env].
     The accumulator, which has type [env], contains [state.env] and is
     extended with the new bindings. *)
  (* Once we are done, we update [state.env]. *)
  state.env <- Bindings.fold (fun r v accu ->
    bind1 (PReg r) (eval state v) accu
  ) bs state.env

(* -------------------------------------------------------------------------- *)

(* Printing a trace message. *)

let print state format =
  ksprintf state.emit format

(* -------------------------------------------------------------------------- *)

(* Executing a primitive operation. *)

exception Abort

let exec_prim state prim =
  match prim with
  | PrimLexerCall _ ->
      (* We do not look up the registers [lexer] and [lexbuf], so it is okay
         if they are not defined. *)
      let tok = state.lexer state.lexbuf in
      print state
        "Lookahead token is now %s (%d-%d)\n"
        (Terminal.print tok)
        state.lexbuf.lex_start_p.pos_cnum
        state.lexbuf.lex_curr_p.pos_cnum;
      GVToken tok
  | PrimOCamlFieldAccess (_, _) ->
      (* ASSUMPTION. *)
      (* We assume that this is an access to a position field in [lexbuf]. *)
      (* A position is replaced with a dummy value. *)
      GVDummy
  | PrimOCamlAction (_bs, prod, _action) ->
      (* Because the bindings have local scope, they affect just the semantic
         action, which we do not execute; so, they can be ignored. *)
      (* As a special case, under the simplified strategy, if the production
         [prod] involves the [error] token, then we assume that the semantic
         action aborts the parser. The reference interpreter behaves in this
         way, too. *)
      if Settings.strategy = `Simplified
      && not (Production.error_free prod) then
        raise Abort;
      (* A semantic value is replaced with a dummy value. *)
      GVDummy

(* -------------------------------------------------------------------------- *)

(* Popping a cell (holding a list of ground values) off the stack. *)

let pop state : gvalues =
  match state.stack with
  | gvs :: stack ->
      state.stack <- stack;
      gvs
  | [] ->
      error state "attempt to pop something off an empty stack"

let peek state : gvalues =
  match state.stack with
  | gvs :: _stack ->
      gvs
  | [] ->
      error state "attempt to peek at an empty stack"

(* -------------------------------------------------------------------------- *)

(* Executing a block. *)

let rec exec state block =
  let m = state.m in
  match block with

  | IPush (vs, _, block) ->
      let gvs = map (eval state) vs in
      state.stack <- gvs :: state.stack;
      m.push <- m.push + 1;
      exec state block

  | IPop (ps, _, block) ->
      let gvs = pop state in
      bind state ps gvs;
      m.pop <- m.pop + 1;
      exec state block

  | IPeek (ps, _, block) ->
      let gvs = peek state in
      bind state ps gvs;
      m.peek <- m.peek + 1;
      exec state block

  | IDef (bindings, block) ->
      eval_bind state bindings;
      m.def <- m.def + 1;
      exec state block

  | IPrim (p, prim, block) ->
      begin match exec_prim state prim with
      | gv ->
          bind state [p] [gv];
          m.prim <- m.prim + 1;
          exec state block
      | exception Abort ->
          AbortBySemanticAction
      end

  | ITrace (trace, block) ->
      print state "%s\n" trace;
      m.trace <- m.trace + 1;
      exec state block

  | IComment (_, block) ->
      exec state block

  | IDead _ ->
      (* This is problematic! This point in the code should not be reachable. *)
      assert false

  | IStop s ->
      m.stop <- m.stop + 1;
      Reject s

  | IReturn (_, v) ->
      let (_gv : gvalue) = eval state v in
      m.return <- m.return + 1;
      Accept

  | IJump label ->
      state.label <- label;
      m.jump <- m.jump + 1;
      exec_tblock state (lookup state.program label)

  | ICaseToken (r, branches, odefault) ->
      let tok = asToken state (eval state (VReg r)) in
      m.casetoken <- m.casetoken + 1;
      exec_casetoken state tok branches odefault

  | ICaseTag (r, branches) ->
      let tag = asTag state (eval state (VReg r)) in
      m.casetag <- m.casetag + 1;
      exec_casetag state tag branches

and exec_casetoken state tok branches odefault =
  match (branches, odefault) with
  | (TokSingle (tok', r), block) :: _, _ when Terminal.equal tok tok' ->
      bind state [PReg r] [GVDummy];
      exec state block
  | (TokMultiple toks, block) :: _, _ when TerminalSet.mem tok toks ->
      exec state block
  | _ :: branches, _ ->
      exec_casetoken state tok branches odefault
  | [], Some block ->
      exec state block
  | [], None ->
      error state "nonexhaustive case analysis on a token (%s)" (Terminal.print tok)

and exec_casetag state tag branches =
  let block =
    try
      Block.select_branch tag branches
    with Not_found ->
      error state "nonexhaustive case analysis on a tag (%s)" (Tag.print tag)
  in
  exec state block

and exec_tblock state { block; needed } =
  (* Before executing this block, we check that all its needed registers
     are indeed available. This is an important sanity check, which could
     help us debug code production via testing. *)
  let available = Env.domain state.env in
  if not (Reg.Set.subset needed available) then
    error state "incorrect [needed] annotation; have %s, need %s"
      (Reg.Set.print available)
      (Reg.Set.print needed);
  state.env <- Env.restrict needed state.env;
  exec state block

(* -------------------------------------------------------------------------- *)

(* The interpretation of a program begins with an environment in which
   certain dummy bindings are made (in order to avoid runtime failures
   at NEED instructions) and with an empty stack. *)

let interpret m program label emit lexer lexbuf =
  let env =
    List.fold_right
      (fun r env -> Env.add r GVDummy env)
      EmitStackLang.required
      Env.empty
  and stack = [] in
  let state = { emit; lexer; lexbuf; program; m; env; stack; label } in
  exec state (IJump label)
