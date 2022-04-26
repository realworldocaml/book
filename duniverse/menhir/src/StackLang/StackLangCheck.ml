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
open StackLang
let state = EmitStackLang.state

(* -------------------------------------------------------------------------- *)

(* Error reporting machinery. *)

(* Every error that is reported here is an internal error.
   These errors are supposed to occur in production. *)

type error = {
  (* The program that is being checked. *)
  error_program : program;
  (* The label of the block that is being checked. *)
  error_label : label;
  (* The instruction that is being checked. *)
  error_pc : block;
  (* The message that must be displayed. *)
  error_message : string;
}

exception Error of error

let fail error_program error_label error_pc error_message =
  let error = { error_program; error_label; error_pc; error_message } in
  raise (Error error)

let handle f x =
  try
    f x
  with Error error ->
    eprintf "StackLangCheck: internal error.\n";
    eprintf "%s" error.error_message;
    eprintf "\nInside %s, at this instruction:\n"
      (Label.export error.error_label);
    StackLangPrinter.ToChannel.instruction stderr error.error_pc;
    if true then begin
      eprintf "\n\nThis is the complete block:\n";
      StackLangPrinter.ToChannel.typed_block stderr
        (lookup error.error_program error.error_label)
    end;
    if true then begin
      eprintf "\n\nStates:\n";
      StackLangPrinter.ToChannel.states stderr error.error_program.states
    end;
    exit 1

(* -------------------------------------------------------------------------- *)

(* A StackLang program is well-formed if it contains no references to
   undefined registers or tags. *)

type wf_env = {
  (* The program that is being checked. *)
  program : program;
  (* The label of the block that is being checked. *)
  label : label;
  (* The instruction that is being checked. *)
  pc : block;
  (* The set of registers that are currently defined. *)
  registers : registers;
}

open Reg.Set

let wf_fail env format =
  Printf.ksprintf (fail env.program env.label env.pc) format

let wf_regs env required =
  (* Check that [required] is a subset of [env.registers]. *)
  let stray = diff required env.registers in
  if not (is_empty stray) then
    wf_fail env
      "Undefined register%s:\n\
      \  %s\n\
       The following registers are defined:\n\
      \  %s\n"
      (if cardinal stray > 1 then "s" else "")
      (print stray)
      (print env.registers)

let wf_reg env required =
  wf_regs env (singleton required)

let wf_tag env tag =
  if not (Tag.Map.mem tag env.program.states) then
    wf_fail env "Reference to an unknown tag: %s\n"
      (Tag.print tag)

let wf_tagpat env (TagSingle tag) =
  wf_tag env tag

let wf_value env v =
  match v with
  | VTag tag ->
      wf_tag env tag
  | VReg r ->
      wf_reg env r
  | VUnit ->
      ()

let wf_values env vs =
  List.iter (wf_value env) vs

let def rs p =
  match p with
  | PWildcard ->
      rs
  | PReg r ->
      (* Check that no name is bound twice by a pattern. *)
      assert (not (mem r rs));
      add r rs

let defs rs ps =
  List.fold_left def rs ps

let defs env ps =
  (* The newly defined registers are the previously defined registers
     plus the registers defined by the patterns [ps]. *)
  { env with registers = union env.registers (defs empty ps) }

let wf_bindings env bs =
  (* Check the codomain. *)
  Bindings.iter (fun _r v -> wf_value env v) bs;
  (* Extend the set [rs] with the domain. *)
  { env with registers = union env.registers (Bindings.domain bs) }

let wf_prim env p =
  match p with
  | PrimLexerCall vs ->
      List.iter (wf_value env) vs
  | PrimOCamlFieldAccess (v, _) ->
      wf_value env v
  | PrimOCamlAction (bs, _prod, action) ->
      let env = wf_bindings env bs in
      let vars = import (Action.vars action) in
      wf_regs env vars

let def_tokpat env tokpat =
  match tokpat with
  | TokSingle (_, r) ->
      defs env [PReg r]
  | TokMultiple _ ->
      env

let rec wf_block env block =
  let env = { env with pc = block } in
  match block with
  | IPush (vs, _cell, block) ->
      wf_values env vs;
      wf_block env block
  | IPop (ps, _cell, block)
  | IPeek (ps, _cell, block) ->
      let env = defs env ps in
      wf_block env block
  | IDef (bs, block) ->
      let env = wf_bindings env bs in
      wf_block env block
  | IPrim (p, prim, block) ->
      wf_prim env prim;
      let env = defs env [p] in
      wf_block env block
  | ITrace (_, block) ->
      wf_block env block
  | IComment (_, block) ->
      wf_block env block
  | IDead _
  | IStop _ ->
      ()
  | IReturn (_nt, v) ->
      wf_value env v
  | IJump label' ->
      (* Check that every register that is needed at the destination label
         is defined here. *)
      wf_regs env (lookup env.program label').needed
  | ICaseToken (r, branches, odefault) ->
      wf_reg env r;
      List.iter (wf_tokbranch env) branches;
      Option.iter (wf_block env) odefault
  | ICaseTag (r, branches) ->
      wf_reg env r;
      List.iter (wf_tagbranch env) branches

and wf_tokbranch env (tokpat, block) =
  let env = def_tokpat env tokpat in
  wf_block env block

and wf_tagbranch env (tagpat, block) =
  wf_tagpat env tagpat;
  wf_block env block

(* [wf_tblock program label tblock] checks the block [tblock] at address
   [label]. The set of registers needed by this block is used to initialize
   the environment. *)

let wf_tblock program label { block; needed } =
  let pc = block
  and registers = needed in
  let env = { program; label; pc; registers } in
  wf_block env block

let wf program =
  Label.Map.iter (wf_tblock program) program.cfg;
  Time.tick "StackLang: checking well-formedness"

let wf program =
  handle wf program;
  program

(* -------------------------------------------------------------------------- *)

(**We keep track of the potential synchronization between the stack and the
   current state, which is held in the special register [state]. At any time,
   we are in one of three possible situations:

   - [Synced k] means that [state] is consistent with the stack that existed
     before the [k] most recent stack cells were pushed. In particular,
     [Sync 0] means that [state] is consistent with the stack.

   - [Constant tag] means that [state] definitely contains [tag].
     It is not necessarily consistent with the stack.

   - [Unknown] means that [state] is neither known nor consistent with the
     stack.

   The [state] register is not necessarily live everywhere, but that is not
   a problem. We can reason about its value, regardless of whether this value
   is used in the future. *)
type sync =
  | Synced of int
  | Constant of tag
  | Unknown

module Sync = struct

  let push sync =
    match sync with
    | Synced k ->
        Synced (k + 1)
    | Constant _
    | Unknown ->
        (* If the current state was known, it remains known. *)
        (* If it was unknown, it remains unknown. *)
        sync

  let pop sync =
    match sync with
    | Synced k ->
        (* In state [Sync 0], popping a cell off the stack destroys the
           synchronization between the current state and the stack. It can be
           restored by a POP instruction that assigns [state]. *)
        if k > 0 then Synced (k - 1) else Unknown
    | Constant _
    | Unknown ->
        (* If the current state was known, it remains known. *)
        (* If it was unknown, it remains unknown. *)
        sync

  let def bs sync =
    if Bindings.mem state bs then
      match Bindings.apply bs (VReg state) with
      | VTag tag ->
          Constant tag
      | VReg _ ->
          (* An assignment of the register [state] to itself is eliminated
             by the smart constructor [Bindings.assign], and we never move
             another register into [state], so this case cannot arise. *)
          assert false
      | _ ->
          assert false
    else
      sync

  let print sync =
    match sync with
    | Synced k ->
        sprintf "unknown state; synced with the stack at depth %d" k
    | Constant tag ->
        sprintf "known state: %s" (Tag.print tag)
    | Unknown ->
        sprintf "unknown state; unsynced"

end

(* -------------------------------------------------------------------------- *)

(* The type-checking environment. *)

type wt_env = {
  (* The program that is being checked. *)
  program : program;
  (* The label of the block that is being checked. *)
  label : label;
  (* The instruction that is being checked. *)
  pc : block;
  (* The expected type of the current (sub)block. In other words, due to
     contravariance, [current.stack] is the actual current shape of the
     stack, while [current.final] is the expected final type of this block.
     If thinking in terms of block types makes your head hurt, just think
     separately about [current.stack] and [current.final]. *)
  current : block_type;
  (* The current relationship between the stack and the [state] register. *)
  sync : sync;
}

open Invariant

let wt_fail env format =
  Printf.ksprintf (fail env.program env.label env.pc) format

(* -------------------------------------------------------------------------- *)

(* The type-checker. *)

type direction =
  Push | Pop

let direction = function
  | Push ->
      "pushed value"
  | Pop ->
      "popped pattern"

(* Extracting the head of a list, which must be nonempty. *)

let wt_uncons d kind env xs =
  match xs with
  | x :: xs ->
      x, xs
  | [] ->
      wt_fail env "Missing %s in %s.\n" kind (direction d)

(* Checking that a list is empty. *)

let wt_unnil d env vs =
  match vs with
  | _ :: _ ->
      wt_fail env "Too many fields in %s.\n" (direction d)
  | [] ->
      env

(* Optionally extracting the head of a list. If [flag] is [true],
   then its head is extracted and passed to the function [f].
   The environment serves as an accumulator. *)

let wt_maybe_uncons d kind flag f env xs =
  if flag then
    let x, xs = wt_uncons d kind env xs in
    let env = f env x in
    env, xs
  else
    env, xs

let none env _x =
  env

(* [wt_subtype env context target] is used when jumping to a block whose
   type is [target]. It checks that the block type [target] is a subtype of
   the block type [env.current]. In other words, it checks that (1) the
   stack type [env.current.stack] is a subtype of [target.stack] and (2) the
   final type [target.final] is a subtype of [env.current.final].

   Regarding (1), the stack type [env.current.stack] must guarantee the
   existence of at least as many cells as are required by [target.stack],
   and the types of these cells must match.

   Regarding (2), either [target.final] and [env.current.final] are equal,
   or [target.final] is [None] and [env.current.final] is [Some _], which
   means that we are jumping from a function that is monomorphic in its
   final type to a function that is polymorphic in its final type. In short,
   the subtype preorder on final types is [None <= Some _]. *)

let wt_subtype env context target =
  let current = env.current in

  (* 1. Stacks. *)

  let actual, expected = current.stack, target.stack in
  let fail message =
    wt_fail env
      "Failed stack subtype check (%s) : %s.\n\
       Actual shape: %s\n\
       Expected shape: %s\n"
      context
      message
      (print actual)
      (print expected)
  in
  let n1, n2 = length actual, length expected in
  if n1 < n2 then
    fail (sprintf "%i known cells; %i expected cells." n1 n2)
  else
    for i = 1 to n2 do
      if not ((similar (get actual (n1 - i)) (get expected (n2 - i)))) then
        fail (sprintf "the cells at depth %d differ." i)
    done
  ;

  (* Finals. *)

  let actual, expected = target.final, current.final in
  if not (Final.sub actual expected) then
    wt_fail env
      "Failed final subtype check (%s).\n\
       Actual final : %s\n\
       Expected final : %s\n"
      context
      (StackLangPrinter.ToString.final actual)
      (StackLangPrinter.ToString.final expected)

(* [wt_sync env] checks that the stack and the current state are in sync. *)

let wt_sync env =
  match env.sync with
  | Synced k ->
      (* [Synced 0] is great. [Synced k], where [k] is nonzero, is an error. *)
      if k <> 0 then
        wt_fail env "The state and stack are out of sync (%d).\n" k
  | Constant tag ->
      (* The current state is known to be [tag]. Check that the current
         stack is consistent with the shape predicted by the invariant
         for this state. If so, we are effectively also [Sync 0], and
         we are happy. *)
      wt_subtype env "sync check" (invariant env.program tag)
  | Unknown ->
      wt_fail env
        "The state and stack are out of sync (and the state is unknown).\n"

(* [wt_push_state env v] checks that it is permitted to push the value [v],
   which represents a state, into a new cell on top of the current stack. It
   returns an unmodified environment. *)

let wt_push_state env v =
  (* If the value [v] is a register, then it must be the special register
     [state], since no other register is used to hold states. *)
  assert (match v with VReg r -> r = state | _ -> true);
  (* Simulate an assignment of the value [v] to the [state] register.
     This simplifies the code that follows. *)
  let bs = Bindings.assign [PReg state] [v] in
  let env' = { env with sync = Sync.def bs env.sync } in
  (* Now, check that the stack and the virtually modified [state] register
     are in sync. *)
  wt_sync env';
  (* Return an unmodified environment. *)
  env

(* [wt_pop_state env p] checks that it is permitted to pop the pattern [p],
   which represents a state, out of the top stack cell. It returns an
   environment whose [sync] field has been updated so as to reflect a
   possible assignment to the [state] register. *)

let wt_pop_state env p =
  match p with
  | PReg r ->
      (* The register [r] must be the special register [state], since no
         other register is used to hold states. *)
      assert (r = state);
      (* Because a state is popped off the stack and stored in the [state]
         register, this register is now consistent with the stack. *)
      { env with sync = Synced 0 }
  | PWildcard ->
      (* No register is assigned, so the current state is unaffected.
         [env.sync] is updated by [Sync.pop]. *)
      { env with sync = Sync.pop env.sync }

(* [wt_push env vs cell] checks that it is permitted to push a cell whose
   fields are the values [vs]. It also checks that the list [vs] is
   consistent with the description [cell]. The presence or absence of each
   field is dictated by [cell]. *)

let wt_push env vs cell =
  let d = Push in
  (* If a state is being pushed onto the stack, check that it is permitted
     to push this state on top of the current stack. *)
  let env, vs = wt_maybe_uncons d "state" (holds_state cell) wt_push_state env vs in
  (* Check that a semantic value is present or absent, as expected.
     We currently do not check the type of this semantic value. *)
  let env, vs = wt_maybe_uncons d "semantic value" (holds_semv cell) none env vs in
  (* Check that start position is present or absent, as expected.
     We currently do not check that it is indeed a position. *)
  let env, vs = wt_maybe_uncons d "start position" (holds_startp cell) none env vs in
  (* Check the end position. *)
  let env, vs = wt_maybe_uncons d "end position" (holds_endp cell) none env vs in
  (* Check that there remains nothing. *)
  wt_unnil d env vs

(* [wt_pop env ps cell] checks that it is permitted to pop a cell using the
   patterns [ps]. It also checks that the list [ps] is consistent with the
   description [cell]. *)

let wt_pop env ps cell =
  let d = Pop in
  let env, ps = wt_maybe_uncons d "state" (holds_state cell) wt_pop_state env ps in
  let env, ps = wt_maybe_uncons d "semantic value" (holds_semv cell) none env ps in
  let env, ps = wt_maybe_uncons d "start position" (holds_startp cell) none env ps in
  let env, ps = wt_maybe_uncons d "end position" (holds_endp cell) none env ps in
  wt_unnil d env ps

(* [wt_peek] is almost identical to [wt_pop], but does not allow the [state]
   register to be written, so [env] is unaffected. *)

let wt_peek env ps cell =
  assert (not (Pattern.occurs state ps));
  let env0 = env in
  let d = Pop in
  let env, ps = wt_maybe_uncons d "state" (holds_state cell) none env ps in
  let env, ps = wt_maybe_uncons d "semantic value" (holds_semv cell) none env ps in
  let env, ps = wt_maybe_uncons d "start position" (holds_startp cell) none env ps in
  let env, ps = wt_maybe_uncons d "end position" (holds_endp cell) none env ps in
  let env = wt_unnil d env ps in
  assert (env0 = env);
  env

(* [wt_similar env expected actual] checks that the cell shapes [expected]
   and [actual] are similar. It returns one of them. *)

let wt_similar env expected actual =
  if not (Invariant.similar expected actual) then
    wt_fail env "Cell shape mismatch"
  else
    expected

(* [wt_block env block] type-checks the block [block]. It returns a new block,
   where provably dead branches in [casetag] instructions have been removed. *)

let rec wt_block env block : block =
  let env = { env with pc = block } in
  match block with

  | IPush (vs, cell, block) ->
      (* Check that the values [vs] match the description [cell] and that
         it is permitted to push such a cell onto the stack. *)
      let env = wt_push env vs cell in
      (* Update [env.current] and [env.sync]. *)
      let current = env.current in
      let current = { current with stack = push current.stack cell } in
      let sync = Sync.push env.sync in
      let env = { env with current; sync } in
      IPush (vs, cell, wt_block env block)

  | IPop (ps, expected, block) ->
      let current = env.current in
      (* Check that it is possible to pop one cell, and find out about
         its shape. *)
      if length current.stack = 0 then
        wt_fail env "Unsafe pop (%s).\n" (Sync.print env.sync);
      let actual = top current.stack in
      (* Check that the cell shapes [expected] and [actual] match. *)
      let cell = wt_similar env expected actual in
      (* Check that the patterns [ps] match the description [cell].
         Update [env.sync] if the [state] register is written. *)
      let env = wt_pop env ps cell in
      (* Update [env.current]. *)
      let current = { current with stack = pop current.stack } in
      let env = { env with current } in
      IPop (ps, cell, wt_block env block)

  | IPeek (ps, expected, block) ->
      let current = env.current in
      if length current.stack = 0 then
        wt_fail env "Unsafe peek (%s).\n" (Sync.print env.sync);
      let actual = top current.stack in
      (* Check that the cell shapes [expected] and [actual] match. *)
      let cell = wt_similar env expected actual in
      (* We do not allow the [state] register to be written by PEEK.
         As a result, [env] is not affected. *)
      let env = wt_peek env ps cell in
      IPeek (ps, cell, wt_block env block)

  | IDef (bs, block) ->
      let env = { env with sync = Sync.def bs env.sync } in
      IDef (bs, wt_block env block)

  | IJump label ->
      let tblock = lookup env.program label in
      wt_jump env "jump" tblock;
      IJump label

  | ICaseTag (r, branches) ->
      assert (r = state);
      (* The tags that are not covered by [branches] correspond to implicit
         branches whose body is [IDead `Dynamic]. We now make these branches
         explicit, so they are examined by [wt_casetag], allowing it to prove
         some of these branches dead, in which case their body is replaced
         with [IDead `Static]. Once this is done, [trim] makes all branches
         whose body is [IDead `Dynamic] implicit again, for the sake of
         compactness. *)
      (* This examination of all implicit branches is costly, but it seems
         difficult to avoid it, because the OCaml type-checker is really fussy
         about provably dead branches. It wants us to prove that we know what
         we are doing. *)
      let branches = complete env.program branches in
      let branches = wt_casetag env branches in
      let branches = trim branches in
      ICaseTag (r, branches)

  | IPrim (p, prim, block) ->
      assert (not (Pattern.occurs state [p]));
      IPrim (p, prim, wt_block env block)

  | IReturn (nt, v) ->
      (* If this function contains a [return] instruction, then it cannot
         be polymorphic in its final type. So, the final type cannot be
         [None]. In fact, it must be [Some nt]. *)
      let actual = env.current.final
      and expected = Some nt in
      if expected <> actual then
        wt_fail env "Final type mismatch at RET: expected %s, got %s.\n"
          (StackLangPrinter.ToString.final actual)
          (StackLangPrinter.ToString.final expected)
      ;
      IReturn (nt, v)

  | IDead `Static ->
      wt_fail env "IDead `Static must not be used outside ICaseTag.\n"

  | IDead `Dynamic
  | ITrace _
  | IComment _
  | IStop _
  | ICaseToken _
    -> Block.map (wt_block env) block

(* [wt_jump env context tblock] checks a jump to the typed block [tblock].
   It is used both at JUMP instructions and upon entry into an inlined
   block. *)

and wt_jump env context tblock =
  (* Retrieve the type of the target block. *)
  let target = tblock.block_type in
  (* Check that this jump is permitted. *)
  wt_subtype env context target;
  (* If the [state] register is needed by this block, then check that the
     current state and current stack are in sync. Indeed, our convention is
     that, at the beginning of every block, they must be in sync. *)
  if mem state tblock.needed then
    wt_sync env

(* [wt_tblock] checks a typed block. Like [wt_block], it returns an updated
   block where dead branches have been removed. *)

and wt_tblock program label tblock =
  let { block; block_type; _ } = tblock in
  let pc = block
  and current = block_type
  and sync = Synced 0 in
  let env = { program; label; pc; current; sync } in
  let block = wt_block env block in
  { tblock with block }

(* [wt_casetag] checks an [ICaseTag] instruction. *)

and wt_casetag env branches : tagbranch list =
  match env.sync with

  | Synced k ->
      (* This [ICaseTag] instruction inspects the [state] register,
         which is known to be consistent with the stack, provided
         the [k] topmost stack cells are disregarded. [k] is most
         often 0, but could be 1, for instance, if we push a cell
         onto the stack before inspecting the current state in a
         [goto] function. *)
      List.map (wt_casetag_branch env k) branches

  | Constant tag ->
      (* This [ICaseTag] instruction inspects the [state] register,
         whose value is statically known to be [tag]. This is illegal. *)
      wt_fail env "Case analysis on a statically known state (%s).\n"
        (Tag.print tag)

  | Unknown ->
      wt_fail env "Case analysis on an unknown and unsynced state.\n"

(* [wt_casetag_branch] checks one branch in an [ICaseTag] instruction. The
   parameter [k] indicates that [env.sync] is [Synced k]. [tag] is the tag
   of interest. [block] is the block that must be checked under a refined
   environment. *)

and wt_casetag_branch env k (tagpat, block) : tagbranch =
  let { stack; final } = env.current in
  assert (k <= length stack);

  (* We must compute the evolution of [env.current] as we discover that the
     current state is [tag], under the assumption that this state is in sync
     with the stack at depth [k]. Both the stack type and the final type may
     be refined. *)

  (* The upper part of the stack, that is to say, the top [k] stack cells,
     is unchanged. The lower part of the stack is refined: we can take the
     meet (the conjunction) of the previous lower part (which remains valid)
     and of the stack type associated with the state [tag]. If the meet is
     inconsistent (bottom), then the branch is provably dead. *)

  let lower, upper = split stack k in

  let TagSingle tag = tagpat in
  let bty = invariant env.program tag in
  tagpat,
  match meet lower bty.stack, Final.lub final bty.final with
  | None, _
  | _, None ->
      (* A provably dead tag, due either to a mismatch of the stack shapes
         or a mismatch of the final types. Replace the body of this branch
         with a DEAD instruction. *)
      IDead `Static
  | Some lower, Some final ->
      (* A live branch. *)
      let stack = append lower upper in
      let current = { stack; final } in
      let env = { env with current } in
      (* Check the body of this branch. *)
      wt_block env block

(* [wt] checks a program. *)

let wt program =
  let cfg = Label.Map.mapi (wt_tblock program) program.cfg in
  Time.tick "StackLang: checking well-typedness";
  { program with cfg }

let wt program =
  handle wt program
