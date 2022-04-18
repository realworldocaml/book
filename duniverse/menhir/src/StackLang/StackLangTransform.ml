(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let map, length = List.(map, length)
let take, drop = MList.(take, drop)
open Printf
open StackLang
let state = EmitStackLang.state
let print = Tag.print
module Print = StackLangPrinter.ToString

(* -------------------------------------------------------------------------- *)

(* [no_info format ... x] returns [x]. [no_info] is used in the definition of
   [info], and has the same type as [info]. *)

(* [ikfprintf] has existed since OCaml 4.01.0, but its type has been made more
   general in OCaml 4.03.0, and this extra generality is exploited here. *)

let no_info format =
  ikfprintf (fun () x -> x) () format

(* [info format ... block] constructs an information message which appears in
   the generated code if [decorate] is true. Its result is the block [block],
   possibly annotated with a comment. *)

let decorate =
  false

let info =
  if decorate then
    fun format ->
      ksprintf (fun s block -> IComment (s, block)) format
  else
    no_info

(* [kinfo] is analogous to [info], but is applied to a triple [(_, _, block)]
   instead of just a block [block]. *)

let kinfo =
  if decorate then
    fun format ->
      ksprintf (fun s (x, y, block) -> x, y, IComment (s, block)) format
  else
    no_info

(* Correct singular and plural forms for "cells". *)

let cells i =
  match i with
  | 0 ->
      "0 cell"
  | 1 ->
      "1 cell"
  | _ ->
      sprintf "%d cells" i

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Specialization. *)

(* The purpose of this program transformation is to specialize certain
   routines for the situation where the [state] register contains a
   statically known tag.

   This is important, in particular, because it allows Unit Production
   Elimination (UPE).

   One might wonder whether we also need to specialize certain routines for
   the situation where the top stack cell contains a statically known tag.
   This is the case of the [reduce] routines associated with unit
   productions. We are able to get away without this extra complexity by
   recognizing these routines and inlining them instead, with the same
   end result. *)

module Specialize (X : sig val program : program end) = struct open X

(* [can_specialize label] determines whether it is desirable to create a
   specialized version of the routine labeled [label] for the situation
   where the current state is statically known.

   This function controls the specialization machinery.

   We obey the specialization hint transmitted to us by EmitStackLang. *)

let can_specialize label =
  let tblock = lookup program label in
  match tblock.spec with
  | SpecAllowed ->
      (* If the register [state] does not appear among the registers needed by
         the block labeled [label], then specialization would be pointless, so
         we return [false]. This situation is rare; almost always, the [state]
         register is needed. *)
      Reg.Set.mem state tblock.needed
  | SpecDisallowed ->
      false

(* [can_inline_when_known_state label] determines whether it is permitted (and
   desirable) to inline the routine [label] in a situation where the current
   state is statically known.

   [can_inline_when_known_stack label] determines whether it is permitted (and
   desirable) to inline the routine [label] in a situation where the state
   contained in the top stack cell is statically known.

   This must be used with care (beware of code explosion and divergence!).

   When the current state is known, we inline [goto] routines, which we
   recognize by the inlining hint [OnlyIfKnownState]. In such a situation, a
   [goto] routine degenerates to a single [jump] instruction. We also inline
   [reduce] routines associated with epsilon productions, which we recognize
   by the inlining hint [IfCells 0]. Inlining these [reduce] routines in turn
   allow inlining the [goto] routines that follow them. This guarantees that
   a reduction of an epsilon production never requires a case analysis of
   the current state.

   When the top stack state is known, we inline the routines that carry the
   inlining hint [IfCells 1]. These are the [reduce] routines associated with
   unit productions. Inlining these routines removes the need to specialize
   them for the situation where the top stack cell contains a known tag. *)

let can_inline_when_known_state label =
  let tblock = lookup program label in
  match tblock.hint with
  | Always
  | OnlyIfKnownState
  | IfCells 0 ->
      true
  | IfCells _
  | NoHint ->
      false

let can_inline_when_known_stack label =
  let tblock = lookup program label in
  match tblock.hint with
  | Always
  | IfCells 1 ->
      true
  | IfCells _
  | OnlyIfKnownState
  | NoHint ->
      false

(* The control flow graph (under construction) of the specialized program. *)

let cfg =
  ref Label.Map.empty

(* A queue of [(label, tag)] pairs, waiting to be processed. Each such pair
   represents a request to specialize the block [label] for the state [tag]. *)

let queue : (label * tag) Queue.t =
  Queue.create()

(* A set of the pairs that have already been processed (therefore must not
   be entered again into the queue). *)

module LTSet =
  Set.Make (struct
    type t = label * tag
    let compare (label1, tag1) (label2, tag2) =
      let c = Label.compare label1 label2 in
      if c <> 0 then c else Tag.compare tag1 tag2
  end)

let inserted =
  ref LTSet.empty

(* A set of the labels that have been specialized. This set is used for
   counting purposes only. *)

let labels =
  ref Label.Set.empty

(* [enqueue label tag] inserts the pair [(label, tag)] into the queue,
   unless it has already been inserted before. *)

let enqueue label tag =
  let pair = (label, tag) in
  if not (LTSet.mem pair !inserted) then begin
    Queue.add pair queue;
    inserted := LTSet.add pair !inserted;
    labels := Label.Set.add label !labels
  end

(* [spec_label label tag] is the conventional name of the specialized
   copy of the block labeled [label] for the tag [tag]. *)

let spec_label label tag : label =
  sprintf "%s_spec_%s" (Label.export label) (print tag)
  |> Label.import

(* [specialize_block_type tag block_type] specializes the block type for the
   situation where the current state is [tag]. The result is a block type
   that is a *supertype* of the original block type.

   To do so, we take the meet of the original expected stack shape and the
   stack shape that is guaranteed by [tag]. Symmetrically, we take the join
   of the original final and of the final that is associated with [tag].

   If we did not do this, then a PUSH instruction in the block could become
   ill-typed. Indeed, the typing rule for PUSH is more stringent when the
   current state is known than when it is unknown. *)

let spec_block_type tag block_type =
  let tag_type = invariant program tag in
  match
    Invariant.meet block_type.stack tag_type.stack,
    Final.lub block_type.final tag_type.final
  with
  | Some stack, Some final ->
      { stack; final }
  | None, _
  | _, None ->
      (* The meet is undefined. This means that the specialized routine
         that we are attempting to create must be dead! Yet, this seems
         impossible, since we perform specialization only on demand and
         only in reachable code -- that is, there is a call site where
         this specialized routine is needed. *)
      assert false

(* The specialization environment carries information about the stack
   and about the [state] register. It is carried down while a block
   is being transformed. Its eventual purpose is to allow on-the-fly
   simplification of [casetag] instructions and replacement of [jump]
   instructions with jumps to specialized routines.

   Because our main intention is to enable Unit Production Elimination
   (UPE), the information that we maintain about the stack is limited
   to depth 1. All we care about is to know whether the top stack cell
   contains a known state. *)

type env = {

  (* If [stack] is [Some tag], then the top stack cell definitely
     contains the tag [tag] (in its first field). Otherwise, nothing
     is known about the stack. *)
  stack: tag option;

  (* If [state] is [Some tag], then the [state] register definitely
     contains the tag [tag]. Otherwise, nothing is known about this
     register. *)
  state: tag option;

  (* Are we inside a CASEtag? This information is used to turn off
     constant propagation inside the branches of a [goto] routine. *)
  incasetag: bool;

}

(* A debugging printer. *)

let print_tag_option otag =
  match otag with
  | None ->
      "<unknown>"
  | Some tag ->
      Tag.print tag

let print_env env =
  sprintf "stack: %s; state: %s"
    (print_tag_option env.stack)
    (print_tag_option env.state)

(* [apply env v] transforms [env.state] into a substitution, which it applies
   to the value [v]. If [env.state] is [None], this substitution has no
   effect. If [env.state] is [Some tag], this substitution replaces the
   [state] register with the value [VTag tag]. Thus, in addition to
   specialization, we perform a little bit of constant propagation. This is
   cheap and produces slightly more aesthetic code. (Indeed, once all uses of
   [state] have been replaced with the constant [tag], the instruction [DEF
   state <- tag] can disappear as well.) *)

let apply env v =
  match env.state with
  | None ->
      v
  | Some tag ->
      let bs = Bindings.assign [PReg state] [VTag tag] in
      Bindings.apply bs v

(* [materialize env block] materializes the information in [env.state] into
   either nothing or an instruction of the form [DEF state <- tag]. This is
   used immediately before a JUMP instruction, and is part of the constant
   propagation machinery described above. *)

(* This transformation is disabled inside CASEtag instructions, because its
   effect there is to replace the variable [state] with a constant, thus
   making every branch look different, whereas in reality some branches may
   be identical (and could be shared, if the OCaml type-checker allows it). *)

let materialize env block =
  match env.state, env.incasetag with
  | None, _
  | _, true ->
      block
  | Some tag, false ->
      let bs = Bindings.assign [PReg state] [VTag tag] in
      Block.def bs block

(* Specialization. *)

let rec spec_block env block =
  match block with

  | IPush (vs, cell, block) ->
      let env, vs =
        if Value.occurs state vs then begin
          (* We are pushing the [state] register onto the stack. *)
          assert (Invariant.holds_state cell);
          assert (vs <> [] && List.hd vs = VReg state);
          { env with stack = env.state },
          map (apply env) vs
        end
        else
          (* Nothing is known about the new top stack cell. *)
          { env with stack = None }, vs
      in
      IPush (vs, cell, spec_block env block)

  | IPop (ps, cell, block) ->
      let env =
        if Pattern.occurs state ps then begin
          (* We are popping the [state] register off the stack. *)
          assert (ps <> [] && List.hd ps = PReg state);
          { env with stack = None; state = env.stack }
        end
        else
          (* Nothing is known about the new top stack cell. *)
          { env with stack = None }
      in
      IPop (ps, cell, spec_block env block)

  | IPeek (ps, cell, block) ->
      assert (not (Pattern.occurs state ps));
      IPeek (ps, cell, spec_block env block)

  | IDef (bs, block) ->
      let env =
        match Bindings.apply bs (VReg state) with
        | VTag tag ->
            (* We are assigning a known value to the [state] register. *)
            { env with state = Some tag }
        | VReg r' ->
            assert (state = r');
            (* We are assigning some other register. *)
            env
        | _ ->
            assert false
      in
      IDef (bs, spec_block env block)

  | IJump label ->
      (* If the current state is known to be [tag], and if the block [label]
         can be specialized for this tag, then enqueue a specialization
         request (if necessary) and replace this instruction with a jump to
         the specialized block. *)
      if env.state <> None && can_inline_when_known_state label then
        let tag = Option.force env.state in
        info "(spec) Inlining %s (state = %s)" (Label.export label) (print tag)
        (spec_block env (lookup program label).block)

      else if env.stack <> None && can_inline_when_known_stack label then
        let tag = Option.force env.stack in
        info "(spec) Inlining %s (stack = %s)" (Label.export label) (print tag)
        (spec_block env (lookup program label).block)

      else if env.state <> None && can_specialize label then begin
        let tag = Option.force env.state in
        enqueue label tag;
        info "(spec) Specializing %s (state = %s)" (Label.export label) (print tag)
        (IJump (spec_label label tag))
      end

      else
        info "(spec) Cannot inline or specialize (%s)" (print_env env)
        (materialize env (IJump label))

  | ICaseTag (r, branches) ->
      assert (r = state);
      begin match env.state with
      | Some tag ->
          (* The current state is known to be [tag]. Eliminate this [casetag]
             construct. *)
          let block = Block.select_branch tag branches in
          info "(spec) Eliminating casetag (state = %s)" (print tag)
          (spec_block env block)
      | None ->
          (* The [casetag] construct cannot be eliminated. We can (and should)
             still transform its branches. *)
          ICaseTag (r, List.concat (List.map (spec_casetag_branch env) branches))
      end

  | IPrim _
  | ITrace _
  | IComment _
  | IDead _
  | IStop _
  | IReturn _
  | ICaseToken _
    ->
      Block.map (spec_block env) block

and spec_casetag_branch env (TagSingle tag, body) =
  (* In this branch, we learn that the current state is [tag]. *)
  let env = { env with state = Some tag; incasetag = true } in
  [ TagSingle tag, spec_block env body ]

let spec_tblock tag tblock =
  assert (Reg.Set.mem state tblock.needed);
  (* The current state is initially assumed to be [tag].
     Nothing is initially assumed about the stack. *)
  let env = { stack = None; state = Some tag; incasetag = false } in
  (* Specialize this block. *)
  let block = spec_block env tblock.block in
  (* Insert a definition of the register [state] at the beginning. This
     ensures that this block does not take [state] as a parameter. If
     our constant propagation has the desired effect, this definition
     should be dead anyway. *)
  let block = IDef (Bindings.assign [PReg state] [VTag tag], block) in
  (* Remove [state] from the set of needed registers. Better do it
     explicitly, if we later decide to recompute the needed registers
     by calling [NeededRegisters.update]. *)
  let needed = Reg.Set.remove state tblock.needed in
  (* Specialize the type of this block. *)
  let block_type = spec_block_type tag tblock.block_type in
  (* Done. *)
  { tblock with block; block_type; needed }
  (* The specialized routine carries the same
     inlining hint as the original routine. *)

let spec_routine (label, tag) =
  let block = spec_tblock tag (lookup program label) in
  cfg := Label.Map.add (spec_label label tag) block !cfg

let inspect_tblock tblock =
  (* Nothing is initially assumed. *)
  let env = { stack = None; state = None; incasetag = false } in
  (* Inspect and transform this block. *)
  let block = spec_block env tblock.block in
  { tblock with block }

let inspect_routine _label tblock =
  inspect_tblock tblock

let program =
  cfg := Label.Map.mapi inspect_routine program.cfg;
  Misc.qiter spec_routine queue;
  { program with cfg = !cfg }

let () =
  Error.logC 1 (fun f ->
    fprintf f "%d specialized copies of %d functions have been created.\n"
      (LTSet.cardinal !inserted)
      (Label.Set.cardinal !labels)
  )

end (* Specialize *)

let specialize program =
  let module S = Specialize(struct let program = program end) in
  Time.tick "StackLang: specialization";
  S.program

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Commuting PUSH instructions. *)

module CommutePushes (X : sig val program : program end) = struct open X

(* We maintain a set of discovered labels and a FIFO queue of labels that
   have been discovered but not yet processed. *)

let discovered : Label.Set.t ref =
  ref Label.Set.empty

let waiting : label Queue.t =
  Queue.create()

(* [discover label] marks the label [label] as discovered and inserts it into
   the waiting queue if was not already discovered. *)

let discover label =
  if not (Label.Set.mem label !discovered) then begin
    discovered := Label.Set.add label !discovered;
    Queue.add label waiting
  end

(* We move trains (sequences) of PUSH instructions forward in the code.
   A wagon (a single PUSH instruction) contains the values [vs] and the
   cell [cell] that appear in the PUSH instruction. *)

type wagon =
  values * cell

(* A train is a list of wagons. The head of the list is the instruction
   that appears at the end of the instruction sequence. *)

type train =
  wagon list

(* [pushes_vanish block] determines whether a PUSH instruction, in front
   of the block [block], can vanish. This is the case if [block] contains
   a sequence of DEF, PUSH, TRACE instructions that ends with DEAD or STOP. *)

(* This information is exploited by [materialize], which itself is used in
   [pad_branch]. *)

let rec pushes_vanish block =
  match block with
  | IDead _
  | IStop _ ->
      true
  | IComment (_, block)
  | IDef (_, block)
  | IPush (_, _, block)
  | ITrace (_, block) ->
      pushes_vanish block
  | _ ->
      false

(* [materialize pushes block] re-creates the PUSH instructions described by
   the list [pushes] in front of the block [block]. *)

let materialize (pushes : train) block =
  if pushes_vanish block then
    info "(optm) Some push instructions have vanished here"
    block
  else
    List.fold_left (fun block (vs, cell) ->
      info "(optm) Re-materializing push"
      (IPush (vs, cell, block))
    ) block pushes

(* These auxiliary functions determine whether the register [r] occurs
   in a wagon and in a train. *)

let occurs_in_wagon r wagon =
  let (vs, _cell) = wagon in
  Value.occurs r vs

let occurs_in_train r wagons =
  List.exists (occurs_in_wagon r) wagons

(* [current_state_is_known bs] determines whether, according to the
   bindings [bs], the current value of the [state] register is known. *)

let current_state_is_known bs =
  match Bindings.apply bs (VReg state) with
  | VTag _ ->
      true
  | VReg _ ->
      false
  | _ ->
      assert false

(* We are pushing forward a train of PUSH instructions followed with a DEF
   instruction, that is, [PUSH*; DEF]. The PUSH instructions are represented
   by a list [pushes], where the head of the list is the rightmost PUSH, the
   most recent PUSH. The DEF instruction carries a set of bindings [bs]. *)

(* The information that is carried down during the transformation is gathered
   in an environment. *)

type env = {

  path: Label.Set.t;
  (** The path [path] records the labels of the routines that we have decided
      to inline. It is used to detect and forbid cycling inlining. *)

  pushes: train;
  (** [pushes] is a train of PUSH instructions that is being pushed forward. *)

  bs: bindings;
  (**[bs] is a set of bindings that is also being pushed forward. *)

  fresh: unit -> int;
  (**[fresh] is a generator of fresh integers. *)

}

(* We must sometimes rename a register in order to avoid a name clash.
   We add a fresh numeric suffix (delimited with an underscore) to the
   register's original name. *)

let rename env (r : register) : register =
  Reg.import (sprintf "%s_%d" (Reg.export r) (env.fresh()))

(* [transform_assignment_to_reg env r] moves the PUSH train [env.pushes] and
   the bindings [env.bs] past an assignment of the register [r]. It returns
   a pair of an updated environment and a possibly renamed register. *)

let transform_assignment_to_reg env r =
  if occurs_in_train r env.pushes then
    (* [r] appears in the train. To avoid a name clash, we rename [r] to [r'],
       so the assignment of [r] becomes an assignment of [r'] instead, and we
       we add an assignment [DEF r <- r'] to the traveling bindings [bs], so
       that the code that follows is not affected. *)
    let r' = rename env r in
    let bs' = Bindings.(seq env.bs (assign [PReg r] [VReg r'])) in
    { env with bs = bs' }, r'
  else
    (* [r] does not appear in the train. Thus, the train can safely move past
       an assignment of [r]. We simply have to remove [r] from the domain of
       the bindings [bs], since the new assignment hides any previous binding. *)
    let bs = Bindings.remove env.bs (Reg.Set.singleton r) in
    let env = { env with bs } in
    env, r

let transform_assignment_to_pat env p =
  match p with
  | PReg r ->
      let env, r = transform_assignment_to_reg env r in
      env, PReg r
  | PWildcard ->
      (* No renaming is necessary. *)
      env, p

let transform_assignment_to_tokpat env tokpat =
  match tokpat with
  | TokSingle (tok, r) ->
      let env, r = transform_assignment_to_reg env r in
      env, TokSingle (tok, r)
  | TokMultiple _ ->
      (* No renaming is necessary. *)
      env, tokpat

(* [do_not_transform_block] is a trivial function that satisfies the same
   specification as [transform_block] (see below). It consumes zero PUSH
   instructions, achieves zero happiness, and leaves [DEF bs; block]
   unchanged. *)

let do_not_transform_block env block : int * int * block =
  Block.successors discover block;
  0, 0, IDef (env.bs, block)

(* [transform_block env block] attempts to move the sequence [PUSH pushes;
   DEF bs] into the block [block]. There is no obligation to use all of the
   PUSH instructions in the list [pushes]: instead, [transform_block]
   returns an integer [k] that indicates how many PUSH instructions were
   used. It also returns an integer happiness level and (of course) a
   transformed block [block'].

   If [pushes'] is defined as [drop k pushes], then [PUSH pushes'; block']
   must be semantically equivalent to [PUSH pushes; DEF bs; block].

   It is possible to achieve nonzero happiness even without absorbing any
   PUSHES: thus, [k = 0 && happiness > 0] is possible. On the other hand, we
   ensure that [k > 0 && happiness = 0] is impossible; that is, we absorb
   one or more PUSH instructions only if this helps make us happy. Thus,
   [k > 0] implies [happiness > 0], and [happiness = 0] implies [k = 0]. *)

let rec transform_block env block : int * int * block =
  match block with

  | IPush (_vs, _cell, block) when pushes_vanish block ->
      (* As a special case, if the block that follows is so trivial that PUSH
         instructions in front of it vanish, then we are done. We drop the
         PUSH instructions (and transform the block, which should result in no
         transformation). We include this special rule because STOP reports
         that it does not want to absorb any PUSHes (see below). Thus, this
         rule remains necessary in order to transform PUSH; STOP into STOP. *)
      kinfo "(optm) Some push instructions have vanished here"
      (do_not_transform_block env block)

  | IPush (vs, cell, block) ->
      (* [DEF bs; PUSH vs] is equivalent to [PUSH vs'; DEF bs], where [vs'] is
         the result of applying the bindings [bs] to the values [vs]. *)
      let vs = map (Bindings.apply env.bs) vs in
      (* Add this wagon to the train and continue. *)
      let wagon = (vs, cell) in
      let pushes = wagon :: env.pushes in
      let env = { env with pushes } in
      let k, happiness, block = transform_block env block in
      (* [k] PUSH instructions have been absorbed into [block]. If [k] is zero,
         then this PUSH instruction should remain here; otherwise, it has been
          absorbed. Either way, the happiness level is transmitted. *)
      if k = 0 then
        0,
        happiness,
        info "(optm) Not moving this push instruction"
        (IPush (vs, cell, block))
      else
        (* This is where we may report zero absorbed PUSHes but nonzero
           happiness. *)
        k - 1,
        happiness,
        info "(optm) A push instruction was here"
        block

  | IPop (ps, cell, block) ->
      transform_pop env ps cell block

  | IPeek _ ->
      (* We expect PEEK to be rarely used, and we expect it to be used in a
         specific context (namely, a combined [action/goto] routine) which a
         PUSH train cannot enter. So, there is no need to be smart here. The
         train, if there is one, stops here. *)
      do_not_transform_block env block

  | IDef (bs', block) ->
      (* The train [PUSH*; DEF bs], which we are pushing forward, reaches an
         instruction of the form [DEF bs']. By constructing the sequential
         composition of [bs] and [bs'], we absorb this DEF instruction and
         obtain a new train of the form [PUSH*; DEF]. *)
      let bs = Bindings.seq env.bs bs' in
      let env = { env with bs } in
      transform_block env block

  | IPrim (p, prim, block) as original_block ->
      let original_env = env in
      (* This instruction assigns the pattern [p]. *)
      let env, p = transform_assignment_to_pat env p in
      (* The bindings [bs] are applied to the primitive operation. *)
      let prim = Primitive.apply env.bs prim in
      (* There remains to transform the remainder of the block. *)
      let k, happiness, block = transform_block env block in
      if happiness > 0 then
        (* We could always return this... *)
        k, happiness, IPrim (p, prim, block)
      else begin
        assert (k = 0);
        (* ... but, as a special case, if [happiness] is zero, which means
           that the remainder of the block was not transformed in a useful
           way, then we prefer to use [do_not_transform_block], so as to
           avoid the use of [Primitive.apply], which creates local bindings
           in front of a semantic action. This is purely aesthetic. This
           special case is the common case: we do not usually expect a PUSH
           instruction to be able to travel past a semantic action. *)
        do_not_transform_block original_env original_block
      end

  | ITrace (t, block) ->
      let k, happiness, block = transform_block env block in
      k, happiness, ITrace (t, block)

  | IComment (comment, block) ->
      let k, happiness, block = transform_block env block in
      k, happiness, IComment (comment, block)

  | IDead phase ->
      0, 0, IDead phase

  | IStop s ->
      (* [PUSH*; DEF; STOP] is equivalent to [STOP]. Thus, STOP is able to
         absorb an arbitrary number of PUSH instructions. However, we do not
         wish to return the integer [length pushes], because that would mean
         that we *want* to absorb all of them, and when STOP appears in a
         branch of a CASE construct, that would imply that all available PUSH
         instructions *must* enter the CASE construct. In reality, STOP does
         not care how many PUSHes enter the CASE construct; it will absorb all
         of them, anyway. So, we artificially return 0, so as to not influence
         the [max] computation that takes place at CASE constructs. *)
      0, 0, IStop s

  | IReturn (nt, v) ->
      (* We certainly have no PUSH instructions, because the parser accepts
         only when the stack is empty. We materialize just [DEF bs] and
         merge it into the RETURN instruction. *)
      assert (env.pushes = []);
      0, 0, IReturn (nt, Bindings.apply env.bs v)

  | IJump label ->

      (* [transform_block] is never applied to a branch in a [casetag]
         construct, so we need not worry about preserving the property that
         every branch of a [casetag] construct must be a jump. *)

      (* In general, we want the [PUSH*; DEF] train to stop here. We do not
         attempt to move it past the JUMP instruction and into the target
         block; that would require agreement between the predecessors of the
         label [label]. *)

      (* In some situations, however, we want to inline the target block. This
         is the case, for instance, when the target block is a [goto] routine
         and the current state is statically known; or when the target block
         is a [reduce] routine and our PUSH train is long enough to cancel all
         of the POP instructions inside it. (The details may vary.) *)

      (* Something one might wish to do is to first speculatively inline, then
         inspect the integer result [k], which tells how many PUSH instructions
         can be absorbed by the inlined routine. If [k] is positive, then
         inlining may be worthwhile. A quick experiment suggests that this is
         too aggressive: on a small grammar, the increase in code size is x4; on
         larger grammars, Menhir does not terminate in a reasonable time. *)

      (* If the target label already appears in the path that we have
         followed, then we cannot inline; we would fall into a cycle. If we
         can and do inline, then we must add this label to the path that we
         have followed. *)

      let do_not_inline format =
        ksprintf (fun s ->
          kinfo "%s" s
          (do_not_transform_block env block)
        ) format
      in

      if Label.Set.mem label env.path then
        do_not_inline "(optm) Cannot inline (cyclic path)"
      else

        let target = lookup program label in

        let do_inline format =
          ksprintf (fun s ->
            let path = Label.Set.add label env.path in
            let env = { env with path } in
            kinfo "%s" s
            (transform_block env target.block)
          ) format
        in

        begin match target.hint with
          | Always ->
              do_inline "(optm) Inlining %s (always inlined)" (Label.export label)
          | OnlyIfKnownState ->
              if current_state_is_known env.bs then
                do_inline "(optm) Inlining %s (known state)" (Label.export label)
              else
                do_not_inline "(optm) Cannot inline (unknown state)"
          | IfCells k ->
              let h = length env.pushes in
              (* If we have [k] PUSHes or more, then inline this routine. This
                 guarantees, in particular, that [reduce] routines for epsilon
                 productions are always inlined. Inlining when [k] cells are at
                 hand is good, because in this case, the [goto] routine that
                 follows can be inlined as well, and the CASEtag instruction can
                 be simplified. *)
              if k <= h then
                do_inline "(optm) Inlining %s (%s)" (Label.export label) (cells k)
                (* Even if we do not have [k] PUSHes, as soon as we have at
                   least one PUSH, we inline anyway; this allows at least one
                   PUSH/POP elimination. *)
              else if 0 < h then
                do_inline "(optm) Inlining %s (%s at hand, %s ideally needed)"
                  (Label.export label) (cells h) (cells k)
              else
                do_not_inline "(optm) Cannot inline (%s needed, %s at hand)"
                  (cells k) (cells h)
          | NoHint ->
              do_not_inline "(optm) Cannot inline (no hint)"
        end

  | ICaseToken (r, branches, odefault) ->
      transform_casetok env r branches odefault

  | ICaseTag (r, branches) ->
      assert (r = state);
      transform_casetag env branches

and transform_pop env ps cell block : int * int * block =

  (* We begin by restricting [bs] so that no names are bound both in [bs] and
     in [ps]. Clearly, this is correct: if a name is bound in [bs] and in [ps],
     then the binding in [bs] is useless and can be removed. *)

  let bs = Bindings.remove env.bs (Pattern.registers ps) in
  let env = { env with bs } in

  (* Determine whether the PUSH train is empty or nonempty. *)
  match env.pushes with

  | [] ->
      (* Our train of PUSH instructions is unfortunately empty, so this POP
         instruction cannot be cancelled. The remainder of the block is
         transformed. *)
      let k, happiness, block = transform_block env block in
      assert (k = 0);
      k, happiness, IPop (ps, cell, block)

  | (vs, _cell) :: pushes ->
      assert (Invariant.similar cell _cell);
      (* Our train of PUSH instructions is nonempty; the rightmost instruction
         in it is [PUSH vs]. Thus, we are looking at [PUSH vs; DEF bs; POP ps].

         Because no name is bound both in [bs] and in [ps], one might think
         that [DEF bs] and [POP ps] commute. This is not true: some registers
         in the codomain of [bs] could be defined by [ps]. Instead, we remark
         that [PUSH vs; DEF bs; POP ps] is equivalent to [DEF (bs || ps := vs)].
         Indeed, it is easy to check that these sequences have the same effect
         on the registers in [bs] and on the registers in [ps]. *)

      let bs = Bindings.(par bs (assign ps vs)) in
      let env = { env with pushes; bs } in

      (* We consume one wagon from the train, and propose the remainder of
         the train for use in the transformation of the remainder of the
         block. If this transformation consumes [k] wagons, then [k+1]
         wagons have been consumed in total. Because we absorb at least
         one PUSH instruction, we may report nonzero happiness. *)
      let k, happiness, block = transform_block env block in

      k + 1,
      happiness + 1,
      info "(optm) Cancelled push %s with pop %s"
        (Print.values vs)
        (Print.patterns ps)
      block

and transform_casetag env branches : int * int * block =
  match Bindings.apply env.bs (VReg state) with
  | VTag tag ->
      (* The bindings [bs] assign a known value to the register [state].
         This allows us to statically reduce this [casetag] construct. This
         is good! We bump our happiness level to indicate that we are happy.
         This probably makes no difference (indeed, if we reach this point,
         then we have probably successfully eliminated PUSH/POP pairs
         already, so our happiness level is already nonzero), but let's do
         it, just in case. *)
      let block = Block.select_branch tag branches in
      kinfo "(optm) Eliminating casetag"
      (happily (transform_block env block))
  | VReg r' ->
      assert (r' = state);
      (* The bindings [bs] do not allow reducing this [casetag] construct,
         which must therefore be preserved. We cannot allow the PUSH train to
         enter it, because the branches of a [casetag] construct must remain
         small; ideally, they should be jumps. So, the train stops here. *)
      do_not_transform_block env (ICaseTag (state, branches))
  | _ ->
      assert false

and happily (k, happiness, block) =
  k, happiness + 1, block

and transform_casetok env r branches odefault : int * int * block =
  (* As a simplifying assumption, we assume that the bindings in [bs] do not
     apply to the register [r]. This is true, at the moment, because [r] must
     be the [token] register and this register is never written by a DEF
     instruction. *)
  assert (not (Bindings.mem r env.bs));
  (* Propose to send the PUSH train down into every branch. For every
     branch, this yields a triple of an integer number of absorbed PUSHes, a
     happiness level, and a transformed branch. *)
  let branches = map (transform_casetok_branch env r) branches
  and odefault = Option.map (transform_block env) odefault in
  (* Compute the total happiness. *)
  let add2 (_, happiness, _) accu = happiness + accu in
  let happiness = List.fold_right add2 branches 0 in
  let happiness = Option.fold add2 odefault happiness in
  (* Compute the maximum number of PUSHes that are absorbed by a branch.
     This is the number of PUSHes that we must move into the [casetok]
     construct. It is therefore the number of PUSHes that we absorb. *)
  let max1 (j, _, _) accu = max j accu in
  let k = List.fold_right max1 branches 0 in
  let k = Option.fold max1 odefault k in
  assert (k <= List.length env.pushes);
  (* Keep only the PUSHes that we wish to absorb. *)
  let pushes = take k env.pushes in
  (* Now, pad each branch. Each branch has already absorbed [j] PUSHes,
     where [j <= k] holds. We must therefore move the remaining [k-j]
     PUSHes into this branch. *)
  let branches = map (pad_branch pushes) branches
  and odefault = Option.map (pad_default pushes) odefault in
  k, happiness, ICaseToken (r, branches, odefault)

and transform_casetok_branch env _r branch : int * int * tokbranch =
  let tokpat, block = branch in
  (* Rename the pattern if necessary. *)
  let env, tokpat = transform_assignment_to_tokpat env tokpat in
  (* Transform the body of this branch. *)
  let j, happiness, block = transform_block env block in
  j, happiness, (tokpat, block)

and pad_branch pushes (j, _happiness, (tokpat, block)) : tokbranch =
  (* This branch has absorbed [j] PUSHes out of the list [pushes], so
     the remainder of the list must be re-materialized at the beginning
     of the branch. *)
  tokpat,
  materialize (drop j pushes) block

and pad_default pushes (j, _happiness, block) : block =
  (* Same as above, with an implicit wildcard pattern. *)
  materialize (drop j pushes) block

let transform_tblock label tblock =
  let path = Label.Set.singleton label in
  let pushes = []
  and bs = Bindings.empty
  and fresh = Misc.mkgensym() in
  let env = { path; pushes; bs; fresh } in
  let k, _happiness, block = transform_block env tblock.block in
  assert (k = 0);
  { tblock with block }

(* Initialization and main loop. *)

let program =
  (* Create an empty new control flow graph. *)
  let cfg = ref Label.Map.empty in
  (* Insert the entry labels into the queue. *)
  program.entry |> StringMap.iter begin fun _ label ->
    discover label
  end;
  (* Process the waiting labels. *)
  waiting |> Misc.qiter begin fun label ->
    assert (Label.Set.mem label !discovered);
    assert (not (Label.Map.mem label !cfg));
    let tblock = lookup program label in
    let tblock = transform_tblock label tblock in
    cfg := Label.Map.add label tblock !cfg;
  end;
  (* Done. *)
  { program with cfg = !cfg }

end (* CommutePushes *)

let commute_pushes program =
  let module CP = CommutePushes(struct let program = program end) in
  Time.tick "StackLang: moving PUSHes";
  CP.program

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* [in_degree program] computes the in-degree of every label in the program
   [program]. The result is a table that maps every reachable label to its
   in-degree. Unreachable labels do not appear in the table. *)

(* Every entry label artificially receives an in-degree of at least 2. Every
   edge that originates in a [casetag] construct also confers in-degree 2,
   so as to forbid inlining this edge. *)

let in_degree program =

  (* Initialize a queue and a map of labels to degrees. *)
  let queue  : label Queue.t = Queue.create()
  and degree : int Label.Map.t ref = ref Label.Map.empty in

  (* [tick incasetag label] increments the degree associated with [label]. If
     its previous degree was zero, then [label] is enqueued for exploration. *)
  let tick incasetag label =
    let d =
      try
        Label.Map.find label !degree
      with Not_found ->
        Queue.add label queue;
        0
    in
    let delta = if incasetag then 2 else 1 in
    degree := Label.Map.add label (d + delta) !degree
  in

  (* [visit () label] examines the block at address [label]. *)
  let visit () label =
    (lookup program label).block
    |> Block.jumps tick
  in

  (* Initialize the queue with the entry labels. Every entry label is
     inserted into the queue with an in-degree of 2, so it cannot be
     inlined or considered unreachable. *)
  program.entry |> StringMap.iter (fun _name label ->
    Queue.add label queue;
    degree := Label.Map.add label 2 !degree
  );

  (* Process the queue until it  becomes empty. Return the final table. *)
  Misc.qfold visit () queue;
  !degree

(* -------------------------------------------------------------------------- *)

(* Removing unreachable blocks. *)

(* Removing unreachable blocks before attempting to type-check the StackLang
   program can be important. The static analysis of the stack shape and of the
   final type in an unreachable block should in principle yield bottom, but we
   do not have a way of expressing this, so we run a risk of producing
   something else (e.g. an empty stack shape, or an unknown final type). This
   can cause a jump from an unreachable block to a reachable block to be
   considered ill-typed. *)

let remove_unreachable_blocks program =
  let degree = in_degree program in
  let cfg = Label.Map.fold (fun label block accu ->
    if Label.Map.mem label degree then
      Label.Map.add label block accu
    else
      accu
  ) program.cfg Label.Map.empty in
  Time.tick "StackLang: removing unreachable blocks";
  { program with cfg }

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Inlining routines whose in-degree is 1. *)

module Inline (X : sig
  val cautious: bool
  val program : program
end) = struct open X

(* Compute every label's in-degree. *)

let degree =
  in_degree program

(* We maintain a set of discovered labels and a FIFO queue of labels that
   have been discovered but not yet processed. *)

let discovered : Label.Set.t ref =
  ref Label.Set.empty

let waiting : label Queue.t =
  Queue.create()

(* [discover label] marks [label] as discovered and inserts it into
   the waiting queue if was not already discovered. *)

let discover label =
  if not (Label.Set.mem label !discovered) then begin
    discovered := Label.Set.add label !discovered;
    Queue.add label waiting
  end

(* This simple-minded inliner performs conservative inlining only. More
   intelligent and more aggressive forms of inlining are performed by
   [specialize] and [commute_pushes] above. The following function defines
   our inlining policy. [label] and [target] are the labels of the source
   and destination blocks. *)

let can_inline label target =
  match (lookup program target).hint with
  | OnlyIfKnownState ->
      (* This hint disallows inlining. *)
      false
  | Always
  | IfCells _ ->
      (* These hints disallow inlining when [cautious] is true. *)
      not cautious
  | NoHint ->
      (* In cautious mode, inlining is allowed only if both the source
         block and the destination mode carry [NoHint]. *)
      not cautious ||
      (lookup program label).hint = NoHint

(* -------------------------------------------------------------------------- *)

(* The following functions assume that the in-degree of every reachable label
   has been computed (as shown above) and is stored in the table [degree]. *)

(* Transforming a block labeled [label]. We carry [label] around because it
   influences our inlining decisions; see [can_inline label target]. *)

let rec inline_block label block =
  match block with
  | IJump target ->
      (* If [target]'s in-degree is 1 and inlining is permitted, follow the
         indirection; otherwise, keep the [jump] instruction. Note that the
         [target]'s in-degree cannot be 0, since we have reached an edge
         that leads to [target]. *)
      if Label.Map.find target degree = 1
      && can_inline label target then
        (* The type information associated with the target block is lost:
           we do not have a construct for inserting a type annotation in
           the middle of a block. *)
        info "(inlg) Inlining %s (in-degree 1, cautious = %b)"
          (Label.export target) cautious
        (inline_block label (lookup program target).block)
      else begin
        discover target;
        IJump target
      end
  | ICaseTag _ ->
      (* Do not inline anything into a [casetag] construct. *)
      Block.successors discover block;
      block
  | _ ->
      Block.map (inline_block label) block

let inline_tblock label tblock =
  { tblock with block = inline_block label tblock.block }

(* -------------------------------------------------------------------------- *)

(* Transforming a control flow graph. *)

let program =
  (* Create an empty new control flow graph. *)
  let cfg = ref Label.Map.empty in
  (* Insert the entry labels into the queue. *)
  program.entry |> StringMap.iter begin fun _ label ->
    discover label
  end;
  (* Process the waiting labels. *)
  waiting |> Misc.qiter begin fun label ->
    assert (Label.Set.mem label !discovered);
    assert (not (Label.Map.mem label !cfg));
    let tblock = lookup program label in
    cfg := Label.Map.add label (inline_tblock label tblock) !cfg
  end;
  (* Done. *)
  { program with cfg = !cfg }

let () =
  Time.tick "StackLang: inlining"

end

let inline cautious program =
  let module I = Inline(struct
    let cautious = cautious
    let program = program
  end) in
  I.program
