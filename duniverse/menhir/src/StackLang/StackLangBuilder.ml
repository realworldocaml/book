(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLang

let identity x = x

let compose f g x = f (g x)

(* -------------------------------------------------------------------------- *)

(* The internal state of our imperative construction machinery. Either it is
   idle; or a block has been opened but not completed, in which case we have
   a partial block at hand, a block with a hole; or a block has been completed,
   in which case we are waiting for someone to come and get it. *)

type state =
  | Idle
  | Open of (block -> block)
  | Closed of block

let current : state ref =
  ref Idle

(* [exec body] saves our current state and moves to a new state where
   an empty block has just been opened. Then, it executes [body()],
   which is expected to grow and close this block, so, when [body]
   returns, we expect the current state to be [Closed block]. We
   then restore the initial state and return [block]. *)

let exec (body : unit -> unit) : block =
  let saved = !current in
  current := Open identity;
  body ();
  match !current with
  | Idle ->
      (* This cannot happen, because there is no path from [Open _] to
         [Idle] in the transition diagram. *)
      assert false
  | Open _ ->
      (* The user has misused our API: a block has been opened and has not
         been properly ended by calling [stop], [return], [jump], or a case
         analysis construction. *)
      assert false
  | Closed block ->
      current := saved;
      block

(* -------------------------------------------------------------------------- *)

(* More internal state. These components of the state are reset at the
   beginning of every typed block, that is, at the beginning of every
   labeled block. [current_block_type] keeps track of type information, and
   is updated by [set_block_type]. [current_hint] is updated by [set_hint].
   [current_spec] is updated by [set_spec]. *)

let current_block_type : block_type option ref =
  ref None

let set_block_type block_type =
  assert (!current_block_type = None);
  current_block_type := Some block_type

let current_hint : hint ref =
  ref NoHint

let set_hint hint =
  assert (!current_hint = NoHint);
  current_hint := hint

let current_spec : spec ref =
  ref SpecDisallowed

let set_spec spec =
  assert (!current_spec = SpecDisallowed);
  current_spec := spec

(* This wrapper around [exec] handles these extra state components
   and returns a [typed_block] instead of just a block. *)

let typed_exec (body : unit -> unit) : typed_block =
  current_block_type := None;
  current_hint := NoHint;
  current_spec := SpecDisallowed;
  let block = exec body in
  match !current_block_type with
  | None ->
      (* The user has misused our API: [set_block_type] must be called. *)
      assert false
  | Some block_type ->
      (* [needed] is inferred later on; see [NeededRegisters.update] below. *)
      let needed = Reg.Set.empty
      and hint = !current_hint
      and spec = !current_spec in
      { block; block_type; needed; hint; spec }

(* -------------------------------------------------------------------------- *)

(* [extend g] extends the currently open block with a new part [g]. *)

let extend (g : block -> block) =
  match !current with
  | Idle ->
      (* The user has misused our API: an instruction generation operation
         has been called while no block was in construction. *)
      assert false
  | Open f ->
      (* The current block is extended with [g] and remains open. *)
      current := Open (compose f g)
  | Closed _ ->
      (* The user has misused our API: an instruction generation operation
         has been called, but the block under construction has already been
         closed. *)
      assert false

(* -------------------------------------------------------------------------- *)

(* [close i] closes the currently open block with the instruction [i]. *)

let close i =
  match !current with
  | Idle ->
      (* The user has misused our API: an instruction generation operation
         has been called while no block was in construction. *)
      assert false
  | Open f ->
      (* The instruction [i] is the final instruction in this block, which
         becomes closed. *)
      current := Closed (f i)
  | Closed _ ->
      (* The user has misused our API: an instruction generation operation
         has been called, but the block under construction has already been
         ended by calling [stop], [return], [jump], or a case analysis
         construction. *)
      assert false

(* -------------------------------------------------------------------------- *)

(* Here come the public construction functions. *)

let push vs cell =
  assert (vs <> []);
  extend (fun block -> IPush (vs, cell, block))

let pop ps cell =
  assert (ps <> []);
  extend (fun block -> IPop (ps, cell, block))

let peek ps cell =
  assert (ps <> []);
  extend (fun block -> IPeek (ps, cell, block))

let def ps vs =
  extend (fun block -> Block.assign ps vs block)

let move dst src =
  def [PReg dst] [VReg src]

let prim r prim =
  extend (fun block -> IPrim (PReg r, prim, block))

let trace t =
  extend (fun block -> ITrace (t, block))

let comment s =
  extend (fun block -> IComment (s, block))

let stop s =
  close (IStop s)

let return nt v =
  close (IReturn (nt, v))

let jump label =
  close (IJump label)

let case_token r cases =
  (* Create a growing list of branches. *)
  let branches, default = ref [], ref None in
  (* Define a function that creates a new branch. *)
  let def_branch pat body = branches := (pat, exec body) :: !branches in
  (* Define a function that creates the default branch. *)
  let def_default body = default := Some (exec body) in
  (* Give the user access to these functions. *)
  cases def_branch def_default;
  (* Retrieve the branches that have been constructed. *)
  let branches, default = (List.rev !branches, !default) in
  (* If the branches cover all terminal symbols, then there is no need
     for a default branch; drop it. *)
  let default = if exhaustive branches then None else default in
  (* Close this block with an [ICaseToken] instruction. *)
  close (ICaseToken (r, branches, default))

let case_tag r cases =
  (* Create a growing list of branches. *)
  let branches = ref [] in
  (* Define a function that creates a new branch. *)
  let def_branch pat body = branches := (pat, exec body) :: !branches in
  (* Give the user access to this function. *)
  cases def_branch;
  (* Retrieve the branches that have been constructed. *)
  let branches = List.rev !branches in
  (* Close the block. *)
  match branches with
  | [] ->
      (* In principle, we can generate whatever code we want here, since it
         must be dead. Using [IDead `Dynamic] seems appropriate. If we used
         [IDead `Static], the type-checker may not be able to prove that
         this code is dead. *)
      close (IDead `Dynamic);
      false
  | [ _pat, block ] ->
      (* There is a single branch, so no case analysis is required. *)
      close block;
      false
  | _ :: _ :: _ ->
      (* There are several branches, so a case analysis is required.
         Disjunction patterns must be expanded away. *)
      let expand (tags, body) =
        List.map (fun tag -> (TagSingle (Lazy.force tag), body)) tags
      in
      let branches = List.flatten (List.map expand branches) in
      close (ICaseTag (r, branches));
      true

(* -------------------------------------------------------------------------- *)

(* The module [Build]. *)

module Build (A : sig
  type address
  val print : address -> label
  val iter : (address -> unit) -> unit
  val code : address -> unit
  val entry : address StringMap.t
  val states : states
end) = struct
  open A

  (* The type [address] is not [label]. For convenience, we let the user pick
     her own (structured) type of addresses. *)

  (* Build a reverse mapping of labels to addresses. This is required because
     the [jump] function that we expose to the user takes a label as an
     argument, not an address. Exposing a form of [jump] that expects an
     address would require another layer of functorisation. *)

  let unprint : label -> address =
    let accu = ref Label.Map.empty in
    iter begin fun addr ->
      let label = print addr in
      accu := Label.Map.add label addr !accu
    end;
    let reverse_mapping = !accu in
    fun label ->
      try
        Label.Map.find label reverse_mapping
      with Not_found ->
        Printf.eprintf "Argh! Cannot unprint %s.\n%!" (Label.export label);
        assert false

  (* Wrap the user function [code] in a suitable way. *)

  let code (addr : address) : typed_block =
    typed_exec (fun () -> code addr)

  (* Construct the control flow graph. We could construct every block directly
     by using [A.iter], but that could lead us to generate unreachable blocks.
     We prefer to avoid this by construction; we generate a block only after
     we have determined that it is reachable. *)

  let cfg =
    (* The control flow graph under construction. *)
    let cfg = ref Label.Map.empty in
    (* A waiting queue of addresses for which we must generate code. *)
    let waiting = Queue.create() in
    (* Insert the entry points into the queue. *)
    entry |> StringMap.iter begin fun _public_name addr ->
      Queue.add addr waiting
    end;
    (* Process the waiting queue. *)
    waiting |> Misc.qiter begin fun (addr : address) ->
      let label = print addr in
      if not (Label.Map.mem label !cfg) then begin
        (* Generate code for this address. *)
        let tblock = code addr in
        (* Add it to the control flow graph. *)
        cfg := Label.Map.add label tblock !cfg;
        (* Examine the successors of this block and insert them into the
           queue, if they have not been processed already. (I am lazy, so
           I allow a label to be inserted several times into the queue.) *)
        tblock.block |> Block.successors begin fun (target : label) ->
          if not (Label.Map.mem target !cfg) then
            Queue.add (unprint target) waiting
        end
      end
    end;
    !cfg

  (* Construct the entry points. *)
  let entry =
    StringMap.map print entry

  let () =
    Time.tick "StackLang: producing code"

  (* Construct a complete program, and annotate every block with the set
     of the registers that it uses. *)
  let program =
    NeededRegisters.update { cfg; entry; states }

end (* Build *)
