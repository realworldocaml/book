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

let empty, singleton, mem, (+), (-) =
  Reg.Set.(empty, singleton, mem, union, diff)

let maybe = function Some s -> s | None -> empty
let union = List.fold_left ( + ) empty

module Variable =
  Fix.Glue.HashTablesAsImperativeMaps(Label)

(* -------------------------------------------------------------------------- *)

(* The set of registers defined by a [tokpat]. *)

let defined_tokpat = function
  | TokSingle (_, r) ->
      singleton r
  | TokMultiple _ ->
      empty

(* -------------------------------------------------------------------------- *)

(* [needed block query] computes the registers needed by the block [block]. At
   each jump instruction, the function [query] is used as an oracle, which
   tells us which registers are needed by the destination block. *)

let needed block (query : label -> registers) : registers =
  let rec needed block =
    match block with
    | IPush (vs, _, block) ->
        let used = needed block in
        Value.registers vs + used
    | IPop (ps, _, block)
    | IPeek (ps, _, block) ->
        let used = needed block in
        used - Pattern.registers ps
    | IDef (bs, block) ->
        (* If the register [r] is not needed by the block [block], then the
           instruction [DEF r <- v] can be removed. Then, the registers that
           appear in the value [v] need not be considered used. *)
        let used = needed block in
        let bs = Bindings.restrict used bs in
        Bindings.codomain bs + (used - Bindings.domain bs)
    | IPrim (p, prim, block) ->
        (* Similarly, if [prim] is pure and if the register defined by [p] is
           not needed by [block], then this primitive instruction can be
           removed, in which case the registers that appear in it need not be
           considered used. *)
        let used = needed block in
        let p = Pattern.restrict1 used p in
        if Primitive.pure prim && p = PWildcard then
          used
        else
          Primitive.registers prim + (used - Pattern.registers [p])
    | ITrace (_, block) ->
        needed block
    | IComment (_, block) ->
        needed block
    | IDead _
    | IStop _ ->
        empty
    | IReturn (_, v) ->
        Value.registers [v]
    | IJump label ->
        query label
    | ICaseToken (r, branches, odefault) ->
        let seed = singleton r + needed_odefault odefault in
        List.fold_left needed_token_branch seed branches
    | ICaseTag (r, branches) ->
        let seed = singleton r in
        List.fold_left needed_tag_branch seed branches
  and needed_token_branch accu (tokpat, block) =
    accu + (needed block - defined_tokpat tokpat)
  and needed_odefault odefault =
    match odefault with None -> empty | Some block -> needed block
  and needed_tag_branch accu (_tagpat, block) =
    accu + needed block
  in
  needed block

(* -------------------------------------------------------------------------- *)

(* [prune block query] computes the registers needed by the block [block],
   exactly like [needed] above, and, at the same time, transforms the block
   by removing DEF and (pure) PRIM instructions whose destination register
   is dead. *)

(* Because instructions inside a block do not have addresses, we cannot easily
   construct a table that maps each instruction to the set of used registers.
   Thus, we must recompute the set of needed registers, bottom-up, while we
   transform a block. This creates a redundancy between [needed] and [prune],
   which seems tolerable. Removing [needed] and keeping just [prune] would
   cause us to perform useless transformation work during the fixed point
   computation. *)

let prune block (query : label -> registers) : registers * block =
  let rec prune block =
    match block with
    | IPush (vs, cell, block) ->
        let used, block = prune block in
        Value.registers vs + used,
        IPush (vs, cell, block)
    | IPop (ps, cell, block) ->
        let used, block = prune block in
        (* We also prune POPs by replacing unused registers with wildcards. *)
        let ps = Pattern.restrict used ps in
        used - Pattern.registers ps,
        IPop (ps, cell, block)
    | IPeek (ps, cell, block) ->
        let used, block = prune block in
        let ps = Pattern.restrict used ps in
        (* If all of the pruned patterns [ps] are wildcards, then the PEEK
           instruction could be removed. We do not bother; we expect PEEKs
           to be rare anyway, and a PEEK whose destination registers are dead
           should in principle never appear. *)
        used - Pattern.registers ps,
        IPeek (ps, cell, block)
    | IDef (bs, block) ->
        let used, block = prune block in
        (* This is where we prune unused definitions. *)
        let bs = Bindings.restrict used bs in
        Bindings.codomain bs + (used - Bindings.domain bs),
        Block.def bs block
    | IPrim (p, prim, block) ->
        let used, block = prune block in
        let p = Pattern.restrict1 used p in
        if Primitive.pure prim && p = PWildcard then
          used, block
        else
          Primitive.registers prim + (used - Pattern.registers [p]),
          IPrim (p, prim, block)
    | ITrace (trace, block) ->
        let used, block = prune block in
        used,
        ITrace (trace, block)
    | IComment (comment, block) ->
        let used, block = prune block in
        used,
        IComment (comment, block)
    | IDead phase ->
        empty,
        IDead phase
    | IStop s ->
        empty,
        IStop s
    | IReturn (nt, v) ->
        Value.registers [v],
        IReturn (nt, v)
    | IJump label ->
        query label,
        IJump label
    | ICaseToken (r, branches, odefault) ->
        let used1, branches = List.(split (map prune_casetok_branch branches))
        and used2, odefault = Option.(split (map prune odefault)) in
        singleton r + union used1 + maybe used2,
        ICaseToken (r, branches, odefault)
    | ICaseTag (r, branches) ->
        let used, branches = List.(split (map prune_casetag_branch branches)) in
        singleton r + union used,
        ICaseTag (r, branches)

  and prune_casetok_branch (tokpat, block) =
    let used, block = prune block in
    match tokpat with
    | TokSingle (tok, r) ->
        (* If [r] is not used, then we can turn it into a wildcard pattern.
           This requires turning [TokSingle] into [TokMultiple], which is a
           bit weird, but works. *)
        if mem r used then
          used - singleton r,
          (TokSingle (tok, r), block)
        else
          used,
          (TokMultiple (Grammar.TerminalSet.singleton tok), block)
    | TokMultiple toks ->
        used,
        (TokMultiple toks, block)

  and prune_casetag_branch (tags, block) =
    let used, block = prune block in
    used,
    (tags, block)

  in
  prune block

(* -------------------------------------------------------------------------- *)

module Property =
  Fix.Prop.Set(Reg.Set)

module F =
  Fix.Make(Variable)(Property)

let update program =

  (* Compute the fixed point of [needed]. *)
  let needed : label -> registers =
    F.lfp (fun label -> needed (lookup program label).block)
  in
  (* Force the fixed point, to obtain correct timing data. *)
  Label.Map.iter (fun label _ -> ignore (needed label)) program.cfg;
  Time.tick "StackLang: computing needed registers";

  (* Transform every block by pruning its dead DEF and PRIM instructions
     and by updating its [needed] field. *)
  let cfg =
    Label.Map.mapi (fun _label tblock ->
      let needed, block = prune tblock.block needed in
      { tblock with needed; block }
    ) program.cfg
  in
  Time.tick "StackLang: updating needed registers";

  (* Done. *)
  { program with cfg }
