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

(* -------------------------------------------------------------------------- *)

(* Constructors. *)

let def bs block =
  if Bindings.is_empty bs then
    block
  else
    match block with
    | IDef (bs', block) ->
        IDef (Bindings.seq bs bs', block)
    | _ ->
        IDef (bs, block)

let assign ps vs block =
  def (Bindings.assign ps vs) block

(* -------------------------------------------------------------------------- *)

(* Partial evaluation. *)

let rec select_branch tag branches =
  match branches with
  | (TagSingle tag', block) :: _ when tag = tag' ->
      block
  | _ :: branches ->
      select_branch tag branches
  | [] ->
      raise Not_found

(* -------------------------------------------------------------------------- *)

(* Visitors. *)

let map f block =
  match block with
  | IPush (vs, cell, block) ->
      IPush (vs, cell, f block)
  | IPop (ps, cell, block) ->
      IPop (ps, cell, f block)
  | IPeek (ps, cell, block) ->
      IPeek (ps, cell, f block)
  | IDef (bs, block) ->
      IDef (bs, f block)
  | IPrim (r, prim, block) ->
      IPrim (r, prim, f block)
  | ITrace (t, block) ->
      ITrace (t, f block)
  | IComment (content, block) ->
      IComment (content, f block)
  | IDead _
  | IStop _
  | IReturn _
  | IJump _ ->
      block
  | ICaseToken (r, branches, odefault) ->
      ICaseToken (
        r,
        List.map (branch_map f) branches,
        Option.map f odefault
      )
  | ICaseTag (r, branches) ->
      ICaseTag (r, List.map (branch_map f) branches)

let iter f block =
  match block with
  | IPush (_, _, block)
  | IPop (_, _, block)
  | IPeek (_, _, block)
  | IDef (_, block)
  | IPrim (_, _, block)
  | ITrace (_, block)
  | IComment (_, block)
      -> f block
  | IDead _
  | IStop _
  | IReturn _
  | IJump _
      -> ()
  | ICaseToken (_, branches, odefault) ->
      List.iter (branch_iter f) branches;
      Option.iter f odefault
  | ICaseTag (_, branches) ->
      List.iter (branch_iter f) branches

let rec jumps yield block =
  match block with
  | IPush (_, _, block)
  | IPop (_, _, block)
  | IPeek (_, _, block)
  | IDef (_, block)
  | IPrim (_, _, block)
  | ITrace (_, block)
  | IComment (_, block) ->
      jumps yield block
  | IDead _
  | IStop _
  | IReturn _ ->
      ()
  | IJump label ->
      yield false label
  | ICaseToken (_, branches, oblock) ->
      List.iter (branch_iter (jumps yield)) branches;
      Option.iter (jumps yield) oblock
  | ICaseTag (_, branches) ->
      (* We are now inside a CASEtag. *)
      let yield _incasetag target = yield true target in
      List.iter (branch_iter (jumps yield)) branches

let successors yield block =
  jumps (fun _incasetag label -> yield label) block
