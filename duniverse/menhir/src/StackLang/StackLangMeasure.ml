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

(* -------------------------------------------------------------------------- *)

(* Measuring the size of a StackLang program. *)

type measure = {
  mutable push: int;
  mutable pop: int;
  mutable peek: int;
  mutable def: int;
  mutable prim: int;
  mutable trace: int;
  mutable dead: int;
  mutable stop: int;
  mutable return: int;
  mutable jump: int;
  mutable casetoken: int;
  mutable casetag: int;
  mutable total: int;
}

let zero () = {
  push = 0;
  pop = 0;
  peek = 0;
  def = 0;
  prim = 0;
  trace = 0;
  dead = 0;
  stop = 0;
  return = 0;
  jump = 0;
  casetoken = 0;
  casetag = 0;
  total = 0;
}

let print f m =
  let pad i = Misc.padded_index m.total i in
  fprintf f "#### count PUSH    %s\n" (pad m.push);
  fprintf f "#### count POP     %s\n" (pad m.pop);
  fprintf f "#### count PEEK    %s\n" (pad m.peek);
  fprintf f "#### count DEF     %s\n" (pad m.def);
  fprintf f "#### count PRIM    %s\n" (pad m.prim);
  fprintf f "#### count TRAC    %s\n" (pad m.trace);
  fprintf f "#### count DEAD    %s\n" (pad m.dead);
  fprintf f "#### count STOP    %s\n" (pad m.stop);
  fprintf f "#### count RET     %s\n" (pad m.return);
  fprintf f "#### count JUMP    %s\n" (pad m.jump);
  fprintf f "#### count CASEtok %s\n" (pad m.casetoken);
  fprintf f "#### count CASEtag %s\n" (pad m.casetag);
  fprintf f "#### total         %s\n" (pad m.total);
  ()

let adjust_total m =
  m.total <-
    m.push +
    m.pop +
    m.peek +
    m.def +
    m.prim +
    m.trace +
    m.dead +
    m.stop +
    m.return +
    m.jump +
    m.casetoken +
    m.casetag +
    0

let rec measure_block m block =
  match block with
  | IPush (_, _, block) ->
      m.push <- m.push + 1;
      measure_block m block
  | IPop (_, _, block) ->
      m.pop <- m.pop + 1;
      measure_block m block
  | IPeek (_, _, block) ->
      m.peek <- m.peek + 1;
      measure_block m block
  | IDef (_, block) ->
      m.def <- m.def + 1;
      measure_block m block
  | IPrim (_, _, block) ->
      m.prim <- m.prim + 1;
      measure_block m block
  | ITrace (_, block) ->
      m.trace <- m.trace + 1;
      measure_block m block
  | IComment (_, block) ->
      measure_block m block
  | IDead _ ->
      m.dead <- m.dead + 1
  | IStop _ ->
      m.stop <- m.stop + 1
  | IReturn _ ->
      m.return <- m.return + 1
  | IJump _ ->
      m.jump <- m.jump + 1
  | ICaseToken (_, branches, odefault) ->
      m.casetoken <- m.casetoken + 1;
      List.iter (branch_iter (measure_block m)) branches;
      Option.iter (measure_block m) odefault
  | ICaseTag (_, branches) ->
      m.casetag <- m.casetag + 1;
      List.iter (branch_iter (measure_block m)) branches

and measure_tblock measure { block } =
  measure_block measure block

let measure program =
  let m = zero () in
  Label.Map.iter (fun _ -> measure_tblock m) program.cfg;
  adjust_total m;
  m
