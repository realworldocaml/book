(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Dot
open StackLang

(* [uniq] transforms an arbitrary [iter] function into one that produces each
   element at most once. *)

let uniq iter =
  let encountered = ref Label.Set.empty in
  fun yield ->
    iter begin fun label ->
      if not (Label.Set.mem label !encountered) then begin
        encountered := Label.Set.add label !encountered;
        yield label
      end
    end

let print program =
  let module P = Dot.Print (struct

    type vertex =
      label

    let name label =
      Label.export label

    let successors (f : ?style:style -> label:string -> vertex -> unit) label =
      (lookup program label).block
      |> uniq Block.successors (fun target -> f ~label:"" target)

    let iter
      (f : ?shape:shape -> ?style:style -> label:string -> vertex -> unit) =
      program.cfg
      |> Label.Map.iter begin fun label _block ->
        f ~shape:Box ~label:(name label) label
      end

  end) in
  let f = open_out (Settings.base ^ ".dot") in
  P.print f;
  close_out f
