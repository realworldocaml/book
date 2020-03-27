(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* Experimental code to produce a rendition of the automaton as a
   directed graph. This code is currently not plugged in. *)

open Grammar

module G = struct

  type vertex =
      Lr1.node

  let number s : string =
    Printf.sprintf "%02d" (Lr1.number s)

  let name s =
    Printf.sprintf "s%s" (number s)

  let successors (action: ?style:Dot.style -> label:string -> vertex -> unit) s : unit =
    Lr1.transitions s |> SymbolMap.iter (fun symbol s' ->
      action ~label:(Symbol.print symbol) s'
    )

  let iter (action: ?shape:Dot.shape -> ?style:Dot.style -> label:string -> vertex -> unit) : unit =
    Lr1.iter (fun s ->
      let has_reduction =
        match Invariant.has_default_reduction s with
        | Some _ ->
            true
        | None ->
            not (TerminalMap.is_empty (Lr1.reductions s))
      in
      let shape =
        if has_reduction then Dot.DoubleCircle else Dot.Circle
      in
      action ~shape ~label:(number s) s
    )

end

let filename =
  Printf.sprintf "%s.dot" Settings.base

let () =
  let c = open_out filename in
  let module P = Dot.Print(G) in
  P.print ~orientation:Dot.Portrait ~ratio:Dot.Compress ~size:(5.,5.) c;
  close_out c

