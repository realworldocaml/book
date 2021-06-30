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

open Printf
open Grammar

module P = Dot.Print (struct

  type vertex = Lr1.node

  let name node =
    sprintf "s%d" (Lr1.number node)

  let successors (f : ?style:Dot.style -> label:string -> vertex -> unit) source =
    SymbolMap.iter (fun symbol target ->
      let label = Symbol.print symbol in
      f ~label target
    ) (Lr1.transitions source)

  let iter (f : ?shape:Dot.shape -> ?style:Dot.style -> label:string -> vertex -> unit) =
    Lr1.iter (fun node ->
      let label = sprintf "%d" (Lr1.number node) in
      f ~label node
    )

end)

let print_automaton_graph() =
  let f = open_out (Settings.base ^ ".dot") in
  P.print f;
  close_out f
