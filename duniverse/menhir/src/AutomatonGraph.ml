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
open Grammar

module P = Dot.Print (struct

  type vertex = Lr1.node

  let name node =
    sprintf "s%d" (Lr1.number node)

  let successors (f : ?style:Dot.style -> label:string -> vertex -> unit) source =
    SymbolMap.iter (fun symbol target ->
      (* Shift transitions are solid; goto transitions are dashed. *)
      let style = if Symbol.is_terminal symbol then Dot.Solid else Dot.Dashed in
      (* Edges are unlabeled, because it is more economical to
         place the edge label in the target node. Some whitespace
         is artificially placed in the label to obtain better
         placement of nodes and edges. *)
      let label = "        " in
      f ~style ~label target
    ) (Lr1.transitions source)

  let iter (f : ?shape:Dot.shape -> ?style:Dot.style -> label:string -> vertex -> unit) =
    Lr1.iter (fun node ->
      let label =
        match Lr1.incoming_symbol node with
        | None ->
            sprintf "%d" (Lr1.number node)
        | Some nt ->
            (* The incoming symbol and the node number,
               stacked vertically. *)
            sprintf "{%s|%d}"
              (Symbol.print false nt)
              (Lr1.number node)
      in
      f ~shape:Dot.Record ~label node
    )

end)

let print_automaton_graph() =
  let f = open_out (Settings.base ^ ".dot") in
  P.print f;
  close_out f
