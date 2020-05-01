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

open Grammar

let print_dependency_graph() =

  (* Allocate. *)

  let forward : NonterminalSet.t NonterminalMap.t ref =
    ref NonterminalMap.empty
  in

  let successors nt =
    try
      NonterminalMap.find nt !forward
    with Not_found ->
      NonterminalSet.empty
  in

  (* Populate. *)

  Production.iter (fun prod ->
    let nt1 = Production.nt prod
    and rhs = Production.rhs prod in
    Array.iter (function
      | Symbol.T _ ->
          ()
      | Symbol.N nt2 ->
          forward := NonterminalMap.add
            nt1
            (NonterminalSet.add nt2 (successors nt1))
            !forward
    ) rhs
  );

  (* Print. *)

  let module P = Dot.Print (struct
    type vertex = Nonterminal.t
    let name nt =
      Printf.sprintf "nt%d" (Nonterminal.n2i nt)
    let successors (f : ?style:Dot.style -> label:string -> vertex -> unit) nt =
      NonterminalSet.iter (fun successor ->
        f ~label:"" successor
      ) (successors nt)
    let iter (f : ?shape:Dot.shape -> ?style:Dot.style -> label:string -> vertex -> unit) =
      Nonterminal.iter (fun nt ->
        f ~label:(Nonterminal.print false nt) nt
      )
  end) in
  let f = open_out (Settings.base ^ ".dot") in
  P.print f;
  close_out f

