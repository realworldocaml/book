(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The automaton is now frozen and will no longer be modified. It is
   time to dump a new description of it, if requested by the user. *)

let () =
  if Settings.dump_resolved then
    let module D = Dump.Make(Default) in
    D.dump (Settings.base ^ ".automaton.resolved")

let () =
  if Settings.automaton_graph then
    AutomatonGraph.print_automaton_graph()

(* Let [Interpret] handle the command line options [--interpret],
   [--interpret-error], [--compile-errors], [--compare-errors]. *)

let () =
  Interpret.run()

(* If [--list-errors] is set, produce a list of erroneous input sentences,
   then stop. *)

let () =
  if Settings.list_errors then begin
    let module X = struct
      (* Undocumented: if [--log-automaton 2] is set, be verbose. *)
      let verbose = Settings.logA >= 2
      (* For my own purposes, LRijkstra can print one line of statistics to a .csv file. *)
      let statistics = if false then Some "lr.csv" else None
      (* Fast algorithm can validate its results against classic one if
         [validate] is true. *)
      let validate = Settings.list_errors_algorithm = `Validate
    end in
    let (module Alg) = match Settings.list_errors_algorithm with
      | `Fast | `Validate ->
        (module LRijkstraFast.Run(X) : LRijkstra.REACHABILITY_ALGORITHM)
      | `Classic ->
        (module LRijkstraClassic.Run(X) : LRijkstra.REACHABILITY_ALGORITHM)
    in
    let module L = struct
      include LRijkstra.Run(X)(Alg)()
    end in
    exit 0
  end

(* If requested, generate a .cmly file. *)

let () =
  if Settings.cmly then
    Cmly_write.write (Settings.base ^ ".cmly")
