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

(* Driver for the back-end. *)

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

(* If [--list-errors] is set, produce a list of erroneous input sentences, then stop. *)

let () =
  if Settings.list_errors then begin
    let module L = LRijkstra.Run(struct
      (* Undocumented: if [--log-automaton 2] is set, be verbose. *)
      let verbose = Settings.logA >= 2
      (* For my own purposes, LRijkstra can print one line of statistics to a .csv file. *)
      let statistics = if false then Some "lr.csv" else None
    end) in
    exit 0
  end

(* Define an .ml file writer . *)

let write program =
  let module P = Printer.Make (struct
    let filename = Settings.base ^ ".ml"
    let f = open_out filename
    let locate_stretches =
      (* 2017/05/09: always include line number directives in generated .ml
         files. Indeed, they affect the semantics of [assert] instructions
         in the semantic actions. *)
      (* 2011/10/19: do not use [Filename.basename]. The line number
         directives that we insert in the [.ml] file must retain their full
         path. This does mean that the line number directives depend on how
         menhir is invoked -- e.g. [menhir foo/bar.mly] and [cd foo && menhir
         bar.mly] will produce different files. Nevertheless, this seems
         useful/reasonable. *)
      Some filename
  end) in
  P.program program

(* If requested, generate a .cmly file. *)

let () =
  if Settings.cmly then
    Cmly_write.write (Settings.base ^ ".cmly")

(* Construct and print the code using an appropriate back-end. *)

let () =
  if Settings.table then begin
    let module B = TableBackend.Run (struct end) in
    write B.program;
    Interface.write Front.grammar ()
  end
  else if Settings.coq then begin
    let module B = CoqBackend.Run (struct end) in
    let filename = Settings.base ^ ".v" in
    let f = open_out filename in
    B.write_all f
  end
  else begin
    let module B = CodeBackend.Run (struct end) in
    write (CodeInliner.inline B.program);
    Interface.write Front.grammar ()
  end

let () =
  Time.tick "Printing"
