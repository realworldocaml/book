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

(* Construct the code, using either the table-based or the code-based
   back-end, and pass it on to the printer. (This continuation-passing
   style is imposed by the fact that there is no conditional in ocaml's
   module language.) *)

let () =
  if Settings.coq then
    let module B = CoqBackend.Run (struct end) in
    let filename = Settings.base ^ ".v" in
    let f = open_out filename in
    B.write_all f;
    exit 0
  else
    if Settings.table then
      let module B = TableBackend.Run (struct end) in
      write B.program
    else
      let module B = CodeBackend.Run (struct end) in
      write (CodeInliner.inline B.program)

(* Write the interface file. *)

let () =
  Interface.write Front.grammar ()

let () =
  Time.tick "Printing"
