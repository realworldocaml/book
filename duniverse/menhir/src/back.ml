(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* Driver for the back-end. *)

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

(* Construct and print the code using an appropriate back-end. *)

let () =
  match Settings.backend with
  | `ReferenceInterpreter ->
      (* This case is handled in Interpret. *)
      assert false
  | `TableBackend ->
      let module B = TableBackend.Run (struct end) in
      write B.program;
      Interface.write Front.grammar ()
  | `CoqBackend ->
      let module B = CoqBackend.Run (struct end) in
      let filename = Settings.base ^ ".v" in
      let f = open_out filename in
      B.write_all f
  | `OldCodeBackend ->
      let module B = CodeBackend.Run () in
      write (CodeInliner.inline B.program);
      Interface.write Front.grammar ()
  | `NewCodeBackend ->
      let module B = NewCodeBackend.Run () in
      write B.program;
      Interface.write Front.grammar ()

let () =
  Time.tick "Printing"
