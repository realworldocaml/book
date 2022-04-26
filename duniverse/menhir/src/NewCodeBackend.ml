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
open StackLangMeasure

(* -------------------------------------------------------------------------- *)

(* [check] checks that the program is well-formed and well-typed, and marks
   dead branches in [casetag] instructions.

   The well-formedness check is performed only in development builds; in
   release builds, it is skipped. The well-typedness check is performed
   always; skipping it in release builds could be problematic, since it
   is not the identity (it can remove dead branches). *)

let check program =
  if Profile.release then
    program
    |> StackLangCheck.wt
  else
    program
    |> StackLangCheck.wf
    |> StackLangCheck.wt

(* -------------------------------------------------------------------------- *)

(* This is the driver of the new code back-end. *)

module Run () = struct

  (* [EmitStackLang] produces a StackLang program [program]. *)

  (* By construction, every block in this program is reachable,
     so there is no need to remove unreachable blocks right away. *)

  include EmitStackLang.Run()

  (* Check. *)

  let program =
    program |> check

  (* At -O 2, perform specialization. *)

  let program =
    if Settings.optimization_level >= 2 then
      program
      |> StackLangTransform.specialize
      |> StackLangTransform.remove_unreachable_blocks
      |> check
    else
      program

  (* At -O 1, inline routines whose in-degree is 1, provided they carry no
     inlining hint. This typically allows us to inline a [run] routine into a
     [run] routine, and has a favorable effect on the movement of PUSH
     instructions (performed next), as it removes JUMPs, which prevent the
     movement of PUSH instructions. The reason why we must be cautious here is
     that we must not allow inlining to create large [reduce] or [goto]
     routines; indeed, these routines are inlined (duplicated) while moving
     PUSH instructions. *)

  let program =
    if Settings.optimization_level >= 1 && Settings.code_inlining then
      let cautious = true in
      program
      |> StackLangTransform.inline cautious
      |> check
    else
      program

  (* At -O 1, move PUSH instructions forward, in the hope that they meet POP
     instructions and cancel out. *)

  let program =
    if Settings.optimization_level >= 1 then
      program
      |> StackLangTransform.commute_pushes
      |> check
    else
      program

  (* Simplify [ICaseToken] instructions where possible. This transformation
     improves performance by eliminating some redundant [ICaseToken]
     instructions. It also clarifies the code by allowing some instructions
     and blocks to be recognized as dead. *)

  let program =
    StackLangTokenAnalysis.transform program

  (* Inline functions whose in-degree is 1, insofar as possible. *)

  let program =
    if Settings.code_inlining then
      let cautious = false in
      program
      |> StackLangTransform.inline cautious
      |> check
    else
      program

  (* Clean up redundant definitions that might be present. E.g.,
     specialization can leave a few redundant definitions of the
     [state] register. *)

  let program =
    NeededRegisters.update program

  (* If requested, dump the StackLang program to %.stacklang
     and a static instruction count to %.scount. *)

  let m =
    lazy (measure program)

  let () =
    if Settings.stacklang_dump then begin
      let file = open_out (Settings.base ^ ".stacklang") in
      StackLangPrinter.ToChannel.program file program;
      close_out file;
      let file = open_out (Settings.base ^ ".scount") in
      StackLangMeasure.print file (Lazy.force m);
      close_out file
    end

  (* If requested, dump the call graph of the StackLang program. *)

  let () =
    if Settings.stacklang_graph then
      StackLangGraph.print program

  (* If requested, test the StackLang program. *)

  let () =
     if Settings.stacklang_test then
       StackLangTester.test program

  (* If requested, be verbose. *)

  let () =
    Error.logC 1 begin fun f ->
      let m = Lazy.force m in
      fprintf f "The StackLang code contains %d instructions in %d blocks.\n"
        m.total (StackLang.cardinal program)
    end

  (* Compile the StackLang program down to IL. *)

  let program =
    LeaveStackLang.compile program

end
