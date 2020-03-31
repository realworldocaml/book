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

open Ocamlbuild_plugin
open Command

(* This file contains ocamlbuild rules for dealing with Menhir's [.messages]
   files. *)

(* ---------------------------------------------------------------------------- *)

(* This rule generates an .ml file [target] from an .mly file [grammar] and a
   .messages file [messages]. *)

(* If the name of a witness file is passed, it is made an additional
   dependency. This triggers a separate rule (see below) which performs a
   completeness check, that is, which checks that the .messages file lists
   every possible syntax error. *)

let compile_errors grammar messages (witness : string list) target =
  rule
    "menhir/compile_errors"
    ~prod:target
    ~deps:([ grammar; messages ] @ witness)
    (fun env _ ->
      let grammar = env grammar in
      let tags = tags_of_pathname grammar ++ "ocaml" ++ "menhir" in
      Cmd(S[
        !Options.ocamlyacc; (* menhir *)
        T tags;
        P grammar;
        A "--compile-errors"; P (env messages);
        Sh ">"; Px (env target);
      ]))

(* A generic version of the above rule, with uniform naming. *)

let generic_compile_errors (check_completeness : bool) =
  compile_errors
    (* sources: *)
    "%.mly" "%Messages.messages"
    (* if present, this dependency forces a completeness check: *)
    (if check_completeness then [ "%Messages.witness" ] else [])
    (* target: *)
    "%Messages.ml"

(* ---------------------------------------------------------------------------- *)

(* This rule generates a .messages file [messages] from an .mly file
   [grammar]. *)

let list_errors grammar messages =
  rule
    "produce a list of messages"
    ~prod:messages
    ~dep:grammar
    (fun env _ ->
      let grammar = env grammar in
      let tags = tags_of_pathname grammar ++ "ocaml" ++ "menhir" in
      Cmd(S[
        !Options.ocamlyacc; (* menhir *)
        T tags;
        P grammar;
        A "--list-errors";
        Sh ">"; Px (env messages);
      ]))

(* ---------------------------------------------------------------------------- *)

(* This rule compares the .messages files [messages1] and [messages2]. This is
   used to ensure complete coverage, i.e., check that every possible error is
   covered. The file [witness] is used as a witness that the comparison has
   been carried out. *)

let compare_errors grammar messages1 messages2 witness =
  rule
    "compare two lists of messages"
    ~stamp:witness
    ~deps:[ grammar; messages1; messages2 ]
    (fun env _ ->
      let grammar = env grammar in
      let tags = tags_of_pathname grammar ++ "ocaml" ++ "menhir" in
      Cmd(S[
        !Options.ocamlyacc; (* menhir *)
        T tags;
        P grammar;
        A "--compare-errors"; P (env messages1);
        A "--compare-errors"; P (env messages2);
      ]))

(* ---------------------------------------------------------------------------- *)

(* This rule combines the above two rules and makes sure that the [messages]
   file is complete, i.e., covers all possible errors. This rule creates a
   witness file. *)

let completeness_check grammar messages witness =
  (* We need a name for a temporary [.messages] file, which we produce,
     and which lists all possible errors. *)
  let complete_messages = grammar ^ ".auto.messages" in
  (* Use the above two rules. *)
  list_errors grammar complete_messages;
  compare_errors grammar complete_messages messages witness

(* A generic version of the above rule, with uniform naming. *)

let generic_completeness_check () =
  completeness_check
    (* sources: *)
    "%.mly" "%Messages.messages"
    (* target: *)
    "%Messages.witness"

(* ---------------------------------------------------------------------------- *)

(* That's it! *)

(* In a concrete project, the above rules can be used simply by saying:

     generic_compile_errors true;
     generic_completeness_check()

 *)
