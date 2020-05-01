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

(* ---------------------------------------------------------------------------- *)

(* A mechanism to turn all display (logging, warnings, errors) on and off. *)

let enabled =
  ref true

let enable () =
  enabled := true

let disable () =
  enabled := false

(* ---------------------------------------------------------------------------- *)

(* The new OCaml type inference protocol means that Menhir is called twice, first
   with [--infer-write-query], then with [--infer-read-reply]. This means that any
   information messages or warnings issued before OCaml type inference takes place
   are duplicated, unless we do something about it. To address this issue, when
   [--infer-read-reply] is set, we disable all output until the point where we
   read the inferred [.mli] file. Then, we enable it again and continue. *)

(* An alternative idea would be to disable all output when [--infer-write-query]
   is set. However, we would then have no output at all if this command fails. *)

let () =
  Settings.(match infer with
  | IMReadReply _ ->
      disable()
  | _ ->
      ()
  )

(* ---------------------------------------------------------------------------- *)

(* Logging and log levels. *)

let log kind verbosity msg =
  if kind >= verbosity && !enabled then
    Printf.fprintf stderr "%t%!" msg

let logG =
  log Settings.logG

let logA =
  log Settings.logA

let logC =
  log Settings.logC

(* ---------------------------------------------------------------------------- *)

(* Errors and warnings. *)

let print_positions f positions =
  List.iter (fun position ->
    fprintf f "%s:\n" (Positions.string_of_pos position)
  ) positions

let display continuation header positions format =
  let kprintf = if !enabled then Printf.kfprintf else Printf.ikfprintf in
  kprintf continuation stderr
    ("%a" ^^ header ^^ format ^^ "\n%!")
    print_positions positions

let error positions format =
  display
    (fun _ -> exit 1)
    "Error: "
    positions format

let warning positions format =
  display
    (fun _ -> ())
    "Warning: "
    positions format

let errorp v =
  error [ Positions.position v ]

(* ---------------------------------------------------------------------------- *)

(* Delayed error reports -- where multiple errors can be reported at once. *)

type category =
  bool ref

let new_category () =
  ref false

let signal category positions format =
  display
    (fun _ -> category := true)
    "Error: "
    positions format

let exit_if category =
  if !category then
    exit 1

(* ---------------------------------------------------------------------------- *)

(* Certain warnings about the grammar can optionally be treated as errors. *)

let grammatical_error =
  new_category()

let grammar_warning pos =
  if Settings.strict then signal grammatical_error pos else warning pos
