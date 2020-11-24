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

(* This script removes file names in # (line) directives. *)

(* This is used to textually compare the parsers produced by the stage 2
   and stage 3 executables. *)

let line_directive =
  Str.regexp {|^# \([0-9]+\) ".*"$|}

let process fn =
  let ic = open_in fn in
  try
    while true do
      let s = input_line ic in
      print_endline (Str.replace_first line_directive {|# \1 ""|} s)
    done
  with End_of_file ->
    close_in ic

let () =
  Arg.parse [] process ""
