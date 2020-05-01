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

(* ---------------------------------------------------------------------------- *)

(* The identity of the current input file. *)

(* 2011/10/19: do not use [Filename.basename]. The [#] annotations that
   we insert in the [.ml] file must retain their full path. This does
   mean that the [#] annotations depend on how menhir is invoked -- e.g.
   [menhir foo/bar.mly] and [cd foo && menhir bar.mly] will produce
   different files. Nevertheless, this seems useful/reasonable. *)

(* This also influences the type error messages produced by [--infer]. *)

(* 2016/08/25: in principle, the order in which file names appear on the
   command line (when there are several of them) does not matter. It is
   however used in [BasicPrinter] (see the problem description
   there). For this reason, we define a type [input_file] which includes
   the file's name as well as its index on the command line. *)

type input_file = {
  input_file_name: string;
  input_file_index: int
}

let builtin_input_file = {
  input_file_name = "<builtin>";
  input_file_index = -1
}

let dummy_input_file = {
  input_file_name = "<dummy>";
  input_file_index = 0
}

let same_input_file file1 file2 =
  file1.input_file_index = file2.input_file_index
    (* could also use physical equality [file1 == file2] *)

let compare_input_files file1 file2 =
  Generic.compare file1.input_file_index file2.input_file_index
    (* Ideally, this function should NOT be used, as it reflects the
       order of the input files on the command line. As of 2016/08/25,
       it is used by [BasicPrinter], for lack of a better
       solution. *)

let current_input_file =
  ref dummy_input_file

(* This declares that a new file is being processed. *)
let new_input_file name : unit =
  current_input_file := {
    input_file_name = name;
    input_file_index = !current_input_file.input_file_index + 1
  }

let get_input_file () : input_file =
  assert (!current_input_file != dummy_input_file);
  !current_input_file

let get_input_file_name () : string =
  (get_input_file()).input_file_name

(* ---------------------------------------------------------------------------- *)

(* The contents of the current input file. *)

let get_initialized_ref ref =
  match !ref with
  | None ->
      assert false
  | Some contents ->
      contents

let file_contents =
  ref (None : string option)

let get_file_contents () =
  get_initialized_ref file_contents

let with_file_contents contents f =
  file_contents := Some contents;
  let result = f() in
  file_contents := None; (* avoid memory leak *)
  result

open Lexing

let chunk (pos1, pos2) =
  let ofs1 = pos1.pos_cnum
  and ofs2 = pos2.pos_cnum in
  let contents = get_file_contents() in
  let len = ofs2 - ofs1 in
  String.sub contents ofs1 len
