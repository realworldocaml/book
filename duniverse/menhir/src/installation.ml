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

(* [normalize] normalizes a file name by recognizing . and .. and treating
   them in an appropriate manner. *)

let rec normalize fn =
  let dir = Filename.dirname fn in
  let base = Filename.basename fn in
  if dir = fn then
    (* This could be the case e.g. if [fn] is "/". *)
    dir
  else if base = Filename.current_dir_name then
    (* We have "." as the basename, that is, at the end. Remove it
       and continue. *)
    normalize dir
  else if base = Filename.parent_dir_name then
    (* We have ".." as the basename, that is, at the end. Normalize the rest.
       Once done, chop off the basename, thus moving to the parent directory. *)
    Filename.dirname (normalize dir)
  else
    (* We have a normal basename. Normalize the rest. *)
    Filename.concat (normalize dir) base

(* The directory where (we think) MenhirLib is installed. *)

(* This directory used to be hard-coded in the [menhir] executable. We now
   adopt a different strategy. We fetch the name of the [menhir] executable,
   and hope that it is of the form [.../bin/menhir]. We change this to
   [.../lib/menhirLib], and hope that this is where MenhirLib is installed. *)

let libdir () =
  let root =
    Sys.executable_name
    |> normalize
    |> Filename.dirname (* remove [menhir] *)
    |> Filename.dirname (* remove [bin] *)
  in
  Filename.concat
    root
    (Filename.concat "lib" "menhirLib")
