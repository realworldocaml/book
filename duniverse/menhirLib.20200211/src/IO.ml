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

(* Input-output utilities. *)

(* ------------------------------------------------------------------------- *)
(* [try/finally] has the same semantics as in Java. *)

let try_finally action handler =
  let result =
    try
      action()
    with e ->
      handler();
      raise e
  in
  handler();
  result

(* ------------------------------------------------------------------------- *)
(* [moving_away filename action] moves the file [filename] away (if it exists),
   performs [action], then moves the file back into place (if it was moved
   away). *)

let moving_away filename action =
  if Sys.file_exists filename then
    let newname = filename ^ ".moved_by_menhir" in
    Sys.rename filename newname;
    try_finally action (fun () ->
      Sys.rename newname filename
    )
  else
    action()

(* ------------------------------------------------------------------------- *)
(* [with_file filename creation action] creates the file [filename] by
   running [creation], then runs [action], and ensures that the file
   is removed in the end. *)

let with_file filename creation action =
  creation();
  try_finally action (fun () -> Sys.remove filename)

(* ------------------------------------------------------------------------- *)
(* [exhaust channel] reads all of the data that's available on [channel].
   It does not assume that the length of the data is known ahead of time.
   It does not close the channel. *)

let chunk_size =
  16384

let exhaust channel =
  let buffer = Buffer.create chunk_size in
  let chunk = Bytes.create chunk_size in
  let rec loop () =
    let length = input channel chunk 0 chunk_size in
    if length = 0 then
      Buffer.contents buffer
    else begin
      Buffer.add_subbytes buffer chunk 0 length;
      loop()
    end
  in
  loop()

(* ------------------------------------------------------------------------- *)
(* [invoke command] invokes an external command (which expects no
   input) and returns its output, if the command succeeds. It returns
   [None] if the command fails. *)

let invoke command =
  let ic = Unix.open_process_in command in
  (* 20130911 Be careful to read in text mode, so as to avoid newline
     translation problems (which would manifest themselves on Windows). *)
  set_binary_mode_in ic false;
  let result = exhaust ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 ->
      Some result
  | _ ->
      None

(* ------------------------------------------------------------------------- *)
(* [read_whole_file filename] reads the file [filename] in text mode and
   returns its contents as a string. *)

let read_whole_file filename =

  (* Open the file in text mode, so that (under Windows) CRLF is converted to LF.
     This guarantees that one byte is one character and seems to be required in
     order to report accurate positions. *)

  let channel = open_in filename in

  (* The standard library functions [pos_in] and [seek_in] do not work correctly
     when CRLF conversion is being performed, so we abandon their use. (They were
     used to go and extract the text of semantic actions.) Instead we load the
     entire file into memory up front, and work with a string. *)

  (* The standard library function [in_channel_length] does not work correctly
     when CRLF conversion is being performed, so we do not use it to read the
     whole file. And the standard library function [Buffer.add_channel] uses
     [really_input] internally, so we cannot use it either. Bummer. *)

  let s = exhaust channel in
  close_in channel;
  s

