(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

open Lexing
open Printf

let init filename lexbuf =
  lexbuf.lex_curr_p <- {
    pos_fname = filename;
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0
  };
  lexbuf

let read filename =
  let c = open_in filename in
  let text = really_input_string c (in_channel_length c) in
  close_in c;
  let lexbuf = Lexing.from_string text in
  text, init filename lexbuf

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

let is_dummy (pos1, pos2) =
  pos1 == dummy_pos || pos2 == dummy_pos

let range ((pos1, pos2) as range) =
  if is_dummy range then
    sprintf "At an unknown location:\n"
  else
    let file = pos1.pos_fname in
    let line = pos1.pos_lnum in
    let char1 = pos1.pos_cnum - pos1.pos_bol in
    let char2 = pos2.pos_cnum - pos1.pos_bol in (* yes, [pos1.pos_bol] *)
    sprintf "File \"%s\", line %d, characters %d-%d:\n"
      file line char1 char2
      (* use [char1 + 1] and [char2 + 1] if *not* using Caml mode *)
