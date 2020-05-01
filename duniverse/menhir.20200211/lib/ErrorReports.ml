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

(* -------------------------------------------------------------------------- *)

(* A two-place buffer stores zero, one, or two elements. *)

type 'a content =
| Zero
| One of 'a
| Two of 'a * (* most recent: *) 'a

type 'a buffer =
  'a content ref

(* [update buffer x] pushes [x] into [buffer], causing the buffer to slide. *)

let update buffer x =
  buffer :=
    match !buffer, x with
    | Zero, _ ->
        One x
    | One x1, x2
    | Two (_, x1), x2 ->
        Two (x1, x2)

(* [show f buffer] prints the contents of the buffer. The function [f] is
   used to print an element. *)

let show f buffer : string =
  match !buffer with
  | Zero ->
      (* The buffer cannot be empty. If we have read no tokens,
         we cannot have detected a syntax error. *)
      assert false
  | One invalid ->
      (* It is unlikely, but possible, that we have read just one token. *)
      Printf.sprintf "before '%s'" (f invalid)
  | Two (valid, invalid) ->
      (* In the most likely case, we have read two tokens. *)
      Printf.sprintf "after '%s' and before '%s'" (f valid) (f invalid)

(* [last buffer] returns the last element of the buffer (that is, the invalid
   token). *)

let last buffer =
  match !buffer with
  | Zero ->
      (* The buffer cannot be empty. If we have read no tokens,
         we cannot have detected a syntax error. *)
      assert false
  | One invalid
  | Two (_, invalid) ->
      invalid

(* [wrap buffer lexer] *)

open Lexing

let wrap lexer =
  let buffer = ref Zero in
  buffer,
  fun lexbuf ->
    let token = lexer lexbuf in
    update buffer (lexbuf.lex_start_p, lexbuf.lex_curr_p);
    token

(* -------------------------------------------------------------------------- *)
