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

let last buffer =
  match !buffer with
  | Zero ->
      (* The buffer cannot be empty. If we have read no tokens,
         we cannot have detected a syntax error. *)
      assert false
  | One invalid
  | Two (_, invalid) ->
      invalid

open Lexing

let wrap lexer =
  let buffer = ref Zero in
  buffer,
  fun lexbuf ->
    let token = lexer lexbuf in
    update buffer (lexbuf.lex_start_p, lexbuf.lex_curr_p);
    token

let wrap_supplier supplier =
  let buffer = ref Zero in
  buffer,
  fun () ->
    let (_token, pos1, pos2) as triple = supplier() in
    update buffer (pos1, pos2);
    triple

(* -------------------------------------------------------------------------- *)

let extract text (pos1, pos2) : string =
  let ofs1 = pos1.pos_cnum
  and ofs2 = pos2.pos_cnum in
  let len = ofs2 - ofs1 in
  try
    String.sub text ofs1 len
  with Invalid_argument _ ->
    (* In principle, this should not happen, but if it does, let's make this
       a non-fatal error. *)
    "???"

let sanitize text =
  String.map (fun c ->
    if Char.code c < 32 then ' ' else c
  ) text

(* If we were willing to depend on [Str], we could implement [compress] as
   follows:

   let compress text =
     Str.global_replace (Str.regexp "[ \t\n\r]+") " " text

 *)

let rec compress n b i j skipping =
  if j < n then
    let c, j = Bytes.get b j, j + 1 in
    match c with
    | ' ' | '\t' | '\n' | '\r' ->
        let i = if not skipping then (Bytes.set b i ' '; i + 1) else i in
        let skipping = true in
        compress n b i j skipping
    | _ ->
        let i = Bytes.set b i c; i + 1 in
        let skipping = false in
        compress n b i j skipping
  else
    Bytes.sub_string b 0 i

let compress text =
  let b = Bytes.of_string text in
  let n = Bytes.length b in
  compress n b 0 0 false

let shorten k text =
  let n = String.length text in
  if n <= 2 * k + 3 then
    text
  else
    String.sub text 0 k ^
    "..." ^
    String.sub text (n - k) k

let is_digit c =
  let c = Char.code c in
  Char.code '0' <= c && c <= Char.code '9'

exception Copy

let expand f text =
  let n = String.length text in
  let b = Buffer.create n in
  let rec loop i =
    if i < n then begin
      let c, i = text.[i], i + 1 in
      loop (
        try
          if c <> '$' then raise Copy;
          let j = ref i in
          while !j < n && is_digit text.[!j] do incr j done;
          if i = !j then raise Copy;
          let k = int_of_string (String.sub text i (!j - i)) in
          Buffer.add_string b (f k);
          !j
        with Copy ->
          (* We reach this point if either [c] is not '$' or [c] is '$'
             but is not followed by an integer literal. *)
          Buffer.add_char b c;
          i
      )
    end
    else
      Buffer.contents b
  in
  loop 0
