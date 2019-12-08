(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012-2013 OCamlPro                                          *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

open Pos
open Approx_lexer

type token = {
  region  : Region.t;
  token   : Approx_lexer.token;
  newlines: int;
  between : string Lazy.t;
  substr  : string Lazy.t;
  offset  : int;
}

type cons =
  | Cons of token * t
  | Null

and t = cons lazy_t

let of_string ?(start_pos=Position.zero) ?(start_offset=0) string =
  let lexbuf = {
    Lexing.
    refill_buff = (fun lexbuf -> lexbuf.Lexing.lex_eof_reached <- true);
    lex_buffer = Bytes.of_string string;
    lex_buffer_len = String.length string;
    lex_abs_pos = start_offset;
    lex_start_pos = start_offset;
    lex_curr_pos = start_offset;
    lex_last_pos = start_offset;
    lex_last_action = 0;
    lex_mem = [||];
    lex_eof_reached = true;
    lex_start_p = start_pos;
    lex_curr_p = start_pos;
  }
  in
  Approx_lexer.init ();
  let rec loop last =
    let open Lexing in
    match Approx_lexer.token_with_comments lexbuf with
    | EOL
    | SPACES -> loop last
    | token ->
    let pos_last = Region.snd last
    and pos_start = lexbuf.lex_start_p
    and pos_end = lexbuf.lex_curr_p
    in
    let region = Region.create pos_start pos_end in
    let offset = Region.start_column region - Region.start_column last
    in
    let spaces = pos_start.pos_cnum - pos_last.pos_cnum in
    let len = pos_end.pos_cnum - pos_start.pos_cnum in
    let newlines = pos_start.pos_lnum - pos_last.pos_lnum in
    let between = lazy (String.sub string pos_last.pos_cnum spaces) in
    let substr = lazy (String.sub string pos_start.pos_cnum len) in
    Cons ({ region; token; newlines; between; substr; offset },
      lazy (match token with
        | EOF -> Null
        | _ -> loop region)
    )
  in
  let init_region =
    let pos_above =
      {start_pos with Lexing.pos_lnum = start_pos.Lexing.pos_lnum - 1}
    in
    Region.create pos_above pos_above
  in
  lazy (loop init_region)

let of_channel ?(start_pos=Position.zero) ic =
  (* add some caching to the reader function, so that
     we can get back the original strings *)
  let buf = Buffer.create 511 in
  let reader str count =
    let n = input ic str 0 count in
    Buffer.add_substring buf (Bytes.to_string str) 0 n;
    n
  in
  let lexbuf = Lexing.from_function reader in
  let lexbuf = { lexbuf with Lexing.lex_start_p = start_pos;
                             Lexing.lex_curr_p = start_pos; }
  in
  Approx_lexer.init ();
  let rec loop last =
    let open Lexing in
    match Approx_lexer.token_with_comments lexbuf with
    | EOL
    | SPACES -> loop last
    | token ->
    let pos_last = Region.snd last
    and pos_start = lexbuf.lex_start_p
    and pos_end = lexbuf.lex_curr_p
    in
    let spaces = pos_start.pos_cnum - pos_last.pos_cnum in
    let len = pos_end.pos_cnum - pos_start.pos_cnum in
    let newlines = pos_start.pos_lnum - pos_last.pos_lnum in
    let between = let s = Buffer.sub buf 0 spaces in lazy s in
    let substr = let s = Buffer.sub buf spaces len in lazy s
    in
    let total = pos_end.pos_cnum - pos_last.pos_cnum in
    let more = Buffer.sub buf total (Buffer.length buf - total) in
    Buffer.clear buf;
    Buffer.add_string buf more;
    let region = Region.create pos_start pos_end in
    let offset = Region.start_column region - Region.start_column last in
    Cons ({ region; token; newlines; between; substr; offset },
      lazy (match token with
        | EOF -> Null
        | _ -> loop region)
    )
  in
  let init_region =
    let pos_above =
      {start_pos with Lexing.pos_lnum = start_pos.Lexing.pos_lnum - 1}
    in
    Region.create pos_above pos_above
  in
  lazy (loop init_region)

let next = function
  | lazy Null -> None
  | lazy (Cons (car, cdr)) -> Some (car, cdr)
