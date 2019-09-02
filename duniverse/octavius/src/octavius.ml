(*
 * Copyright (c) 2015 Leo White <leo@lpw25.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Types = Types

module Errors = Errors

open Common

type nonrec ('a, 'b) result = ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let parse lexbuf =
  let open Errors in
    try
      Ok (OctParser.main OctLexer.main lexbuf)
    with
    | ParserError(location, err) ->
        Error {Errors.error = Parser err; location}
    | LexerError(location, err) ->
        Error {Errors.error = Lexer err; location}

let parse_ref lexbuf =
  let open Errors in
    try
      Ok (OctParser.reference_parts OctLexer.read_ref lexbuf)
    with
    | ParserError(location, err) ->
        Error {Errors.error = Parser err; location}
    | LexerError(location, err) ->
        Error {Errors.error = Lexer err; location}

let print fmt t =
  Print.pp fmt t
