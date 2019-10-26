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

type position =
  { line: int;
    column: int; }

type location =
  { start: position;
    finish: position; }

type parser_error =
  | Unclosed of
      { opening_loc: location;
        opening: string;
        items: string;
        closing: string; }
  | Expecting of string

type lexer_error =
  | Unmatched_target
  | Unmatched_code
  | Unmatched_pre_code
  | Unmatched_html_code
  | Unterminated_verbatim
  | Unterminated_target
  | Unterminated_code
  | Unterminated_pre_code
  | Unterminated_ref
  | Unterminated_html_code
  | Nested_verbatim
  | Nested_target
  | Nested_pre_code
  | Nested_html_code
  | Expected_see
  | Unterminated_see_url
  | Unterminated_see_file
  | Unterminated_see_doc
  | Expected_ident
  | Expected_string
  | Expected_version

type error =
  | Lexer of lexer_error
  | Parser of parser_error

type t =
  { error: error;
    location: location; }

val message: error -> string
