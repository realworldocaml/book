(*{{{ Copyright (C) 2012, David Sheets <sheets@alum.mit.edu>

  Permission to use, copy, modify, and/or distribute this software for
  any purpose with or without fee is hereby granted, provided that the
  above copyright notice and this permission notice appear in all
  copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  }}}*)

(** Accept-Encoding HTTP header parsing and generation *)

(** Qualities are integers between 0 and 1000.
    A header with ["q=0.7"] corresponds to a quality of [700].
*)
type q = int [@@deriving sexp]

(** Lists, annotated with qualities. *)
type 'a qlist = (q * 'a) list [@@deriving sexp]

(** Sort by quality, biggest first.
    Respect the initial ordering.
*)
val qsort : 'a qlist -> 'a qlist

type pv = Accept_types.pv =
    T of string
  | S of string [@@deriving sexp]

type p = string * pv [@@deriving sexp]

type media_range =
  Accept_types.media_range =
    MediaType of string * string
  | AnyMediaSubtype of string
  | AnyMedia [@@deriving sexp]

type charset = Accept_types.charset =
    Charset of string
  | AnyCharset [@@deriving sexp]

type encoding =
  Accept_types.encoding =
    Encoding of string
  | Gzip
  | Compress
  | Deflate
  | Identity
  | AnyEncoding [@@deriving sexp]

(** Basic language range tag.
    ["en-gb"] is represented as [Language ["en"; "gb"]].
    @see <https://tools.ietf.org/html/rfc7231#section-5.3.5> the specification.
*)
type language = Accept_types.language =
    Language of string list
  | AnyLanguage [@@deriving sexp]


val media_ranges :
  string option -> (media_range * p list) qlist

val charsets : string option -> charset qlist

val encodings : string option -> encoding qlist

val languages : string option -> language qlist

val string_of_media_range : media_range * (string * pv) list -> q -> string
val string_of_charset : charset -> q -> string
val string_of_encoding : encoding -> q -> string
val string_of_language : language -> q -> string

val string_of_media_ranges : (media_range * p list) qlist -> string
val string_of_charsets : charset qlist -> string
val string_of_encodings : encoding qlist -> string
val string_of_languages : language qlist -> string
