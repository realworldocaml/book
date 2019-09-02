(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val parse :
  [< `Document | `Fragment ] option ->
  (string -> string option) ->
  Error.parse_handler ->
  (location * Xml_tokenizer.token) Kstream.t ->
    (location * signal) Kstream.t
