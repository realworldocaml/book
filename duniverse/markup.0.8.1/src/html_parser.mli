(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val parse :
  [< `Document | `Fragment of string ] option ->
  Error.parse_handler ->
  (location * Html_tokenizer.token) Kstream.t *
  (Html_tokenizer.state -> unit) *
  ((unit -> bool) -> unit) ->
    (location * signal) Kstream.t
