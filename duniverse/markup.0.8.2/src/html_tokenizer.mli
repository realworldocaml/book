(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

type token =
  [ `Doctype of doctype
  | `Start of Token_tag.t
  | `End of Token_tag.t
  | `Char of int
  | `Comment of string
  | `EOF ]

type state = [ `Data | `RCDATA | `RAWTEXT | `Script_data | `PLAINTEXT ]

val tokenize :
  Error.parse_handler ->
  (location * int) Kstream.t * (unit -> location) ->
    (location * token) Kstream.t *
    (state -> unit) *
    ((unit -> bool) -> unit)
