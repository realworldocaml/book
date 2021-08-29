(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val write :
  ?escape_attribute:(string -> string) ->
  ?escape_text:(string -> string) ->
  [< signal ] Kstream.t -> string Kstream.t
