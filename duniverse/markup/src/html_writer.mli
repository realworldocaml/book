(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val write :
  ?escape_attribute:(string -> string) ->
  ?escape_text:(string -> string) ->
  [< signal ] Kstream.t -> string Kstream.t
