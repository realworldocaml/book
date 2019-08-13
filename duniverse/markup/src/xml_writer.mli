(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val write :
  Error.write_handler ->
  (string -> string option) ->
  [< signal ] Kstream.t ->
    string Kstream.t
