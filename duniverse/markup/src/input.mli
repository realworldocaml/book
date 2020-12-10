(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val preprocess :
  (int -> bool) -> Error.parse_handler -> int Kstream.t ->
    (location * int) Kstream.t * (unit -> location)
