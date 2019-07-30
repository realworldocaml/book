(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val preprocess :
  (int -> bool) -> Error.parse_handler -> int Kstream.t ->
    (location * int) Kstream.t * (unit -> location)
