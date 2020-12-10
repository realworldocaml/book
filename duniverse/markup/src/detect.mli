(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val select_html : ?limit:int -> char Kstream.t -> Encoding.t cps
val select_xml : char Kstream.t -> Encoding.t cps

(* The following values are exposed for testing. They are not used outside the
   module. *)

val normalize_name : bool -> string -> string
val guess_from_bom_html : char Kstream.t -> string option cps
val guess_from_bom_xml : char Kstream.t -> string option cps
val guess_family_xml : char Kstream.t -> string option cps
val meta_tag_prescan :
  ?supported:(string -> bool cont -> unit) ->
  ?limit:int ->
  char Kstream.t ->
    string option cps
val read_xml_encoding_declaration :
  char Kstream.t -> Encoding.t -> string option cps
