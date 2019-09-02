(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)


(** This module contains the output function for the [info] type bound to nodes
    in the trie, either through [Format] or directly to strings using [Print] *)

open IndexTypes

module Format: sig
  type coloriser =
    { f: 'a. kind ->
        ('a, Format.formatter, unit) format -> Format.formatter
        -> 'a }

  val color: coloriser
  val no_color: coloriser

  (** short name of the identifier *)
  val name: ?colorise:coloriser -> Format.formatter -> info -> unit

  (** fully qualified name (with [short], returns the path the ident was found
      at, not the path where it was originally created) *)
  val path: ?short:bool -> ?colorise:coloriser -> Format.formatter -> info -> unit

  val kind: ?colorise:coloriser -> Format.formatter -> info -> unit

  val ty: ?colorise:coloriser -> Format.formatter -> info -> unit

  val doc:
    ?escaped:bool ->
    ?colorise:coloriser -> Format.formatter -> info -> unit

  val loc:
    ?root:string -> ?intf:bool ->
    ?colorise:coloriser -> Format.formatter -> info -> unit

  val file: ?colorise:coloriser -> Format.formatter -> info -> unit

  (** summary of the information *)
  val info: ?colorise:coloriser -> Format.formatter -> info -> unit

  (** print following a custom format string (%n,%p,%k,%t,%d,%l,%s,%f,%i are
      interpreted). If [~separate] is set to [true], escapes are formatted
      independently. *)
  val format:
    ?root:string -> ?separate:bool -> string -> ?colorise:coloriser ->
    Format.formatter -> info -> unit

end

module Print: sig
  val disable_split_lines: unit -> unit
  val name: ?color:bool -> info -> string
  val path: ?short:bool -> ?color:bool -> info -> string
  val kind: ?color:bool -> info -> string
  val ty: ?color:bool -> info -> string
  val doc: ?escaped:bool -> ?color:bool -> info -> string
  val loc: ?root:string -> ?intf:bool -> ?color:bool -> info -> string
  val file: ?color:bool -> info -> string
  val info: ?color:bool -> info -> string
  val format: ?root:string -> ?separate:bool -> string -> ?color:bool -> info -> string
end
