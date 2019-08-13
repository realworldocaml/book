(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

(** Lexer positions & regions *)

(** Lexer positions *)
module Position : sig

  (** A position in a lexer stream *)
  type t = Lexing.position

  (** Pretty-print a position *)
  val to_string: t -> string

  (** Initial position *)
  val zero: t

  (** Get the coloumn offset associated to a lexing position *)
  val column: t -> int
end

(** Lexer regions *)
module Region : sig

  (** A region in a lexer stream *)
  type t

  (** Create a region from a starting and an ending position *)
  val create: Position.t -> Position.t -> t

  val fst: t -> Position.t
  val snd: t -> Position.t

  (** Return the column where the region starts *)
  val start_column: t -> int

  (** Return the column where the region ends *)
  val end_column: t -> int

  (** Get the region offset (number of characters from the beginning
      of the file *)
  val char_offset: t -> int

  (** Get the lenght of a region *)
  val length: t -> int

  (** Return the line number where the region starts *)
  val start_line: t -> int

  (** Return the line number where the region ends *)
  val end_line: t -> int

  (** The empty region *)
  val zero: t

  (** [translate t x] shifts a region by [x] characters *)
  val translate: t -> int -> t
end
