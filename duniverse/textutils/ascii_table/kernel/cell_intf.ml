(** A cell in the table.

    The contents of a cell may have multiple lines, and may be re-wrapped to display the
    cell in a limited width.
*)

open! Core
open! Import

module type Cell = sig
  type t [@@deriving sexp_of]

  val create : Attr.t list -> string -> t
  val attr : t -> Attr.t list

  (** [to_tuple (create attr contents) = attr, lines] where the line breaks in [lines] are
      those indicated by '\n' in [contents] *)
  val to_tuple : t -> Attr.t list * Utf8_text.t list

  (** [is_empty t] returns true if every line is empty. Note that this means [is_empty
      (create [] s) <> String.is_empty s] when [s] is all '\n'. *)
  val is_empty : t -> bool

  (** [width t] returns the length of the longest line in [t] *)
  val width : t -> int

  (** [wrap t ~width = lines] rewraps the lines of a cell to fit within [width]. [wrap]
      only adds new line breaks. *)
  val wrap : t -> width:int -> prefer_split_on_spaces:bool -> Utf8_text.t list

  (** [height t ~display_empty_rows:false ~width = List.length (wrap t ~width)].

      When [display_empty_rows = true], [height] always returns at least 1. *)
  val height
    :  t
    -> display_empty_rows:bool
    -> width:int
    -> prefer_split_on_spaces:bool
    -> int
end
