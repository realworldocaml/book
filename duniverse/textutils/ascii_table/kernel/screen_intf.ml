(** A [Screen] is two-dimensional grid of text elements onto which we draw imperatively. *)

open! Core
open! Import

module Texel = struct
  (** A [tex]t [el]ement *)

  type t =
    | Line
    | Blank
    | Char of Attr.t list * Uchar.t
  [@@deriving compare, sexp_of]
end

module type Screen = sig
  type t [@@deriving sexp_of]

  val to_string
    :  t
    -> bars:[ `Ascii | `Unicode ]
    -> string_with_attr:(Attr.t list -> string -> string)
    -> string

  val render
    :  t
    -> bars:[ `Ascii | `Unicode ]
    -> output:(Attr.t list -> Buffer.t -> unit)
    -> close:(Buffer.t -> 'a)
    -> 'a

  module Texel = Texel

  (** [string t align attr s ~row ~col ~width] writes a region one high and [width]
      across, whose left corner is at [row, col]. Within that region, it places [s] as
      directed by [align]. *)
  val string
    :  t
    -> Column.Align.t
    -> Attr.t list
    -> Utf8_text.t
    -> row:int
    -> col:int
    -> width:int
    -> unit

  (** [char t attr c ~row ~col] writes [c] at [row, col]. *)
  val char : t -> Attr.t list -> Uchar.t -> row:int -> col:int -> unit

  val create : rows:int -> cols:int -> t

  (** [hline t texel ~row] sets the entire width of [row] to [texel] *)
  val hline : t -> Texel.t -> row:int -> unit

  (** [vline t texel ~col] sets the entire height of [col] to [texel] *)
  val vline : t -> Texel.t -> col:int -> unit
end
