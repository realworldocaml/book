open Core

(** Two dimensional blocks of text *)
type t [@@deriving sexp_of]

include Invariant.S with type t := t

(** The empty block. a left and right unit to both [hcat] and [vcat] *)
val nil : t

(** [fill] and [space] assume width and height are non-negative *)
val fill : char -> width:int -> height:int -> t

val space : width:int -> height:int -> t

(** Fill a space with a Unicode character *)
val fill_uchar : Uchar.t -> width:int -> height:int -> t

(** Vertical and horizontal alignment specifications *)
type valign =
  [ `Top
  | `Bottom
  | `Center
  ]

type halign =
  [ `Left
  | `Right
  | `Center
  ]

(** A basic block of text, split on newlines and horizontally aligned as specified.

    If [max_width] is provided, split each line of the input on whitespace and wrap words
    to respect the request.  So long as no words are longer than [max_width], the
    resulting text block will be no wider than [max_width]
*)
val text : ?align:halign -> ?max_width:int -> string -> t

(** Like [text], but takes a format string like printf *)
val textf : ?align:halign -> ?max_width:int -> ('r, unit, string, t) format4 -> 'r

(** Vertical concatenation with alignment *)
val vcat : ?align:halign -> ?sep:t -> t list -> t

(** Horizontal concatenation with alignment *)
val hcat : ?align:valign -> ?sep:t -> t list -> t

(** Text block dimensions *)

val width : t -> int
val height : t -> int

(** Vertical and horizontal sequence alignment.  Both [valign] and [halign] return a list
    of the same length as the input, with the corresponding elements padded to the
    appropriate alignment.

    If you have a list of a statically known length, using [With_static_lengths.valign] or
    [With_static_lengths.halign] below will let the type checker know that the length of
    the returned list is equal to the length of the input list.
*)

val valign : valign -> t list -> t list
val halign : halign -> t list -> t list

(** Empty blocks with either horizontal or vertical extent -- useful for specifying a
    minimum width or height in conjunction with valign or halign, respectively *)

val hstrut : int -> t
val vstrut : int -> t

(** Wrap a block with an ANSI escape sequence.
    The [prefix] and [suffix] arguments should render with zero width and height. *)
val ansi_escape : ?prefix:string -> ?suffix:string -> t -> t

(** render a block of text as a string *)
val render : t -> string

(** Alignment of a 2D grid of blocks *)
val table : ?sep_width:int -> [ `Cols of (t list * halign) list ] -> [ `Rows of t list ]

(** Compress table header according to column widths.

    Input:  a list of columns of the form (title, values, column alignment).
    Output: one header block and row sequence.
    Raises: if the [values] lists are not the same length in each column.
    Example:

    {v
                                                 first name
      age  first name  last name            age  |     last name
      |    |           |            ==>     |    |     |
      30   sue         smith                30   sue   smith
      18   bill        rodriguez            18   bill  rodriguez
      76   rick        jones                76   rick  jones
    v} *)
val compress_table_header
  :  ?sep_width:int
  -> [ `Cols of (t * t list * halign) list ]
  -> [ `Header of t ] * [ `Rows of t list ]

(** Combinators for building up cell structures separated by box characters: e.g.

    {[
      let a = text "A" in
      let b = text "B" in
      let c = text "C" in
      boxed Boxed.(hcat [vcat [cell a; cell b]; c])
    ]}

    and

    {[
      let a = text "A" in
      let b = text "B" in
      let c = text "C" in
      boxed Boxed.(hcat [
        vcat [cell a; cell b];
        c;
        vcat [cell b; cell a]
      ])
    ]}

    produce

    ┌───┬───┐       ┌───┬───┬───┐
    │ A │   │       │ A │   │ B │
    ├───┤ C │  and  ├───┤ C ├───┤
    │ B │   │       │ B │   │ A │
    └───┴───┘       └───┴───┴───┘

    respectively.
*)
module Boxed : sig
  type outer_t := t
  type t [@@deriving sexp_of]

  (** An outlined table cell, possibly with extra space (padding) on the sides.

      [hpadding] defaults to 1.
      [vpadding] defaults to 0.
  *)
  val cell : ?hpadding:int -> ?vpadding:int -> outer_t -> t

  (** Vertical concatenation with inserts horizontal separator lines. *)
  val vcat : ?align:halign -> t list -> t

  (** horizontal concatenation with inserts vertical separator lines. *)
  val hcat : ?align:valign -> t list -> t
end

(** See comment for [Boxed] *)
val boxed : Boxed.t -> t

(* convenience definitions *)

(** [vsep = vstrut 1] *)
val vsep : t

(** [hsep = hstrut 1] *)
val hsep : t

(** [indent ~n t = hcat [hstrut n; t]].  [n] defaults to [2] *)
val indent : ?n:int -> t -> t

(** [sexp sexp_of_a a = sexp_of_a a |> Sexp.to_string |> text] *)
val sexp : ('a -> Sexp.t) -> 'a -> t

(** Versions of halign and valign with the invariant about list length encoded into the
    types.

    You can write, for example:
    {[

      let [x; y] = Text_block.With_static_lengths.valign [x; y] in
      ...
    ]}
*)
module With_static_lengths : sig
  module List : sig
    type ('a, 'shape) t =
      | [] : (_, [ `nil ]) t
      | ( :: ) : 'a * ('a, 'shape) t -> ('a, [ `cons of 'shape ]) t
  end

  val halign : halign -> (t, 'shape) List.t -> (t, 'shape) List.t
  val valign : valign -> (t, 'shape) List.t -> (t, 'shape) List.t
end
