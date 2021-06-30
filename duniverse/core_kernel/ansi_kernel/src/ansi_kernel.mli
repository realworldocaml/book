(** Common ANSI display attribute definitions.

    NOTE: assorted content lifted from lib/console/src/console.ml *)

module Color_256 = Color_256

module Color : sig
  type primary =
    [ `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    ]

  (** Standard 8 colors and 256-color palette. *)
  type t =
    [ primary
    | `Color_256 of Color_256.t
    ]
  [@@deriving sexp_of, compare, hash, equal]

  val to_int_list : [< t ] -> int list
end

module Attr : sig
  (** Styling attributes: these provide most of the ANSI display attributes,
      but not directly `Reset, `Blink and `Hidden, so as to explicitly
      discourage their use in general code. *)
  type t =
    [ `Bright
    | `Dim
    | `Underscore
    | `Reverse
    | Color.t
    | `Bg of Color.t
    ]
  [@@deriving sexp_of, compare, hash, equal]

  val to_int_list : [< t ] -> int list
  val list_to_string : [< t ] list -> string
end

module With_all_attrs : sig
  (** All supported (by this library) ANSI display attributes. *)
  type t =
    [ Attr.t
    | `Reset
    | `Blink
    | `Hidden
    ]
  [@@deriving sexp_of, compare, hash, equal]

  val to_int_list : [< t ] -> int list
  val list_to_string : [< t ] list -> string
end
