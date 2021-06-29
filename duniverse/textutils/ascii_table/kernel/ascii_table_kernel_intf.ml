open! Core_kernel
module Attr = Ansi_kernel.Attr

module Align = struct
  type t =
    | Left
    | Right
    | Center
end

module type Ascii_table_kernel = sig
  module Attr = Attr
  module Align = Align

  module Display : sig
    type t

    (** Default--cells can be multi-line. *)
    val short_box : t

    (** Puts --- between entries. *)
    val tall_box : t

    (** Trails off with ... if necessary. *)
    val line : t

    (** No lines. *)
    val blank : t

    (** Draw lines only under column titles. *)
    val column_titles : t
  end

  module Column : sig
    type 'a t

    (** creates a column given the header and the to-string function *)
    val create
      :  ?align:Align.t (* Default: left *)
      -> ?min_width:int
      -> ?max_width:int
      -> ?show:[ `Yes | `No | `If_not_empty ] (* Default: `Yes *)
      -> string
      -> ('a -> string)
      -> 'a t

    (** like create, except that the to_string function must provide a list of
        attributes. *)
    val create_attr
      :  ?align:Align.t (* Default: left *)
      -> ?min_width:int
      -> ?max_width:int
      -> ?show:[ `Yes | `No | `If_not_empty ] (* Default: `Yes *)
      -> string
      -> ('a -> Attr.t list * string)
      -> 'a t

    val header : 'a t -> string
    val to_data : 'a t -> 'a -> Attr.t list * string list

    module Of_field : sig
      (** This module is used for constructing lists of ['a t]s from a record's fields. The
          intention is to use [Fields.to_list] to obtain a list. *)

      (** Create a column based on a field of a record. *)
      val field
        :  ?align:Align.t (* Default: left *)
        -> ?min_width:int
        -> ?max_width:int
        -> ?show:[ `Yes | `No | `If_not_empty ]
        -> ?header:string (** Defaults to field name *)
        -> ('field -> string)
        -> ('record, 'field) Field.t
        -> 'record t

      (** [field_attr] is to [field] as [create_attr] is to [create]. *)
      val field_attr
        :  ?align:Align.t (* Default: left *)
        -> ?min_width:int
        -> ?max_width:int
        -> ?show:[ `Yes | `No | `If_not_empty ]
        -> ?header:string (** Defaults to field name *)
        -> ('field -> Attr.t list * string)
        -> ('record, 'field) Field.t
        -> 'record t

      (** Like [field], but defaults to [""] if [None] *)
      val field_opt
        :  ?align:Align.t (* Default: left *)
        -> ?min_width:int
        -> ?max_width:int
        -> ?show:[ `Yes | `No | `If_not_empty ]
        -> ?header:string (** Defaults to field name *)
        -> ('field -> string)
        -> ('record, 'field option) Field.t
        -> 'record t

      (** Like [field_attr], but defaults to [([], "")] if [None] *)
      val field_opt_attr
        :  ?align:Align.t (* Default: left *)
        -> ?min_width:int
        -> ?max_width:int
        -> ?show:[ `Yes | `No | `If_not_empty ]
        -> ?header:string (** Defaults to field name *)
        -> ('field -> Attr.t list * string)
        -> ('record, 'field option) Field.t
        -> 'record t
    end
  end

  module Screen : sig
    (** A [Screen.t] represents a table after all of the layout calculations have been done.
    *)
    type t

    val render
      :  t
      -> bars:[ `Ascii | `Unicode ]
      -> output:(Attr.t list -> Buffer.t -> unit)
      -> close:(Buffer.t -> 'a)
      -> 'a

    (** Given a way to annotate strings with attributes, a [t] can be output to a string. *)
    val to_string
      :  t
      -> bars:[ `Ascii | `Unicode ]
      -> string_with_attr:(Attr.t list -> string -> string)
      -> string
  end

  val draw
    :  ?display:Display.t (* Default: short_box *)
    -> ?spacing:int (* Default: 1 *)
    -> ?limit_width_to:int (* defaults to 90 characters *)
    -> ?header_attr:Attr.t list
    -> ?display_empty_rows:bool (* Default: false *)
    -> 'row Column.t list
    -> 'row list
    -> Screen.t option

  module Table_char : sig
    type t =
      { ascii : char
      ; utf8 : string
      }

    val connect : ?top:unit -> ?bottom:unit -> ?left:unit -> ?right:unit -> unit -> t
  end
end
