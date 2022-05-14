open! Core
open! Import

module Align = struct
  type t =
    | Left
    | Right
    | Center
  [@@deriving sexp_of]
end

module Show = struct
  type t =
    [ `Yes
    | `No
    | `If_not_empty
    ]
  [@@deriving sexp_of]
end

module type Column = sig
  module Align = Align
  module Show = Show

  type 'a t [@@deriving sexp_of]

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

  val lift : 'a t -> f:('b -> 'a) -> 'b t
  val align : _ t -> Align.t
  val header : 'a t -> string
  val show : _ t -> Show.t
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

  module Private : sig
    (** [layout ts values ~spacing ~max_width = widths] where the nth int in [widths] is
        the width to which the nth column in [ts] should wrap its contents.

        [spacing] is the number of spaces to leave on either side of the contents of each
        cell. [layout] also leaves a character per column for a vertical separator.
        [layout] raises if all this cannot fit into [max_width]. *)
    val layout : 'a t list -> 'a list -> spacing:int -> max_width:int -> int list

    val to_cell : 'a t -> value:'a -> Cell.t
  end
end
