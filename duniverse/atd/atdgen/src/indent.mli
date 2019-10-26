(** Simple indentation utility for code generators *)

type t =
  | Line of string        (** single line (not indented) **)
  | Block of t list       (** indented sequence **)
  | Inline of t list      (** in-line sequence (not indented) **)
  | Annot of string * t (** arbitrary annotation **)

val paren : t -> t

val strip : t -> t

val concat : 'a -> 'a list -> 'a list

val to_buffer : ?offset:int -> ?indent:int -> Buffer.t -> t list -> unit
  (** Write to a buffer.

      @param offset defines the number of space characters
      to use for the left margin. Default: 0.

      @param indent defines the number of space characters to use for
      indenting blocks. Default: 2.
  *)

val to_string : ?offset:int -> ?indent:int -> t list -> string
  (** Write to a string. See [to_buffer] for the options. *)

val to_channel : ?offset:int -> ?indent:int -> out_channel -> t list -> unit
  (** Write to a channel. See [to_buffer] for the options. *)

val to_stdout : ?offset:int -> ?indent:int -> t list -> unit
  (** Write to [stdout]. See [to_buffer] for the options. *)
