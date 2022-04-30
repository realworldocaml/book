(** Simple indentation utility for code generators *)

type node =
  | Line of string           (** single line (not indented) **)
  | Block of node list       (** indented sequence **)
  | Inline of node list      (** in-line sequence (not indented) **)

type t = node list

val is_empty_node : node -> bool

val to_buffer : ?offset:int -> ?indent:int -> Buffer.t -> t -> unit
  (** Write to a buffer.

      @param offset defines the number of space characters
      to use for the left margin. Default: 0.

      @param indent defines the number of space characters to use for
      indenting blocks. Default: 2.
  *)

val to_string : ?offset:int -> ?indent:int -> t -> string
  (** Write to a string. See [to_buffer] for the options. *)

val to_channel : ?offset:int -> ?indent:int -> out_channel -> t -> unit
  (** Write to a channel. See [to_buffer] for the options. *)

val to_stdout : ?offset:int -> ?indent:int -> t -> unit
  (** Write to [stdout]. See [to_buffer] for the options. *)

val to_file : ?indent:int -> string -> t -> unit
  (** Write to a file, overwriting it if it was already there.
      See [to_buffer] for the options. *)
