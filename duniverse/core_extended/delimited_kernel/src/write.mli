open Core_kernel

(** Used to describe a way to create a single row of a CSV
    from a given type.
*)
type -'a t

val empty : 'a t
val column : ('a -> string) -> header:string -> 'a t
val column_m : (module Stringable.S with type t = 'a) -> header:string -> 'a t

(** [default] is printed in place of [None], and if not supplied
    is the empty string.
*)
val column_m_opt
  :  ?default:string
  -> (module Stringable.S with type t = 'a)
  -> header:string
  -> 'a option t

val of_list : 'a t list -> 'a t
val append : 'a t -> 'a t -> 'a t
val contra_map : 'b t -> f:('a -> 'b) -> 'a t
val map_headers : 'a t -> f:(string -> string) -> 'a t
val headers : 'a t -> string list
val to_columns : 'a t -> 'a -> string list

(** Open for prefix operators useful for using with Fields.to_list.

    e.g.
    {[
      let csv =
        Builder.Fields_O.(
          Fields.to_list
            ~a_string:!!Fn.id
            ~a_date:!!Date.to_string
            ~mv:!>Long_short.csv)
        |> Builder.of_list
    ]}
*)
module Fields_O : sig
  (** Create a single column from a field of a record. *)
  val ( !! ) : ('a -> string) -> ('b, 'a) Field.t -> 'b t

  (** Nest a builder in a field of a record.

      Column headers will be prefixed with the name of the field.
  *)
  val ( !> ) : 'a t -> ('b, 'a) Field.t -> 'b t
end

module O : sig
  val ( <<| ) : 'b t -> ('a -> 'b) -> 'a t
  val ( <> ) : 'a t -> 'a t -> 'a t
end

module By_row : sig
  type row = string list

  (** Prints a valid csv file to a given channel.
      The [eol] arg can be used to override the default line ending
      of "\r\n" (DOS line endings).
      Example ~eol:`Unix to get *nix line endings
  *)
  val output_lines
    :  ?quote:char
    -> ?sep:char
    -> ?eol:[`Dos | `Unix]
    -> Out_channel.t
    -> row list
    -> unit

  (** Convert one CSV line to a string. *)
  val line_to_string : ?quote:char -> ?sep:char -> row -> string
end

module Expert : sig
  (** Escape the a CSV field if need be.*)
  val maybe_escape_field : ?quote:char -> ?sep:char -> string -> string

  (** Escape a CSV (even if doesn't have any characters that require escaping).*)
  val escape_field : ?quote:char -> string -> string

  (** Get the escaped length of one quoted field (without the quotes). Returns
      None if the field doesn't need to be escaped. *)
  val quote_len : quote:char -> sep:char -> pos:int -> len:int -> string -> int option

  (** Copy and escapes the content of a field over from one string to
      another. This does not put the quotes in.*)
  val quote_blit
    :  quote:char
    -> src:string
    -> dst:Bytes.t
    -> src_pos:int
    -> dst_pos:int
    -> len:int
    -> int
end
