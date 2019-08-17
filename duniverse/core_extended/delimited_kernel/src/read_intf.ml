open Core_kernel

(** If we can read a row correctly but the ['a t] provided can't convert it,
    your ['a On_invalid_row.t] will define what happens.

    Here, 'read a row correctly' means that it's valid CSV (subject to your
    delimiter etc) & it has the correct number of columns. If that's not the
    case the parsers will just raise. *)
module type On_invalid_row = sig
  type 'a t

  (** The default. Raise. *)
  val raise : _ t

  (** Skip the bad row *)
  val skip : _ t

  (** Do something else!

      - [`Skip]: skip the line. Same as [skip] above.
      - [`Yield]: return the given value for this row.
      - [`Raise]: raise the given exception
  *)
  val create
    :  (int String.Map.t (** Map from header to position. *)
        -> string Append_only_buffer.t (** Value at each position. *)
        -> exn (** Exception raised when trying to convert this row. *)
        -> [`Skip | `Yield of 'a | `Raise of exn])
    -> 'a t
end

module type Root = sig
  (** Row up to the error, and the field with the error up to the point of
      failure. Same as [Expert.Parse_state.Bad_csv_formatting]. *)
  exception Bad_csv_formatting of string list * string

  (** This provides an applicative interface for constructing values from a csv file.

      An ['a t] describes how to build an OCaml model ['a] for each row.

      Simple example:
      {[
        type t =
          { foo : int
          ; bar : string
          }

        (* Describes how to generate a [t] from a row of a csv file *)
        let parse : t Delimited_kernel.Parse.t =
          let open Delimited_kernel.Parse.Let_syntax in
          let%map_open foo = at_header "foo" ~f:Int.of_string
          and bar = at_header "bar" ~f:String.of_string in
          { foo; bar }
        ;;

        let _ =
          Delimited_kernel.Parse.list_of_string ~header:`Yes parse
            "foo,bar\n2,\"hello, world\"\n"
        ;;
      ]}
  *)
  type 'a t

  include Applicative.S with type 'a t := 'a t

  module Let_syntax : sig
    module Let_syntax : sig
      include Applicative.S with type 'a t := 'a t

      module Open_on_rhs : sig
        val at_index : int -> f:(string -> 'a) -> 'a t
        val at_header : string -> f:(string -> 'a) -> 'a t
        val at_header_opt : string -> f:(string option -> 'a) -> 'a t
      end
    end
  end

  (** Read a field at the given index. Use [f] to convert the field from string. *)
  val at_index : int -> f:(string -> 'a) -> 'a t

  (** Read a field at the given header. Use [f] to convert the field from string.

      Note that if the given header is not provided through either the file or
      the [~header] argument to the parsers, this will fail at runtime.  *)
  val at_header : string -> f:(string -> 'a) -> 'a t

  (** Read a field at the given header, if it exists. Use [f] to convert the field from
      string. *)
  val at_header_opt : string -> f:(string option -> 'a) -> 'a t

  module On_invalid_row : On_invalid_row

  (** Header parsing control *)
  module Header = Header

  (** Whole-row parsing. *)
  module Row : sig
    type 'a builder_t = 'a t

    include module type of Row

    (** A builder for [Row.t]s.

        As this parses the whole row it's slower than using the builder
        interface directly, but simpler to use. *)
    val builder : t builder_t
  end

  (** Fold the CSV rows contained in the given string. *)
  val fold_string
    :  ?strip:bool
    -> ?sep:char
    -> ?quote:[`No_quoting | `Using of char]
    -> ?header:Header.t
    -> ?on_invalid_row:'a On_invalid_row.t
    -> 'a t
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> string
    -> 'b

  (** Load the CSV as a list *)
  val list_of_string
    :  ?strip:bool
    -> ?sep:char
    -> ?quote:[`No_quoting | `Using of char]
    -> ?header:Header.t
    -> ?on_invalid_row:'a On_invalid_row.t
    -> 'a t
    -> string
    -> 'a list
end

module type Expert = sig
  module Append_only_buffer = Append_only_buffer
  module Parse_state = Parse_state
  module On_invalid_row : On_invalid_row

  type 'a t

  val create_parse_state
    :  ?strip:bool
    -> ?sep:char
    -> ?quote:[`No_quoting | `Using of char]
    -> ?on_invalid_row:'a On_invalid_row.t
    -> header_map:int String.Map.t
    -> 'a t
    -> init:'b
    -> f:('b -> 'a -> 'b)
    -> 'b Parse_state.t

  module Builder : sig
    type nonrec 'a t = 'a t

    val lambda : (int String.Map.t -> string Append_only_buffer.t -> 'a) -> 'a t
    val return : 'a -> 'a t
  end

  module Parse_header : sig
    (** Type [t] represents an incomplete header parse. Keep calling [input] on it until you
        get a map from header name to column number. *)
    type t

    val create
      :  ?strip:bool
      -> ?sep:char
      -> ?quote:[`No_quoting | `Using of char]
      -> ?header:Header.t
      -> unit
      -> (t, int String.Map.t) Either.t

    (** [input t ~len s] reads the first [len] bytes from [s] and returns either [t] or
        [header_map, unused_input]. *)
    val input : t -> len:int -> Bytes.t -> (t, int String.Map.t * string) Either.t

    val input_string : t -> len:int -> string -> (t, int String.Map.t * string) Either.t
    val is_at_beginning_of_row : t -> bool
  end

  (** This creates a function that can be fed partial input to return partial parses.
      Please do not use this in new code. *)
  val create_partial
    :  ?strip:bool
    -> ?sep:char
    -> ?quote:[`No_quoting | `Using of char]
    -> ?header:Header.t
    -> unit
    -> ([`Data of string | `Eof] -> Row.t list) Staged.t
end
