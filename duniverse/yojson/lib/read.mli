val prettify : ?std:bool -> string -> string
  (** Combined parser and pretty-printer.
      See [to_string] for the role of the optional [std] argument. *)

val compact : ?std:bool -> string -> string
  (** Combined parser and printer.
      See [to_string] for the role of the optional [std] argument. *)

(** {2 JSON readers} *)

exception Finally of exn * exn
(** Exception describing a failure in both finalizer and parsing. *)

val from_string :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a string.
      @param buf use this buffer at will during parsing instead of creating
      a new one.
      @param fname data file name to be used in error messages. It does
      not have to be a real file.
      @param lnum number of the first line of input. Default is 1.
  *)

val from_channel :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t
  (** Read a JSON value from a channel.
      See [from_string] for the meaning of the optional arguments. *)

val from_file :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t
  (** Read a JSON value from a file.
      See [from_string] for the meaning of the optional arguments. *)


type lexer_state = Lexer_state.t = {
  buf : Bi_outbuf.t;
  mutable lnum : int;
  mutable bol : int;
  mutable fname : string option;
}
    (** This alias is provided for backward compatibility.
        New code should refer to {!Yojson.lexer_state} directly.
    *)

val init_lexer :
  ?buf: Bi_outbuf.t ->
  ?fname: string ->
  ?lnum: int ->
  unit -> lexer_state
  (** This alias is provided for backward compatibility.
      New code should use {!Yojson.init_lexer} directly. *)

val from_lexbuf :
  lexer_state ->
  ?stream:bool ->
  Lexing.lexbuf -> t
  (** Read a JSON value from a lexbuf.
      A valid initial [lexer_state] can be created with [init_lexer].
      See [from_string] for the meaning of the optional arguments.

      @param stream indicates whether more data may follow. The default value
      is false and indicates that only JSON whitespace can be found between
      the end of the JSON value and the end of the input. *)

val stream_from_string :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t Stream.t
  (** Input a sequence of JSON values from a string.
      Whitespace between JSON values is fine but not required.
      See [from_string] for the meaning of the optional arguments. *)

val stream_from_channel :
  ?buf:Bi_outbuf.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> t Stream.t
  (** Input a sequence of JSON values from a channel.
      Whitespace between JSON values is fine but not required.
      @param fin finalization function executed once when the end of the
      stream is reached either because there is no more input or because
      the input could not be parsed, raising an exception.
      @raise Finally When the parsing and the finalizer both raised, [Finally (exn, fin_exn)]
      is raised, [exn] being the parsing exception and [fin_exn] the finalizer one.

      See [from_string] for the meaning of the other optional arguments. *)

val stream_from_file :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> t Stream.t
  (** Input a sequence of JSON values from a file.
      Whitespace between JSON values is fine but not required.

      See [from_string] for the meaning of the optional arguments. *)

val stream_from_lexbuf :
  lexer_state ->
  ?fin:(unit -> unit) ->
  Lexing.lexbuf -> t Stream.t
  (** Input a sequence of JSON values from a lexbuf.
      A valid initial [lexer_state] can be created with [init_lexer].
      Whitespace between JSON values is fine but not required.
      @raise Finally When the parsing and the finalizer both raised, [Finally (exn, fin_exn)]
      is raised, [exn] being the parsing exception and [fin_exn] the finalizer one.

      See [stream_from_channel] for the meaning of the optional [fin]
      argument. *)


type json_line = [ `Json of t | `Exn of exn ]
    (** The type of values resulting from a parsing attempt of a JSON value. *)

val linestream_from_channel :
  ?buf:Bi_outbuf.t ->
  ?fin:(unit -> unit) ->
  ?fname:string ->
  ?lnum:int ->
  in_channel -> json_line Stream.t
  (** Input a sequence of JSON values, one per line, from a channel.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [stream_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments. *)

val linestream_from_file :
  ?buf:Bi_outbuf.t ->
  ?fname:string ->
  ?lnum:int ->
  string -> json_line Stream.t
  (** Input a sequence of JSON values, one per line, from a file.
      Exceptions raised when reading malformed lines are caught
      and represented using [`Exn].

      See [stream_from_channel] for the meaning of the optional [fin]
      argument.
      See [from_string] for the meaning of the other optional arguments. *)

val read_t : lexer_state -> Lexing.lexbuf -> t
(** Read a JSON value from the given lexer_state and lexing buffer and return it.
    Provided as a reader function for atdgen.
*)


(**/**)
(* begin undocumented section *)

val finish_string : lexer_state -> Lexing.lexbuf -> string

val read_string : lexer_state -> Lexing.lexbuf -> string
val read_ident : lexer_state -> Lexing.lexbuf -> string

val map_string :
  lexer_state -> (string -> int -> int -> 'a) -> Lexing.lexbuf -> 'a
  (* equivalent to finish_string *)

val map_ident :
  lexer_state -> (string -> int -> int -> 'a) -> Lexing.lexbuf -> 'a
  (* equivalent to read_ident *)


type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]
val start_any_variant : lexer_state -> Lexing.lexbuf -> variant_kind
val finish_variant : lexer_state -> Lexing.lexbuf -> t option
val finish_skip_variant : lexer_state -> Lexing.lexbuf -> unit
val read_lt : lexer_state -> Lexing.lexbuf -> unit
val read_gt : lexer_state -> Lexing.lexbuf -> unit
val read_comma : lexer_state -> Lexing.lexbuf -> unit

val finish_stringlit : lexer_state -> Lexing.lexbuf -> string
val finish_skip_stringlit : lexer_state -> Lexing.lexbuf -> unit
val finish_escaped_char : lexer_state -> Lexing.lexbuf -> unit
val finish_comment : lexer_state -> Lexing.lexbuf -> unit


val read_space : lexer_state -> Lexing.lexbuf -> unit
val read_eof : Lexing.lexbuf -> bool
val read_null : lexer_state -> Lexing.lexbuf -> unit
val read_null_if_possible : lexer_state -> Lexing.lexbuf -> bool
val read_bool : lexer_state -> Lexing.lexbuf -> bool
val read_int : lexer_state -> Lexing.lexbuf -> int
val read_int8 : lexer_state -> Lexing.lexbuf -> char
val read_int32 : lexer_state -> Lexing.lexbuf -> int32
val read_int64 : lexer_state -> Lexing.lexbuf -> int64
val read_number : lexer_state -> Lexing.lexbuf -> float
val skip_ident : lexer_state -> Lexing.lexbuf -> unit

val read_sequence :
  ('a -> lexer_state -> Lexing.lexbuf -> 'a) ->
  'a ->
  lexer_state ->
  Lexing.lexbuf -> 'a

val read_list :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a list

val read_list_rev :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a list

val read_array_end : Lexing.lexbuf -> unit
val read_array_sep : lexer_state -> Lexing.lexbuf -> unit

val read_array :
  (lexer_state -> Lexing.lexbuf -> 'a) ->
  lexer_state ->
  Lexing.lexbuf -> 'a array

val read_tuple :
  (int -> 'a -> lexer_state -> Lexing.lexbuf -> 'a) ->
  'a ->
  lexer_state ->
  Lexing.lexbuf -> 'a

val start_any_tuple : lexer_state -> Lexing.lexbuf -> bool
val read_lpar : lexer_state -> Lexing.lexbuf -> unit
val read_rpar : lexer_state -> Lexing.lexbuf -> unit
val read_tuple_end : Lexing.lexbuf -> unit
val read_tuple_end2 : lexer_state -> bool -> Lexing.lexbuf -> unit
val read_tuple_sep : lexer_state -> Lexing.lexbuf -> unit
val read_tuple_sep2 : lexer_state -> bool -> Lexing.lexbuf -> unit
val read_lbr : lexer_state -> Lexing.lexbuf -> unit
val read_rbr : lexer_state -> Lexing.lexbuf -> unit

val read_fields :
  ('acc -> string -> lexer_state -> Lexing.lexbuf -> 'acc) ->
  'acc ->
  lexer_state ->
  Lexing.lexbuf -> 'acc

val read_abstract_fields :
  (lexer_state -> Lexing.lexbuf -> 'key) ->
  ('acc -> 'key -> lexer_state -> Lexing.lexbuf -> 'acc) ->
  'acc ->
  lexer_state ->
  Lexing.lexbuf -> 'acc

val read_lcurl : lexer_state -> Lexing.lexbuf -> unit
val read_object_end : Lexing.lexbuf -> unit
val read_object_sep : lexer_state -> Lexing.lexbuf -> unit
val read_colon : lexer_state -> Lexing.lexbuf -> unit

val read_json : lexer_state -> Lexing.lexbuf -> t
val skip_json : lexer_state -> Lexing.lexbuf -> unit
val buffer_json : lexer_state -> Lexing.lexbuf -> unit

val validate_json : 'path -> t -> 'error option
  (* always returns [None].
     Provided so that atdgen users can write:

       type t <ocaml module="Yojson.Safe"> = abstract
  *)

(* end undocumented section *)
(**/**)
