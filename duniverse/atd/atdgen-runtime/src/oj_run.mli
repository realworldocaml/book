(** OCaml-Json runtime library. *)

exception Error of string

type 'a write = Bi_outbuf.t -> 'a -> unit

val error : string -> _

val write_list : 'a write -> 'a list write
val write_array : 'a write -> 'a array write
val write_float_as_int : float write
val write_assoc_list : 'a write -> 'b write -> ('a * 'b) list write
val write_assoc_array : 'a write -> 'b write -> ('a * 'b) array write
val write_option : 'a write -> 'a option write
val write_std_option : 'a write -> 'a option write
val write_nullable : 'a write -> 'a option write
val write_int8 : char write
val write_int32 : int32 write
val write_int64 : int64 write

type 'a read = Yojson.lexer_state -> Lexing.lexbuf -> 'a

val read_null : unit read
val read_bool : bool read
val read_int : int read
val read_int8 : char read
val read_int32 : int32 read
val read_int64 : int64 read
val read_string : string read
val read_array : 'a read -> 'a array read
val read_assoc_list : 'a read -> 'b read -> ('a * 'b) list read
val read_assoc_array : 'a read -> 'b read -> ('a * 'b) array read
val read_until_field_value : unit read
val read_list : 'a read -> 'a list read
val read_number : float read
val invalid_variant_tag : Yojson.Lexer_state.t -> string -> _

val missing_tuple_fields : Yojson.lexer_state -> int -> int list -> _
val missing_field : Yojson.lexer_state -> string -> _
val missing_fields : Yojson.lexer_state -> int array -> string array -> _

val write_with_adapter :
  (Yojson.Safe.t -> Yojson.Safe.t) -> ('a write) -> ('a write)

val read_with_adapter :
  (Yojson.Safe.t -> Yojson.Safe.t) -> ('a read) -> ('a read)
