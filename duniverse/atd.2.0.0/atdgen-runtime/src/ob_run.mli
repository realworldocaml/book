(** OCaml-Biniou runtime library. *)

[@@@ocaml.warning "-32"]

exception Error of string


val missing_fields : int array -> string array -> 'a
val missing_tuple_fields : int -> int list -> 'a


val write_untagged_option
  : (Bi_outbuf.t -> 'a -> unit)
  -> Bi_outbuf.t
  -> 'a option
  -> unit

val write_untagged_list
  : Bi_io.node_tag
  -> (Bi_outbuf.t -> 'a -> unit)
  -> Bi_outbuf.t
  -> 'a list
  -> unit

val write_untagged_array
  : Bi_io.node_tag
  -> (Bi_outbuf.t -> 'a -> unit)
  -> Bi_outbuf.t
  -> 'a array
  -> unit

val get_list_reader
  : (Bi_io.node_tag -> Bi_inbuf.t -> 'a)
  -> Bi_io.node_tag
  -> Bi_inbuf.t
  -> 'a list

val get_array_reader
  : (Bi_io.node_tag -> Bi_inbuf.t -> 'a)
  -> Bi_io.node_tag
  -> Bi_inbuf.t
  -> 'a array

val read_list : (Bi_io.node_tag -> Bi_inbuf.t -> 'a) -> Bi_inbuf.t -> 'a list

val unsupported_variant : int -> bool -> 'a

val read_error_at : Bi_inbuf.t -> 'a


val read_int : Bi_inbuf.t -> int
val read_char : Bi_inbuf.t -> char
val read_bool : Bi_inbuf.t -> bool
val read_string : Bi_inbuf.t -> string
val read_float32 : Bi_inbuf.t -> float
val read_float64 : Bi_inbuf.t -> float
val read_int64 : Bi_inbuf.t -> int64
val read_int32 : Bi_inbuf.t -> int32
val read_array : (Bi_io.node_tag -> Bi_inbuf.t -> 'a) -> Bi_inbuf.t -> 'a array
val read_array_value
  : (Bi_io.node_tag -> Bi_inbuf.t -> 'a)
  -> Bi_inbuf.t
  -> 'a array

val get_unit_reader : Bi_io.node_tag -> Bi_inbuf.t -> unit
val get_char_reader : Bi_io.node_tag -> Bi_inbuf.t -> char
val get_bool_reader : Bi_io.node_tag -> Bi_inbuf.t -> bool
val get_string_reader : Bi_io.node_tag -> Bi_inbuf.t -> string
val get_float64_reader : Bi_io.node_tag -> Bi_inbuf.t -> float
val get_float32_reader : Bi_io.node_tag -> Bi_inbuf.t -> float
val get_int64_reader : Bi_io.node_tag -> Bi_inbuf.t -> int64
val get_int32_reader : Bi_io.node_tag -> Bi_inbuf.t -> int32
val get_int_reader : Bi_io.node_tag -> Bi_inbuf.t -> int

val read_error : unit -> 'a


val array_iter2 : ('a -> 'b -> unit) -> 'a -> 'b array -> unit
val array_init2 : int -> 'a -> (int -> 'a -> 'b) -> 'b array
