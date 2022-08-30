(* Auto-generated from "test3j.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type rec_type = Test3j_t.rec_type = { more: rec_type list }

type unixtime_list = Test3j_t.unixtime_list

type json = Yojson.Safe.t

type tf_variant2 = Test3j_t.tf_variant2

type tf_variant = Test3j_t.tf_variant

type tf_record2 = Test3j_t.tf_record2 = {
  the_value2: tf_variant2;
  etc2: string
}

type tf_record = Test3j_t.tf_record = { the_value: tf_variant; etc: string }

type dyn = Yojson.Safe.t

type t = Test3j_t.t = { foo: int; bar: json; baz: dyn }

type sf_adapted = Test3j_t.sf_adapted

type sample_open_enum = Test3j_t.sample_open_enum

type sample_open_enums = Test3j_t.sample_open_enums

type patch = Test3j_t.patch = {
  patch1: int option option;
  patch2: int option option;
  patch3: int option option
}

type b = Test3j_t.b = { thing: int }

type a = Test3j_t.a = { thing: string; other_thing: bool }

type adapted_f = Test3j_t.adapted_f

type adapted = Test3j_t.adapted

val write_rec_type :
  Buffer.t -> rec_type -> unit
  (** Output a JSON value of type {!type:rec_type}. *)

val string_of_rec_type :
  ?len:int -> rec_type -> string
  (** Serialize a value of type {!type:rec_type}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rec_type :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rec_type
  (** Input JSON data of type {!type:rec_type}. *)

val rec_type_of_string :
  string -> rec_type
  (** Deserialize JSON data of type {!type:rec_type}. *)

val write_unixtime_list :
  Buffer.t -> unixtime_list -> unit
  (** Output a JSON value of type {!type:unixtime_list}. *)

val string_of_unixtime_list :
  ?len:int -> unixtime_list -> string
  (** Serialize a value of type {!type:unixtime_list}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_unixtime_list :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> unixtime_list
  (** Input JSON data of type {!type:unixtime_list}. *)

val unixtime_list_of_string :
  string -> unixtime_list
  (** Deserialize JSON data of type {!type:unixtime_list}. *)

val write_json :
  Buffer.t -> json -> unit
  (** Output a JSON value of type {!type:json}. *)

val string_of_json :
  ?len:int -> json -> string
  (** Serialize a value of type {!type:json}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_json :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> json
  (** Input JSON data of type {!type:json}. *)

val json_of_string :
  string -> json
  (** Deserialize JSON data of type {!type:json}. *)

val write_tf_variant2 :
  Buffer.t -> tf_variant2 -> unit
  (** Output a JSON value of type {!type:tf_variant2}. *)

val string_of_tf_variant2 :
  ?len:int -> tf_variant2 -> string
  (** Serialize a value of type {!type:tf_variant2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tf_variant2 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tf_variant2
  (** Input JSON data of type {!type:tf_variant2}. *)

val tf_variant2_of_string :
  string -> tf_variant2
  (** Deserialize JSON data of type {!type:tf_variant2}. *)

val write_tf_variant :
  Buffer.t -> tf_variant -> unit
  (** Output a JSON value of type {!type:tf_variant}. *)

val string_of_tf_variant :
  ?len:int -> tf_variant -> string
  (** Serialize a value of type {!type:tf_variant}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tf_variant :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tf_variant
  (** Input JSON data of type {!type:tf_variant}. *)

val tf_variant_of_string :
  string -> tf_variant
  (** Deserialize JSON data of type {!type:tf_variant}. *)

val write_tf_record2 :
  Buffer.t -> tf_record2 -> unit
  (** Output a JSON value of type {!type:tf_record2}. *)

val string_of_tf_record2 :
  ?len:int -> tf_record2 -> string
  (** Serialize a value of type {!type:tf_record2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tf_record2 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tf_record2
  (** Input JSON data of type {!type:tf_record2}. *)

val tf_record2_of_string :
  string -> tf_record2
  (** Deserialize JSON data of type {!type:tf_record2}. *)

val write_tf_record :
  Buffer.t -> tf_record -> unit
  (** Output a JSON value of type {!type:tf_record}. *)

val string_of_tf_record :
  ?len:int -> tf_record -> string
  (** Serialize a value of type {!type:tf_record}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tf_record :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tf_record
  (** Input JSON data of type {!type:tf_record}. *)

val tf_record_of_string :
  string -> tf_record
  (** Deserialize JSON data of type {!type:tf_record}. *)

val write_dyn :
  Buffer.t -> dyn -> unit
  (** Output a JSON value of type {!type:dyn}. *)

val string_of_dyn :
  ?len:int -> dyn -> string
  (** Serialize a value of type {!type:dyn}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dyn :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dyn
  (** Input JSON data of type {!type:dyn}. *)

val dyn_of_string :
  string -> dyn
  (** Deserialize JSON data of type {!type:dyn}. *)

val write_t :
  Buffer.t -> t -> unit
  (** Output a JSON value of type {!type:t}. *)

val string_of_t :
  ?len:int -> t -> string
  (** Serialize a value of type {!type:t}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_t :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
  (** Input JSON data of type {!type:t}. *)

val t_of_string :
  string -> t
  (** Deserialize JSON data of type {!type:t}. *)

val write_sf_adapted :
  Buffer.t -> sf_adapted -> unit
  (** Output a JSON value of type {!type:sf_adapted}. *)

val string_of_sf_adapted :
  ?len:int -> sf_adapted -> string
  (** Serialize a value of type {!type:sf_adapted}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sf_adapted :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sf_adapted
  (** Input JSON data of type {!type:sf_adapted}. *)

val sf_adapted_of_string :
  string -> sf_adapted
  (** Deserialize JSON data of type {!type:sf_adapted}. *)

val write_sample_open_enum :
  Buffer.t -> sample_open_enum -> unit
  (** Output a JSON value of type {!type:sample_open_enum}. *)

val string_of_sample_open_enum :
  ?len:int -> sample_open_enum -> string
  (** Serialize a value of type {!type:sample_open_enum}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sample_open_enum :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sample_open_enum
  (** Input JSON data of type {!type:sample_open_enum}. *)

val sample_open_enum_of_string :
  string -> sample_open_enum
  (** Deserialize JSON data of type {!type:sample_open_enum}. *)

val write_sample_open_enums :
  Buffer.t -> sample_open_enums -> unit
  (** Output a JSON value of type {!type:sample_open_enums}. *)

val string_of_sample_open_enums :
  ?len:int -> sample_open_enums -> string
  (** Serialize a value of type {!type:sample_open_enums}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sample_open_enums :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sample_open_enums
  (** Input JSON data of type {!type:sample_open_enums}. *)

val sample_open_enums_of_string :
  string -> sample_open_enums
  (** Deserialize JSON data of type {!type:sample_open_enums}. *)

val write_patch :
  Buffer.t -> patch -> unit
  (** Output a JSON value of type {!type:patch}. *)

val string_of_patch :
  ?len:int -> patch -> string
  (** Serialize a value of type {!type:patch}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_patch :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> patch
  (** Input JSON data of type {!type:patch}. *)

val patch_of_string :
  string -> patch
  (** Deserialize JSON data of type {!type:patch}. *)

val write_b :
  Buffer.t -> b -> unit
  (** Output a JSON value of type {!type:b}. *)

val string_of_b :
  ?len:int -> b -> string
  (** Serialize a value of type {!type:b}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_b :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> b
  (** Input JSON data of type {!type:b}. *)

val b_of_string :
  string -> b
  (** Deserialize JSON data of type {!type:b}. *)

val write_a :
  Buffer.t -> a -> unit
  (** Output a JSON value of type {!type:a}. *)

val string_of_a :
  ?len:int -> a -> string
  (** Serialize a value of type {!type:a}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_a :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> a
  (** Input JSON data of type {!type:a}. *)

val a_of_string :
  string -> a
  (** Deserialize JSON data of type {!type:a}. *)

val write_adapted_f :
  Buffer.t -> adapted_f -> unit
  (** Output a JSON value of type {!type:adapted_f}. *)

val string_of_adapted_f :
  ?len:int -> adapted_f -> string
  (** Serialize a value of type {!type:adapted_f}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_adapted_f :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> adapted_f
  (** Input JSON data of type {!type:adapted_f}. *)

val adapted_f_of_string :
  string -> adapted_f
  (** Deserialize JSON data of type {!type:adapted_f}. *)

val write_adapted :
  Buffer.t -> adapted -> unit
  (** Output a JSON value of type {!type:adapted}. *)

val string_of_adapted :
  ?len:int -> adapted -> string
  (** Serialize a value of type {!type:adapted}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_adapted :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> adapted
  (** Input JSON data of type {!type:adapted}. *)

val adapted_of_string :
  string -> adapted
  (** Deserialize JSON data of type {!type:adapted}. *)

