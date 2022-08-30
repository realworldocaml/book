(* Auto-generated from "test_abstract.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type int_assoc_list = Testj.int_assoc_list

type any_items = Test_abstract_t.any_items

type any = Test_abstract_t.any

type 'x abs2 = 'x Testj.abs2

type 'x abs1 = 'x Testj.abs1

val write_int_assoc_list :
  Buffer.t -> int_assoc_list -> unit
  (** Output a JSON value of type {!type:int_assoc_list}. *)

val string_of_int_assoc_list :
  ?len:int -> int_assoc_list -> string
  (** Serialize a value of type {!type:int_assoc_list}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_int_assoc_list :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> int_assoc_list
  (** Input JSON data of type {!type:int_assoc_list}. *)

val int_assoc_list_of_string :
  string -> int_assoc_list
  (** Deserialize JSON data of type {!type:int_assoc_list}. *)

val write_any_items :
  Buffer.t -> any_items -> unit
  (** Output a JSON value of type {!type:any_items}. *)

val string_of_any_items :
  ?len:int -> any_items -> string
  (** Serialize a value of type {!type:any_items}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_any_items :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> any_items
  (** Input JSON data of type {!type:any_items}. *)

val any_items_of_string :
  string -> any_items
  (** Deserialize JSON data of type {!type:any_items}. *)

val write_abs2 :
  (Buffer.t -> 'x -> unit) ->
  Buffer.t -> 'x abs2 -> unit
  (** Output a JSON value of type {!type:abs2}. *)

val string_of_abs2 :
  (Buffer.t -> 'x -> unit) ->
  ?len:int -> 'x abs2 -> string
  (** Serialize a value of type {!type:abs2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_abs2 :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'x) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'x abs2
  (** Input JSON data of type {!type:abs2}. *)

val abs2_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'x) ->
  string -> 'x abs2
  (** Deserialize JSON data of type {!type:abs2}. *)

val write_abs1 :
  (Buffer.t -> 'x -> unit) ->
  Buffer.t -> 'x abs1 -> unit
  (** Output a JSON value of type {!type:abs1}. *)

val string_of_abs1 :
  (Buffer.t -> 'x -> unit) ->
  ?len:int -> 'x abs1 -> string
  (** Serialize a value of type {!type:abs1}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_abs1 :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'x) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'x abs1
  (** Input JSON data of type {!type:abs1}. *)

val abs1_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'x) ->
  string -> 'x abs1
  (** Deserialize JSON data of type {!type:abs1}. *)

