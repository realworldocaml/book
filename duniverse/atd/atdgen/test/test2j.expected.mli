(* Auto-generated from "test2.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]
open Test
open Test2
open Testj

val write_poly :
  (Buffer.t -> 'aa -> unit) ->
  (Buffer.t -> 'bb -> unit) ->
  Buffer.t -> ('aa, 'bb) poly -> unit
  (** Output a JSON value of type {!type:poly}. *)

val string_of_poly :
  (Buffer.t -> 'aa -> unit) ->
  (Buffer.t -> 'bb -> unit) ->
  ?len:int -> ('aa, 'bb) poly -> string
  (** Serialize a value of type {!type:poly}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_poly :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'aa) ->
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'bb) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ('aa, 'bb) poly
  (** Input JSON data of type {!type:poly}. *)

val poly_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'aa) ->
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'bb) ->
  string -> ('aa, 'bb) poly
  (** Deserialize JSON data of type {!type:poly}. *)


val write_poly_int2 :
  Buffer.t -> poly_int2 -> unit
  (** Output a JSON value of type {!type:poly_int2}. *)

val string_of_poly_int2 :
  ?len:int -> poly_int2 -> string
  (** Serialize a value of type {!type:poly_int2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_poly_int2 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> poly_int2
  (** Input JSON data of type {!type:poly_int2}. *)

val poly_int2_of_string :
  string -> poly_int2
  (** Deserialize JSON data of type {!type:poly_int2}. *)


val write_test2 :
  Buffer.t -> test2 -> unit
  (** Output a JSON value of type {!type:test2}. *)

val string_of_test2 :
  ?len:int -> test2 -> string
  (** Serialize a value of type {!type:test2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_test2 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> test2
  (** Input JSON data of type {!type:test2}. *)

val test2_of_string :
  string -> test2
  (** Deserialize JSON data of type {!type:test2}. *)

val create_test2 :
  test0: poly_int2 ->
  test1: (int, string option) poly ->
  unit -> test2
  (** Create a record of type {!type:test2}. *)


val write_poly_int_string :
  Buffer.t -> poly_int_string -> unit
  (** Output a JSON value of type {!type:poly_int_string}. *)

val string_of_poly_int_string :
  ?len:int -> poly_int_string -> string
  (** Serialize a value of type {!type:poly_int_string}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_poly_int_string :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> poly_int_string
  (** Input JSON data of type {!type:poly_int_string}. *)

val poly_int_string_of_string :
  string -> poly_int_string
  (** Deserialize JSON data of type {!type:poly_int_string}. *)


