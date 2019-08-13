(* Auto-generated from "test2.atd" *)
[@@@ocaml.warning "-27-32-35-39"]
open Test
open Test2
open Testj

val write_poly :
  (Bi_outbuf.t -> 'aa -> unit) ->
  (Bi_outbuf.t -> 'bb -> unit) ->
  Bi_outbuf.t -> ('aa, 'bb) poly -> unit
  (** Output a JSON value of type {!poly}. *)

val string_of_poly :
  (Bi_outbuf.t -> 'aa -> unit) ->
  (Bi_outbuf.t -> 'bb -> unit) ->
  ?len:int -> ('aa, 'bb) poly -> string
  (** Serialize a value of type {!poly}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_poly :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'aa) ->
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'bb) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ('aa, 'bb) poly
  (** Input JSON data of type {!poly}. *)

val poly_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'aa) ->
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'bb) ->
  string -> ('aa, 'bb) poly
  (** Deserialize JSON data of type {!poly}. *)


val write_poly_int2 :
  Bi_outbuf.t -> poly_int2 -> unit
  (** Output a JSON value of type {!poly_int2}. *)

val string_of_poly_int2 :
  ?len:int -> poly_int2 -> string
  (** Serialize a value of type {!poly_int2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_poly_int2 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> poly_int2
  (** Input JSON data of type {!poly_int2}. *)

val poly_int2_of_string :
  string -> poly_int2
  (** Deserialize JSON data of type {!poly_int2}. *)


val write_test2 :
  Bi_outbuf.t -> test2 -> unit
  (** Output a JSON value of type {!test2}. *)

val string_of_test2 :
  ?len:int -> test2 -> string
  (** Serialize a value of type {!test2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_test2 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> test2
  (** Input JSON data of type {!test2}. *)

val test2_of_string :
  string -> test2
  (** Deserialize JSON data of type {!test2}. *)

val create_test2 :
  test0: poly_int2 ->
  test1: (int, string option) poly ->
  unit -> test2
  (** Create a record of type {!test2}. *)


val write_poly_int_string :
  Bi_outbuf.t -> poly_int_string -> unit
  (** Output a JSON value of type {!poly_int_string}. *)

val string_of_poly_int_string :
  ?len:int -> poly_int_string -> string
  (** Serialize a value of type {!poly_int_string}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_poly_int_string :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> poly_int_string
  (** Input JSON data of type {!poly_int_string}. *)

val poly_int_string_of_string :
  string -> poly_int_string
  (** Deserialize JSON data of type {!poly_int_string}. *)


