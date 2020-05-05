(* Auto-generated from "test_annot.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type pointC = ProtoC_t.pointC = { f: float }

type pointB = ProtoD_t.pointB = { f: float }

type pointA = ProtoA_t.pointA = { f: float }

val write_pointC :
  Bi_outbuf.t -> pointC -> unit
  (** Output a JSON value of type {!pointC}. *)

val string_of_pointC :
  ?len:int -> pointC -> string
  (** Serialize a value of type {!pointC}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pointC :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pointC
  (** Input JSON data of type {!pointC}. *)

val pointC_of_string :
  string -> pointC
  (** Deserialize JSON data of type {!pointC}. *)

val write_pointB :
  Bi_outbuf.t -> pointB -> unit
  (** Output a JSON value of type {!pointB}. *)

val string_of_pointB :
  ?len:int -> pointB -> string
  (** Serialize a value of type {!pointB}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pointB :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pointB
  (** Input JSON data of type {!pointB}. *)

val pointB_of_string :
  string -> pointB
  (** Deserialize JSON data of type {!pointB}. *)

val write_pointA :
  Bi_outbuf.t -> pointA -> unit
  (** Output a JSON value of type {!pointA}. *)

val string_of_pointA :
  ?len:int -> pointA -> string
  (** Serialize a value of type {!pointA}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pointA :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pointA
  (** Input JSON data of type {!pointA}. *)

val pointA_of_string :
  string -> pointA
  (** Deserialize JSON data of type {!pointA}. *)

