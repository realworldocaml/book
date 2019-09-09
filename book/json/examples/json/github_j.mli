(* Auto-generated from "github.atd" *)


type scope = Github_t.scope

type app = Github_t.app = {
  app_name (*atd name *): string;
  app_url (*atd url *): string
}

type authorization_response = Github_t.authorization_response = {
  scopes: scope list;
  token: string;
  app: app;
  url: string;
  id: int;
  note: string option;
  note_url: string option
}

type authorization_request = Github_t.authorization_request = {
  auth_req_scopes (*atd scopes *): scope list;
  auth_req_note (*atd note *): string
}

val write_scope :
  Bi_outbuf.t -> scope -> unit
  (** Output a JSON value of type {!scope}. *)

val string_of_scope :
  ?len:int -> scope -> string
  (** Serialize a value of type {!scope}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scope :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scope
  (** Input JSON data of type {!scope}. *)

val scope_of_string :
  string -> scope
  (** Deserialize JSON data of type {!scope}. *)

val write_app :
  Bi_outbuf.t -> app -> unit
  (** Output a JSON value of type {!app}. *)

val string_of_app :
  ?len:int -> app -> string
  (** Serialize a value of type {!app}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_app :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> app
  (** Input JSON data of type {!app}. *)

val app_of_string :
  string -> app
  (** Deserialize JSON data of type {!app}. *)

val write_authorization_response :
  Bi_outbuf.t -> authorization_response -> unit
  (** Output a JSON value of type {!authorization_response}. *)

val string_of_authorization_response :
  ?len:int -> authorization_response -> string
  (** Serialize a value of type {!authorization_response}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_authorization_response :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> authorization_response
  (** Input JSON data of type {!authorization_response}. *)

val authorization_response_of_string :
  string -> authorization_response
  (** Deserialize JSON data of type {!authorization_response}. *)

val write_authorization_request :
  Bi_outbuf.t -> authorization_request -> unit
  (** Output a JSON value of type {!authorization_request}. *)

val string_of_authorization_request :
  ?len:int -> authorization_request -> string
  (** Serialize a value of type {!authorization_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_authorization_request :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> authorization_request
  (** Input JSON data of type {!authorization_request}. *)

val authorization_request_of_string :
  string -> authorization_request
  (** Deserialize JSON data of type {!authorization_request}. *)

