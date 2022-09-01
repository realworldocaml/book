val version : string

exception Json_error of string
(** Exception used:
    - in JSON readers, if parsing fails;
    - in JSON writers and pretty printing, if [float] value is not allowed in standard JSON. *)

val json_error : string -> 'a
(** @raise Json_error *)

type lexer_state = {
  buf : Buffer.t;
    (** Buffer used to accumulate substrings *)

  mutable lnum : int;
    (** Current line number (counting from 1) *)

  mutable bol : int;
    (** Absolute position of the first character of the current line
        (counting from 0) *)

  mutable fname : string option;
    (** Name referencing the input file in error messages *)
}

module Lexer_state :
sig
  type t = lexer_state = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }
end

val init_lexer :
  ?buf: Buffer.t ->
  ?fname: string ->
  ?lnum: int ->
  unit -> lexer_state
  (** Create a fresh lexer_state record. *)


(**/**)
(* begin undocumented section *)

exception End_of_array
exception End_of_object
exception End_of_tuple
exception End_of_input

(* end undocumented section *)
(**/**)
