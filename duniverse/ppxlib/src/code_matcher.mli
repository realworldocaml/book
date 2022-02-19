(** Match source code against generated code *)

open! Import

val match_structure :
  pos:Lexing.position ->
  expected:structure ->
  mismatch_handler:(Location.t -> structure -> unit) ->
  structure ->
  unit
(** Checks that the given code starts with [expected] followed by
    [@@@deriving.end] or [@@@end].

    Raises if there is no [@@@deriving.end].

    If some items don't match, it calls [mismatch_handler] with the location of
    the source items and the expected code. *)

val match_signature :
  pos:Lexing.position ->
  expected:signature ->
  mismatch_handler:(Location.t -> signature -> unit) ->
  signature ->
  unit
(** Same for signatures *)
