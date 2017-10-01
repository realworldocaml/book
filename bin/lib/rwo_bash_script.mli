(** Bash script parsing and evaluation. *)
open Core
open Async

type t = {
  filename : string;
  commands : string list;
} [@@ deriving sexp]
(** A script is a sequence of commands. *)

val of_file : string -> t Deferred.t

module Evaluated : sig

  type command = {
    command : string;
    output : string; (** merged stdout/stderr *)
    exit_code : int;
  } [@@deriving sexp]

  type t = {
    filename : string;
    commands : command list;
  } [@@deriving sexp]

  val to_string : t -> string

  val check_all_zero : t -> t Or_error.t
  (** If all commands in [t] returned with exit code equal to 0, then
      return [Ok t]. Else return Error. *)

end

val eval : t -> Evaluated.t Or_error.t Deferred.t
val eval_file : string -> Evaluated.t Or_error.t Deferred.t
