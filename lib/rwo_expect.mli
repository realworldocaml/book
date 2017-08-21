open! Core
open Async

module Raw_script : sig
  type part =
    { name : string; content : string; }
    [@@deriving sexp]

  type t = part list
    [@@deriving sexp]

  val of_file : filename:string -> t Deferred.Or_error.t
end

module Chunk : sig
  type kind = OCaml | Raw
    [@@deriving sexp]

  type response = (kind * string)
    [@@deriving sexp]

  type t =
    { ocaml_code : string; toplevel_responses : response list; }
    [@@deriving sexp]

  val code      : t -> string
  val warnings  : t -> string
  val responses : t -> response list
  val stdout    : t -> string
  val evaluated : t -> bool
end

module Part : sig
  type t =
    { name : string; chunks : Chunk.t list; }
    [@@deriving sexp]
end

module Document : sig
  type t =
    { parts : Part.t list; matched : bool; }
    [@@deriving sexp]

  val parts : t -> Part.t list
  val of_file : run_nondeterministic:bool -> filename:string -> t Deferred.Or_error.t
end
