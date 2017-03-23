open Core
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
  type t = Toplevel_expect_test_types.Chunk.t =
    { ocaml_code : string; toplevel_response : string; }
    [@@deriving sexp]
  val code      : t -> string
  val warnings  : t -> string
  val response  : t -> string
  val stdout    : t -> string
  val evaluated : t -> bool
end

module Part : sig
  type t = Toplevel_expect_test_types.Part.t =
    { name : string; chunks : Chunk.t list; }
    [@@deriving sexp]
end

module Document : sig
  type t = Toplevel_expect_test_types.Document.t =
    { parts : Part.t list; matched : bool; }
    [@@deriving sexp]

  val parts : t -> Part.t list
  val of_file : filename:string -> t Deferred.Or_error.t
  val output_corrected : t -> Out_channel.t -> unit
end

