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

module Chunk = Ocaml_topexpect.Chunk
module Part = Ocaml_topexpect.Part

module Mlt : sig
  include (module type of Ocaml_topexpect.Document)
  val of_file: filename:string -> t Deferred.Or_error.t
end

module Cram: sig
  type t [@@deriving sexp]
  val to_html: t -> string
  val part: string -> t -> t option
  val of_file: filename:string -> t Deferred.Or_error.t
end
