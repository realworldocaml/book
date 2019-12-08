open! Core
open Async

type part = {
  title   : string;
  chapters: string list;
} [@@deriving sexp]

type t = [ `part of part | `chapter of string] list [@@deriving sexp]

val get : ?repo_root:string -> unit -> t Deferred.t
