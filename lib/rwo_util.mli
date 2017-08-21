(** Miscellaneous utilities. *)
open! Core
open Async

(** Return all files in the given directory, including files in its
    sub-directories. *)
val find_files : string -> string list Deferred.t

val string_pair_equal : (string * string) -> (string * string) -> bool
