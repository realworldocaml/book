(** Miscellaneous utilities. *)
open Core.Std
open Async.Std

(** Return all files in the given directory, including files in its
    sub-directories. *)
val find_files : string -> string list Deferred.t
