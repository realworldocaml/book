(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



(** This stateful module contains the information about exluded toplevel
    declarations. *)


val add : string -> unit
(** Adds a list of comma-separated elements to excluded list. *)

val add_file : string -> unit
(** Adds a filename pattern to the list of files to exclude. *)

val add_from_file : string -> unit
(** Adds exclusions from the passed file to excluded list.

    Raises [Sys_error] if an i/o error occurs, [Exclude.Exception] if
    an error occurs while parsing the file. *)

val contains_value : string -> string -> bool
(** [contains_value file name] tests whether toplevel value with name
    [name] from file [file] is in excluded list. *)

val contains_file : string -> bool
(** [contains_file file] tests whether the entire file with name [name] is in
    the excluded {e files} list. A file is completely excluded (and not
    instrumented) when a list of excluded top-level values is not given for that
    file at all. *)
